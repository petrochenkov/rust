use crate::{errors, FnCtxt, Inherited};
use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::intravisit::{self, FnKind, Visitor};
use rustc_hir::{BodyId, Expr, ExprKind, FnDecl, Param, Path, QPath, Stmt, StmtKind, CRATE_HIR_ID};
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::query::Providers;
use rustc_middle::traits::{DefiningAnchor, ObligationCause};
use rustc_middle::ty::{FnDef, ParamEnv, Ty, TyCtxt, TypeckResults};
use rustc_session::lint;
use rustc_span::def_id::{DefId, LocalDefId};
use rustc_span::{ExpnKind, MacroKind, Span, Symbol};
use rustc_trait_selection::traits::ObligationCtxt;
use std::collections::BTreeMap;
use std::{fmt, iter};

#[derive(Default, Debug, Copy, Clone)]
enum ArgsMatch {
    SameCount(TyMatch),
    #[default]
    DifferentCount,
}

impl fmt::Display for ArgsMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ArgsMatch::SameCount(TyMatch::Same) => "Same",
            ArgsMatch::SameCount(TyMatch::SameUpToSelfType) => "SameUpToSelfType",
            ArgsMatch::SameCount(TyMatch::Coerced) => "Coerced",
            ArgsMatch::SameCount(TyMatch::Different) => "SameCount",
            ArgsMatch::DifferentCount => "DifferentCount",
        })
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, PartialOrd)]
enum ArgPreproc {
    No,
    Field,
    Getter,
    #[default]
    Other,
}

impl fmt::Display for ArgPreproc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ArgPreproc::No => "No",
            ArgPreproc::Field => "Field",
            ArgPreproc::Getter => "Getter",
            ArgPreproc::Other => "Other",
        })
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, PartialOrd)]
enum TyMatch {
    Same,
    SameUpToSelfType,
    Coerced,
    #[default]
    Different,
}

impl fmt::Display for TyMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TyMatch::Same => "Same",
            TyMatch::SameUpToSelfType => "SameUpToSelfType",
            TyMatch::Coerced => "Coerced",
            TyMatch::Different => "Different",
        })
    }
}

#[derive(Debug, Copy, Clone)]
enum Parent {
    InherentImpl,
    TraitImpl,
    Trait,
    Other,
}

impl fmt::Display for Parent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Parent::InherentImpl => "InherentImpl",
            Parent::TraitImpl => "TraitImpl",
            Parent::Trait => "Trait",
            Parent::Other => "Other",
        })
    }
}

#[derive(Debug, Copy, Clone)]
enum Source {
    User,
    Lang,
    Bang,
    Attr,
    Derive,
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Source::User => "User",
            Source::Lang => "Lang",
            Source::Bang => "Bang",
            Source::Attr => "Attr",
            Source::Derive => "Derive",
        })
    }
}

#[derive(Debug, Clone)]
struct Callee {
    name: Symbol,
    span: Span,
    ret_postproc: bool,
    ret_match: TyMatch,
    stmts_before: bool,
    arg0_preproc: ArgPreproc,
    arg0_match: TyMatch,
    args_preproc: ArgPreproc,
    args_match: ArgsMatch,
    has_self: bool,
    source: Source,
}

fn has_self(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    tcx.def_kind(def_id) == DefKind::AssocFn && tcx.associated_item(def_id).fn_has_self_parameter
}

// FIXME: `Self` type is approximated as `typeof(self)` with references removed,
// with a fallback to parent's `type_of`, that's probably not a good solution.
fn self_ty<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    arg_tys: &[Ty<'tcx>],
    has_self: bool,
) -> Option<Ty<'tcx>> {
    has_self.then(|| arg_tys[0].peel_refs()).or_else(|| match tcx.def_kind(def_id) {
        DefKind::Impl { .. } | DefKind::Trait => {
            Some(tcx.type_of(tcx.parent(def_id)).instantiate_identity())
        }
        _ => None,
    })
}

fn refers_to_param(expr: &Expr<'_>, param: &Param<'_>) -> bool {
    if let ExprKind::Path(QPath::Resolved(None, path)) = expr.kind
        && let Path { res: Res::Local(arg_res_id), .. } = path
        && *arg_res_id == param.pat.hir_id
    {
        true
    } else {
        false
    }
}

fn arg_preproc(arg: &Expr<'_>, param: &Param<'_>) -> ArgPreproc {
    if refers_to_param(arg, param) {
        ArgPreproc::No
    } else if let ExprKind::Field(receiver, _) = arg.kind
        && refers_to_param(receiver, param)
    {
        ArgPreproc::Field
    } else if let ExprKind::MethodCall(_, receiver, args, _) = arg.kind
        && args.is_empty()
        && refers_to_param(receiver, param)
    {
        ArgPreproc::Getter
    } else {
        ArgPreproc::Other
    }
}

fn source(span: Span) -> Source {
    let expn_data = span.ctxt().outer_expn_data();
    match expn_data.kind {
        ExpnKind::Root => Source::User,
        ExpnKind::AstPass(_) | ExpnKind::Desugaring(_) => Source::Lang,
        ExpnKind::Macro(MacroKind::Bang, _) => Source::Bang,
        ExpnKind::Macro(MacroKind::Attr, _) => Source::Attr,
        ExpnKind::Macro(MacroKind::Derive, _) => Source::Derive,
    }
}

struct ExprVisitor<'c, 'tcx> {
    caller: &'c Caller<'tcx>,
    ret_postproc: bool,
    stmts_before: bool,
    callees: Vec<Callee>,
}

impl<'c, 'tcx> ExprVisitor<'c, 'tcx> {
    fn new(caller: &'c Caller<'tcx>, ret_postproc: bool, stmts_before: bool) -> Self {
        ExprVisitor { caller, ret_postproc, stmts_before, callees: Default::default() }
    }

    fn ret_match(&self, ret_ty: Ty<'tcx>, self_ty: Option<Ty<'tcx>>) -> TyMatch {
        let caller = self.caller;
        caller.compare_ty(ret_ty, caller.ret_ty, self_ty, caller.self_ty)
    }

    fn arg0_match(&self, arg_tys: &[Ty<'tcx>], self_ty: Option<Ty<'tcx>>) -> TyMatch {
        let caller = self.caller;
        match (arg_tys.first(), caller.param_tys.first()) {
            (Some(&arg0_ty), Some(&param0_ty)) => {
                caller.compare_ty(param0_ty, arg0_ty, self_ty, caller.self_ty)
            }
            (None, None) => TyMatch::Same,
            _ => TyMatch::Different,
        }
    }

    fn arg0_preproc(&self, args: &[&Expr<'_>]) -> ArgPreproc {
        let caller = self.caller;
        match (args.first(), caller.params.first()) {
            (Some(arg0), Some(param0)) => arg_preproc(arg0, param0),
            (None, None) => ArgPreproc::No,
            _ => ArgPreproc::Other,
        }
    }

    fn args_preproc(&self, args: &[&Expr<'_>]) -> ArgPreproc {
        let caller = self.caller;
        if args.len() != caller.params.len() {
            return ArgPreproc::Other;
        }

        // First argument can be transformed (`arg0` -> `target_expr(arg0)`),
        // so don't include it into the comparison.
        let mut max_arg_preproc = ArgPreproc::No;
        for (arg, param) in iter::zip(args, caller.params.iter()).skip(1) {
            let arg_preproc = arg_preproc(arg, param);
            if arg_preproc > max_arg_preproc {
                max_arg_preproc = arg_preproc;
            }
        }

        max_arg_preproc
    }

    fn args_match(&self, arg_tys: &[Ty<'tcx>], self_ty: Option<Ty<'tcx>>) -> ArgsMatch {
        let caller = self.caller;
        if arg_tys.len() != caller.param_tys.len() {
            return ArgsMatch::DifferentCount;
        }

        // First argument can be transformed (`arg0` -> `target_expr(arg0)`),
        // so don't include it into the comparison.
        let mut max_ty_match = TyMatch::Same;
        for (&callee_param, &caller_param) in iter::zip(arg_tys, caller.param_tys).skip(1) {
            let ty_match = caller.compare_ty(caller_param, callee_param, caller.self_ty, self_ty);
            if ty_match > max_ty_match {
                max_ty_match = ty_match;
            }
        }

        ArgsMatch::SameCount(max_ty_match)
    }
}

impl<'tcx> Visitor<'tcx> for ExprVisitor<'_, 'tcx> {
    fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
        let caller = self.caller;
        let typeck_results = caller.typeck_results;

        let callee = match expr.kind {
            ExprKind::MethodCall(seg, self_arg, other_args, _) => {
                let args: Vec<_> = iter::once(self_arg).chain(other_args).collect();
                Some((seg.ident.name, true, args, caller.def_id.to_def_id() /*never used*/))
            }
            ExprKind::Call(func, args)
                if let FnDef(def_id, ..) = typeck_results.expr_ty(func).kind() =>
            {
                let args: Vec<_> = args.iter().collect();
                Some((caller.tcx.item_name(*def_id), has_self(caller.tcx, *def_id), args, *def_id))
            }
            _ => None,
        };

        if let Some((name, has_self, args, def_id)) = callee {
            let arg_tys: Vec<_> =
                args.iter().map(|arg| typeck_results.expr_ty_adjusted(arg)).collect();
            let self_ty = self_ty(caller.tcx, def_id, &arg_tys, has_self);

            self.callees.push(Callee {
                name,
                span: expr.span,
                ret_postproc: self.ret_postproc,
                ret_match: self.ret_match(typeck_results.expr_ty(expr), self_ty),
                stmts_before: self.stmts_before,
                arg0_preproc: self.arg0_preproc(&args),
                arg0_match: self.arg0_match(&arg_tys, self_ty),
                args_preproc: self.args_preproc(&args),
                args_match: self.args_match(&arg_tys, self_ty),
                has_self,
                source: source(expr.span),
            });
        }

        self.ret_postproc = true;
        intravisit::walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, s: &'tcx Stmt<'tcx>) {
        intravisit::walk_stmt(self, s);
        self.stmts_before = true;
    }
}

struct Caller<'tcx> {
    tcx: TyCtxt<'tcx>,
    def_id: LocalDefId,
    typeck_results: &'tcx TypeckResults<'tcx>,
    param_env: ParamEnv<'tcx>,
    name: Symbol,
    span: Span,
    parent: Parent,
    self_ty: Option<Ty<'tcx>>,
    ret_ty: Ty<'tcx>,
    params: &'tcx [Param<'tcx>],
    param_tys: &'tcx [Ty<'tcx>],
    has_self: bool,
}

impl<'tcx> Caller<'tcx> {
    fn same_ty(&self, src: Ty<'tcx>, dst: Ty<'tcx>) -> bool {
        if src == dst {
            return true;
        }

        let mut builder = self
            .tcx
            .infer_ctxt()
            .ignoring_regions()
            .with_opaque_type_inference(DefiningAnchor::Bubble);
        let infcx = builder.build();
        let ocx = ObligationCtxt::new(&infcx);
        let cause = ObligationCause::dummy();
        let src = ocx.normalize(&cause, self.param_env, src);
        let dst = ocx.normalize(&cause, self.param_env, dst);
        let res = match ocx.sub(&cause, self.param_env, src, dst) {
            Ok(()) => ocx.select_all_or_error().is_empty(),
            Err(_) => false,
        };
        let _ = infcx.take_opaque_types();
        res
    }

    fn compare_ty(
        &self,
        src: Ty<'tcx>,
        dst: Ty<'tcx>,
        src_self: Option<Ty<'tcx>>,
        dst_self: Option<Ty<'tcx>>,
    ) -> TyMatch {
        if self.same_ty(src, dst) {
            return TyMatch::Same;
        }

        // References like `&Self` or similar are also plausible.
        if let Some(src_self) = src_self
            && let Some(dst_self) = dst_self
            && self.same_ty(src.peel_refs(), src_self)
            && self.same_ty(dst.peel_refs(), dst_self)
        {
            return TyMatch::SameUpToSelfType;
        }

        let inh = Inherited::new(self.tcx, self.def_id);
        let fcx = FnCtxt::new(&inh, self.param_env, self.def_id);
        if fcx.can_coerce(src, dst) {
            return TyMatch::Coerced;
        }

        TyMatch::Different
    }

    fn try_emit(&self, callee: &Callee) {
        let tcx = self.tcx;
        let hir_id = tcx.hir().local_def_id_to_hir_id(self.def_id);
        let lint = errors::Delegation {
            caller_span: self.span,
            caller_parent: self.parent.to_string(),
            caller_has_self: self.has_self,
            same_name: callee.name == self.name,
            span: callee.span,
            ret_postproc: callee.ret_postproc,
            ret_match: callee.ret_match.to_string(),
            stmts_before: callee.stmts_before,
            arg0_preproc: callee.arg0_preproc.to_string(),
            arg0_match: callee.arg0_match.to_string(),
            args_preproc: callee.args_preproc.to_string(),
            args_match: callee.args_match.to_string(),
            has_self: callee.has_self,
            source: callee.source.to_string(),
        };
        tcx.emit_spanned_lint(
            lint::builtin::DELEGATIONS_DETAILED,
            hir_id,
            callee.span,
            lint.clone(),
        );
        if self.is_delegation(callee) {
            tcx.emit_spanned_lint(lint::builtin::DELEGATIONS, hir_id, callee.span, lint);
        }
    }

    fn is_delegation(&self, callee: &Callee) -> bool {
        let sig_known = matches!(self.parent, Parent::TraitImpl);
        let arg0_sig_known = self.has_self;

        let ret_ok = match callee.ret_match {
            TyMatch::Same => !callee.ret_postproc,
            TyMatch::SameUpToSelfType => true,
            TyMatch::Coerced => !callee.ret_postproc && sig_known,
            TyMatch::Different => false,
        };
        let stmts_before_ok = !callee.stmts_before
            || matches!(callee.arg0_preproc, ArgPreproc::Other)
            || matches!(callee.args_preproc, ArgPreproc::Other);
        let arg0_ok = match callee.arg0_match {
            TyMatch::Same | TyMatch::SameUpToSelfType => true,
            TyMatch::Coerced | TyMatch::Different => arg0_sig_known,
        };
        let args_ok = match callee.args_match {
            ArgsMatch::SameCount(TyMatch::Same) => matches!(callee.args_preproc, ArgPreproc::No),
            ArgsMatch::SameCount(TyMatch::SameUpToSelfType) => true,
            ArgsMatch::SameCount(TyMatch::Coerced) => {
                matches!(callee.args_preproc, ArgPreproc::No) && sig_known
            }
            ArgsMatch::SameCount(TyMatch::Different) | ArgsMatch::DifferentCount => false,
        };
        let has_self_ok = callee.has_self == self.has_self || matches!(self.parent, Parent::Other);
        let source_ok = !matches!(callee.source, Source::Lang | Source::Derive);

        ret_ok && stmts_before_ok && arg0_ok && args_ok && has_self_ok && source_ok
    }
}

struct DelegationPatternVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    delegations_per_parent_stats: FxIndexMap<LocalDefId, u64>,
}

impl<'tcx> DelegationPatternVisitor<'tcx> {
    fn new(tcx: TyCtxt<'tcx>) -> Self {
        DelegationPatternVisitor { tcx, delegations_per_parent_stats: Default::default() }
    }

    fn emit_methods_stats(&self) {
        let mut accumulated = BTreeMap::default();
        for (_, &delegation_count) in &self.delegations_per_parent_stats {
            *accumulated.entry(delegation_count).or_default() += 1;
        }

        for (delegation_count, parent_count) in accumulated {
            self.tcx.emit_lint(
                lint::builtin::DELEGATIONS_PER_PARENT_STATS,
                CRATE_HIR_ID,
                errors::DelegationsPerParentStats { delegation_count, parent_count },
            );
        }
    }
}

impl<'tcx> Visitor<'tcx> for DelegationPatternVisitor<'tcx> {
    fn visit_fn(
        &mut self,
        fk: FnKind<'tcx>,
        _decl: &'tcx FnDecl<'tcx>,
        body_id: BodyId,
        span: Span,
        def_id: LocalDefId,
    ) {
        if matches!(fk, FnKind::Closure) {
            return;
        }
        let tcx = self.tcx;
        let body = tcx.hir().body(body_id);
        let block = match body.value.kind {
            ExprKind::Block(block, _) => block,
            // Async functions are lowered to this, consider later.
            ExprKind::Closure(..) => return,
            kind => span_bug!(span, "non-block function body: {kind:?}"),
        };

        // Caller signature
        let params = body.params;
        let sig = tcx.liberate_late_bound_regions(
            def_id.to_def_id(),
            tcx.fn_sig(def_id.to_def_id()).instantiate_identity(),
        );
        let param_tys = sig.inputs();
        assert_eq!(params.len(), param_tys.len());

        let has_self = has_self(tcx, def_id.to_def_id());
        let self_ty = self_ty(tcx, def_id.to_def_id(), param_tys, has_self);

        // Combine caller data
        let parent = match tcx.def_kind(tcx.local_parent(def_id)) {
            DefKind::Impl { of_trait: false } => Parent::InherentImpl,
            DefKind::Impl { of_trait: true } => Parent::TraitImpl,
            DefKind::Trait => Parent::Trait,
            _ => Parent::Other,
        };
        let caller = Caller {
            tcx,
            def_id,
            typeck_results: tcx.typeck_body(body_id),
            param_env: tcx.param_env(def_id),
            name: tcx.item_name(def_id.to_def_id()),
            span,
            parent,
            self_ty,
            ret_ty: sig.output(),
            params,
            param_tys,
            has_self,
        };

        let mut is_delegation = false;
        let mut process_expr = |expr, ret_postproc, stmts_before| {
            let mut expr_visitor = ExprVisitor::new(&caller, ret_postproc, stmts_before);
            expr_visitor.visit_expr(expr);
            for callee in expr_visitor.callees {
                is_delegation |= caller.is_delegation(&callee);
                caller.try_emit(&callee);
            }
        };

        for (i, stmt) in block.stmts.iter().enumerate() {
            if let StmtKind::Expr(expr) | StmtKind::Semi(expr) = stmt.kind {
                // Treat the last statement as non-postprocessing if its type matches the caller
                // return type, this often happens with unit return types.
                let ret_postproc = i + 1 != block.stmts.len()
                    || block.expr.is_some()
                    || !caller.same_ty(caller.typeck_results.expr_ty(expr), caller.ret_ty);
                process_expr(expr, ret_postproc, i != 0);
            }
        }

        if let Some(expr) = block.expr {
            process_expr(expr, false, !block.stmts.is_empty());
        }

        if is_delegation {
            let parent_def_id = tcx.local_parent(def_id);
            *self.delegations_per_parent_stats.entry(parent_def_id).or_default() += 1;
        }
    }
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers { check_delegation, ..*providers };
}

fn check_delegation(tcx: TyCtxt<'_>, (): ()) {
    let mut visitor = DelegationPatternVisitor::new(tcx);
    tcx.hir().visit_all_item_likes_in_crate(&mut visitor);
    visitor.emit_methods_stats();
}
