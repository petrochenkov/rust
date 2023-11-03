use crate::{errors, FnCtxt, Inherited};
use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::intravisit::{self, FnKind, Visitor};
use rustc_hir::{BodyId, Expr, ExprKind, FnDecl, Param, Path, QPath, StmtKind, CRATE_HIR_ID};
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::query::Providers;
use rustc_middle::traits::{DefiningAnchor, ObligationCause};
use rustc_middle::ty::{FnDef, ParamEnv, Ty, TyCtxt, TypeckResults};
use rustc_session::lint;
use rustc_span::def_id::{DefId, LocalDefId};
use rustc_span::{Span, Symbol};
use rustc_trait_selection::traits::ObligationCtxt;
use std::collections::BTreeMap;
use std::{fmt, iter};

#[derive(Default, Debug, Copy, Clone)]
enum StmtsCount {
    ZeroWithTail,
    OneWithoutTail,
    OneWithTail,
    #[default]
    Other,
}

impl fmt::Display for StmtsCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            StmtsCount::ZeroWithTail => "ZeroWithTail",
            StmtsCount::OneWithoutTail => "OneWithoutTail",
            StmtsCount::OneWithTail => "OneWithTail",
            StmtsCount::Other => "Other",
        })
    }
}

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
            ArgsMatch::SameCount(TyMatch::Different) => "SameNumber",
            ArgsMatch::DifferentCount => "Different",
        })
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum DelegateTo {
    Field,
    FirstParam,
    #[default]
    Other,
}

impl fmt::Display for DelegateTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            DelegateTo::Field => "Field",
            DelegateTo::FirstParam => "FirstParam",
            DelegateTo::Other => "Other",
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

#[derive(Debug, Clone)]
struct Callee {
    name: Symbol,
    span: Span,
    ret_postproc: bool,
    ret_match: TyMatch,
    delegate_to: DelegateTo,
    args_preproc: bool,
    args_match: ArgsMatch,
    has_self: bool,
}

fn has_self(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    tcx.def_kind(def_id) == DefKind::AssocFn && tcx.associated_item(def_id).fn_has_self_parameter
}

fn refers_to_param(expr: &Expr<'_>, param: &Param<'_>) -> bool {
    if let ExprKind::Path(QPath::Resolved(None, path)) = expr.kind
        && let Path { res: Res::Local(arg_res_id), .. } = path
        && *arg_res_id == param.pat.hir_id {
        true
    } else {
        false
    }
}

struct ExprVisitor<'c, 'tcx> {
    caller: &'c Caller<'tcx>,
    ret_postproc: bool,
    callees: Vec<Callee>,
}

impl<'c, 'tcx> ExprVisitor<'c, 'tcx> {
    fn new(caller: &'c Caller<'tcx>, ret_postproc: bool) -> Self {
        ExprVisitor { caller, ret_postproc, callees: Default::default() }
    }

    fn ret_match(&self, ret_ty: Ty<'tcx>, self_ty: Option<Ty<'tcx>>) -> TyMatch {
        let caller = self.caller;
        caller.compare_ty(ret_ty, caller.ret_ty, self_ty, caller.self_ty)
    }

    fn delegate_to(&self, first_arg: Option<&Expr<'_>>) -> DelegateTo {
        let caller = self.caller;
        match (first_arg, caller.params.first()) {
            (Some(first_arg), Some(first_param)) => {
                if refers_to_param(first_arg, first_param) {
                    DelegateTo::FirstParam
                } else if let ExprKind::Field(receiver, _) = first_arg.kind
                    && refers_to_param(receiver, first_param) {
                    DelegateTo::Field
                } else {
                    DelegateTo::Other
                }
            }
            (None, None) => DelegateTo::FirstParam,
            _ => DelegateTo::Other,
        }
    }

    fn args_preproc(&self, args: &[&Expr<'_>]) -> bool {
        let caller = self.caller;
        if args.len() != caller.params.len() {
            return true;
        }

        // First argument can be transformed (`arg0` -> `target_expr(arg0)`),
        // so don't include it into the comparison.
        for (arg, param) in iter::zip(args, caller.params.iter()).skip(1) {
            if !refers_to_param(arg, param) {
                return true;
            }
        }

        false
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
                Some((seg.ident.name, true, args))
            }
            ExprKind::Call(func, args) if let FnDef(def_id, ..) = typeck_results.expr_ty(func).kind() => {
                let args: Vec<_> = args.iter().collect();
                Some((caller.tcx.item_name(*def_id), has_self(caller.tcx, *def_id), args))
            }
            _ => None,
        };

        if let Some((name, has_self, args)) = callee {
            let arg_tys: Vec<_> =
                args.iter().map(|arg| typeck_results.expr_ty_adjusted(arg)).collect();
            // FIXME: `Self` type is approximated as `typeof(self)` with references removed.
            let self_ty = has_self.then(|| arg_tys[0].peel_refs());

            self.callees.push(Callee {
                name,
                span: expr.span,
                ret_postproc: self.ret_postproc,
                ret_match: self.ret_match(typeck_results.expr_ty(expr), self_ty),
                delegate_to: self.delegate_to(args.first().copied()),
                args_preproc: self.args_preproc(&args),
                args_match: self.args_match(&arg_tys, self_ty),
                has_self,
            });
        }

        self.ret_postproc = true;
        intravisit::walk_expr(self, expr);
    }
}

struct Caller<'tcx> {
    tcx: TyCtxt<'tcx>,
    def_id: LocalDefId,
    typeck_results: &'tcx TypeckResults<'tcx>,
    param_env: ParamEnv<'tcx>,
    name: Symbol,
    span: Span,
    self_ty: Option<Ty<'tcx>>,
    ret_ty: Ty<'tcx>,
    params: &'tcx [Param<'tcx>],
    param_tys: &'tcx [Ty<'tcx>],
    has_self: bool,
    stmts_count: StmtsCount,
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
        let parent = match tcx.def_kind(tcx.local_parent(self.def_id)) {
            DefKind::Impl { of_trait: false } => Parent::InherentImpl,
            DefKind::Impl { of_trait: true } => Parent::TraitImpl,
            DefKind::Trait => Parent::Trait,
            _ => Parent::Other,
        };
        let hir_id = tcx.hir().local_def_id_to_hir_id(self.def_id);
        tcx.emit_spanned_lint(
            lint::builtin::DELEGATIONS_DETAILED,
            hir_id,
            callee.span,
            errors::DelegationDetailed {
                callee: callee.span,
                caller: self.span,
                parent: parent.to_string(),
                same_name: callee.name == self.name,
                ret_match: callee.ret_match.to_string(),
                ret_postproc: callee.ret_postproc,
                callee_has_self: callee.has_self,
                caller_has_self: self.has_self,
                args_match: callee.args_match.to_string(),
                delegate_to: callee.delegate_to.to_string(),
                args_preproc: callee.args_preproc,
                // Audit
                stmts: self.stmts_count.to_string(),
            },
        );

        if !self.is_detailed(callee) {
            tcx.emit_spanned_lint(
                lint::builtin::DELEGATIONS,
                hir_id,
                callee.span,
                errors::Delegation {
                    callee: callee.span,
                    ret_match: callee.ret_match.to_string(),
                    ret_postproc: callee.ret_postproc,
                    callee_has_self: callee.has_self,
                    caller_has_self: self.has_self,
                    args_match: callee.args_match.to_string(),
                    args_preproc: callee.args_preproc,
                },
            );
        }
    }

    fn is_detailed(&self, callee: &Callee) -> bool {
        matches!(self.stmts_count, StmtsCount::Other | StmtsCount::OneWithTail)
            || matches!(callee.args_match, ArgsMatch::DifferentCount)
            || callee.name != self.name
    }

    fn is_delegation(&self, callee: &Callee) -> bool {
        !self.is_detailed(callee)
            && !matches!(callee.args_match, ArgsMatch::SameCount(TyMatch::Different))
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
        let body = self.tcx.hir().body(body_id);
        let block = match body.value.kind {
            ExprKind::Block(block, _) => block,
            // Async functions are lowered to this, consider later.
            ExprKind::Closure(..) => return,
            kind => span_bug!(span, "non-block function body: {kind:?}"),
        };

        // Caller signature
        let params = body.params;
        let sig = self.tcx.liberate_late_bound_regions(
            def_id.to_def_id(),
            self.tcx.fn_sig(def_id.to_def_id()).instantiate_identity(),
        );
        let param_tys = sig.inputs();
        assert_eq!(params.len(), param_tys.len());

        // FIXME: `Self` type is approximated as `typeof(self)` with references removed.
        let has_self = has_self(self.tcx, def_id.to_def_id());
        let self_ty = has_self.then(|| param_tys[0].peel_refs());

        // Combine caller data
        let stmts_count = match (block.expr, block.stmts.len()) {
            (Some(_), 0) => StmtsCount::ZeroWithTail,
            (None, 1) => StmtsCount::OneWithoutTail,
            (Some(_), 1) => StmtsCount::OneWithTail,
            _ => StmtsCount::Other,
        };
        let caller = Caller {
            tcx: self.tcx,
            def_id,
            typeck_results: self.tcx.typeck_body(body_id),
            param_env: self.tcx.param_env(def_id),
            name: self.tcx.item_name(def_id.to_def_id()),
            span,
            self_ty,
            ret_ty: sig.output(),
            params,
            param_tys,
            has_self,
            stmts_count,
        };

        let mut is_delegation = false;
        let mut process_expr = |expr, ret_postproc: bool| {
            let mut expr_visitor = ExprVisitor::new(&caller, ret_postproc);
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
                process_expr(expr, ret_postproc);
            }
        }

        if let Some(expr) = block.expr {
            process_expr(expr, false);
        }

        if is_delegation {
            let parent_def_id = self.tcx.local_parent(def_id);
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
