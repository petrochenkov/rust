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
    Same,
    SameUpToSelfType,
    Coerced,
    SameNumber,
    #[default]
    Different,
}

impl fmt::Display for ArgsMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ArgsMatch::Same => "Same",
            ArgsMatch::SameUpToSelfType => "SameUpToSelfType",
            ArgsMatch::Coerced => "Coerced",
            ArgsMatch::SameNumber => "SameNumber",
            ArgsMatch::Different => "Different",
        })
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum DelegateTo {
    Field,
    #[default]
    Other,
}

impl fmt::Display for DelegateTo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            DelegateTo::Field => "Field",
            DelegateTo::Other => "Other",
        })
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum RetMatch {
    Same,
    SameUpToSelfType,
    Coerced,
    #[default]
    Different,
}

impl fmt::Display for RetMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            RetMatch::Same => "Same",
            RetMatch::SameUpToSelfType => "SameUpToSelfType",
            RetMatch::Coerced => "Coerced",
            RetMatch::Different => "Different",
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

#[derive(Default)]
struct CalleeStats {
    same_name: bool,
    ret_match: RetMatch,
    ret_postproc: bool,
    has_self: bool,
    args_match: ArgsMatch,
    delegate_to: DelegateTo,
    args_preproc: bool,
}

#[derive(Debug, Clone)]
struct Fn<'tcx> {
    span: Span,
    name: Symbol,
    inputs: Vec<Ty<'tcx>>,
    output: Ty<'tcx>,
    has_self: bool,
    ret_postproc: bool,
    delegate_to: DelegateTo,
    args_preproc: bool,
}

fn has_self(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    tcx.def_kind(def_id) == DefKind::AssocFn && tcx.associated_item(def_id).fn_has_self_parameter
}

struct ExprVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    caller_params: &'tcx [Param<'tcx>],
    caller_has_self: bool,
    ret_postproc: bool,
    callees: Vec<Fn<'tcx>>,
}

impl<'tcx> ExprVisitor<'tcx> {
    fn new(
        tcx: TyCtxt<'tcx>,
        typeck_results: &'tcx TypeckResults<'tcx>,
        caller_params: &'tcx [Param<'tcx>],
        caller_has_self: bool,
        ret_postproc: bool,
    ) -> Self {
        ExprVisitor {
            tcx,
            typeck_results,
            caller_params,
            caller_has_self,
            ret_postproc,
            callees: Default::default(),
        }
    }
}

impl<'tcx> Visitor<'tcx> for ExprVisitor<'tcx> {
    fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
        match expr.kind {
            ExprKind::MethodCall(seg, self_arg, args, _) => {
                let inputs: Vec<_> = [&[*self_arg], args]
                    .concat()
                    .iter()
                    .map(|arg| self.typeck_results.expr_ty_adjusted(arg))
                    .collect();
                let mut args_preproc = !args.is_empty();
                if 1 + args.len() == self.caller_params.len() {
                    for (arg_expr, param) in iter::zip(args, self.caller_params.iter().skip(1)) {
                        if let ExprKind::Path(QPath::Resolved(None, path)) = arg_expr.kind
                            && let Path { res: Res::Local(arg_res_id), .. } = path
                            && *arg_res_id == param.pat.hir_id
                        {
                            args_preproc = false;
                        }
                    }
                }

                let mut delegate_to = DelegateTo::Other;
                if let Some(self_param) = self.caller_params.first()
                    && let ExprKind::Field(receiver, _) = self_arg.kind
                    && let ExprKind::Path(QPath::Resolved(None, path)) = receiver.kind
                    && let Path { res: Res::Local(arg_res_id), .. } = path
                    && *arg_res_id == self_param.pat.hir_id {
                        delegate_to = DelegateTo::Field;
                }

                self.callees.push(Fn {
                    span: expr.span,
                    name: seg.ident.name,
                    inputs,
                    output: self.typeck_results.expr_ty(expr),
                    has_self: true,
                    ret_postproc: self.ret_postproc,
                    args_preproc,
                    delegate_to,
                });
            }
            ExprKind::Call(func, args) => {
                if let FnDef(def_id, ..) = self.typeck_results.expr_ty(func).kind() {
                    let inputs: Vec<_> =
                        args.iter().map(|arg| self.typeck_results.expr_ty_adjusted(arg)).collect();
                    let skip = usize::from(self.caller_has_self);

                    let mut args_preproc = args.len() > skip;
                    if args.len() == self.caller_params.len() {
                        for (arg_expr, param) in
                            iter::zip(args, self.caller_params.iter()).skip(skip)
                        {
                            if let ExprKind::Path(QPath::Resolved(None, path)) = arg_expr.kind
                                    && let Path { res: Res::Local(arg_res_id), .. } = path
                                    && *arg_res_id == param.pat.hir_id
                                {
                                    args_preproc = false;
                                }
                        }
                    }

                    let mut delegate_to = DelegateTo::Other;
                    if let Some(self_param) = self.caller_params.first()
                        && let Some(self_arg) = args.first()
                        && let ExprKind::Field(receiver, _) = self_arg.kind
                        && let ExprKind::Path(QPath::Resolved(None, path)) = receiver.kind
                        && let Path { res: Res::Local(arg_res_id), .. } = path
                        && *arg_res_id == self_param.pat.hir_id {
                            delegate_to = DelegateTo::Field;
                    }

                    self.callees.push(Fn {
                        span: expr.span,
                        name: self.tcx.item_name(*def_id),
                        inputs,
                        output: self.typeck_results.expr_ty(expr),
                        has_self: has_self(self.tcx, *def_id),
                        ret_postproc: self.ret_postproc,
                        args_preproc,
                        delegate_to,
                    });
                }
            }
            _ => {}
        }
        self.ret_postproc = true;
        intravisit::walk_expr(self, expr);
    }
}

struct Caller<'tcx> {
    func: Fn<'tcx>,
    def_id: LocalDefId,
    stmts_count: StmtsCount,
    comparator: Comparator<'tcx>,
}

impl<'tcx> Caller<'tcx> {
    fn compare_inputs(&self, callee: &Fn<'tcx>) -> ArgsMatch {
        let caller = &self.func;
        if callee.inputs.len() != caller.inputs.len() {
            return ArgsMatch::Different;
        }

        // `self` argument in the caller may be transformed (`self` -> `self.target_expr()`),
        // so don't include its type into the comparison.
        let same = |src, dst| -> bool { self.comparator.same(src, dst) };
        let mut args_match = ArgsMatch::Same;
        for (&callee_param, &caller_param) in
            iter::zip(&callee.inputs, &caller.inputs).skip(usize::from(caller.has_self))
        {
            if !same(callee_param, caller_param) {
                // Approximate `Self` as `typeof(self)` with references removed.
                if callee.has_self
                    && caller.has_self
                    && same(callee.inputs[0].peel_refs(), callee_param.peel_refs())
                    && same(caller.inputs[0].peel_refs(), caller_param.peel_refs())
                {
                    args_match = ArgsMatch::SameUpToSelfType;
                } else if self.comparator.coercible(caller_param, callee_param) {
                    args_match = ArgsMatch::Coerced;
                } else {
                    return ArgsMatch::SameNumber;
                }
            }
        }

        args_match
    }

    fn compare_output(&self, callee: &Fn<'tcx>) -> RetMatch {
        let caller = &self.func;
        let same = |src, dst| -> bool { self.comparator.same(src, dst) };

        if same(caller.output, callee.output) {
            return RetMatch::Same;
        }

        // Approximate `Self` as `typeof(self)` with references removed.
        if callee.has_self
            && caller.has_self
            && same(callee.inputs[0].peel_refs(), callee.output.peel_refs())
            && same(caller.inputs[0].peel_refs(), caller.output.peel_refs())
        {
            return RetMatch::SameUpToSelfType;
        }

        if self.comparator.coercible(callee.output, caller.output) {
            return RetMatch::Coerced;
        }

        RetMatch::Different
    }

    fn collect_stats(&self, callee: &Fn<'tcx>) -> CalleeStats {
        CalleeStats {
            same_name: callee.name == self.func.name,
            ret_match: self.compare_output(callee),
            ret_postproc: callee.ret_postproc,
            has_self: callee.has_self,
            args_match: self.compare_inputs(callee),
            delegate_to: callee.delegate_to,
            args_preproc: callee.args_preproc,
        }
    }

    fn try_emit(&self, tcx: TyCtxt<'tcx>, callee: &Fn<'tcx>, stats: &CalleeStats) {
        let caller = &self.func;
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
                caller: caller.span,
                parent: parent.to_string(),
                same_name: stats.same_name,
                ret_match: stats.ret_match.to_string(),
                ret_postproc: stats.ret_postproc,
                callee_has_self: stats.has_self,
                caller_has_self: caller.has_self,
                args_match: stats.args_match.to_string(),
                delegate_to: stats.delegate_to.to_string(),
                args_preproc: stats.args_preproc,
                // Audit
                stmts: self.stmts_count.to_string(),
            },
        );

        if !self.is_detailed(stats) {
            tcx.emit_spanned_lint(
                lint::builtin::DELEGATIONS,
                hir_id,
                callee.span,
                errors::Delegation {
                    callee: callee.span,
                    ret_match: stats.ret_match.to_string(),
                    ret_postproc: stats.ret_postproc,
                    callee_has_self: stats.has_self,
                    caller_has_self: caller.has_self,
                    args_match: stats.args_match.to_string(),
                    args_preproc: stats.args_preproc,
                },
            );
        }
    }

    fn is_detailed(&self, stats: &CalleeStats) -> bool {
        matches!(self.stmts_count, StmtsCount::Other | StmtsCount::OneWithTail)
            || matches!(stats.args_match, ArgsMatch::Different)
            || !stats.same_name
    }

    fn is_delegation(&self, stats: &CalleeStats) -> bool {
        !self.is_detailed(stats) && !matches!(stats.args_match, ArgsMatch::SameNumber)
    }
}

struct Comparator<'tcx> {
    tcx: TyCtxt<'tcx>,
    param_env: ParamEnv<'tcx>,
    def_id: LocalDefId,
}

impl<'tcx> Comparator<'tcx> {
    fn new(tcx: TyCtxt<'tcx>, def_id: LocalDefId) -> Self {
        Comparator { tcx, param_env: tcx.param_env(def_id), def_id }
    }

    fn same(&self, src: Ty<'tcx>, dst: Ty<'tcx>) -> bool {
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

    fn coercible(&self, src: Ty<'tcx>, dst: Ty<'tcx>) -> bool {
        let inh = Inherited::new(self.tcx, self.def_id);
        let fcx = FnCtxt::new(&inh, self.param_env, self.def_id);

        fcx.can_coerce(src, dst)
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
        let sig = self.tcx.liberate_late_bound_regions(
            def_id.to_def_id(),
            self.tcx.fn_sig(def_id.to_def_id()).instantiate_identity(),
        );
        let inputs = sig.inputs().to_vec();

        // Combine caller data
        let func = Fn {
            span,
            name: self.tcx.item_name(def_id.to_def_id()),
            inputs,
            output: sig.output(),
            has_self: has_self(self.tcx, def_id.to_def_id()),
            ret_postproc: false,            // doesn't matter for the caller
            args_preproc: false,            // doesn't matter for the caller
            delegate_to: DelegateTo::Other, // doesn't matter for the caller
        };
        let stmts_count = match (block.expr, block.stmts.len()) {
            (Some(_), 0) => StmtsCount::ZeroWithTail,
            (None, 1) => StmtsCount::OneWithoutTail,
            (Some(_), 1) => StmtsCount::OneWithTail,
            _ => StmtsCount::Other,
        };
        let comparator = Comparator::new(self.tcx, def_id);
        let caller = Caller { func, def_id, stmts_count, comparator };

        let typeck_results = self.tcx.typeck_body(body_id);

        let mut is_delegation = false;
        let mut process_expr = |expr, ret_postproc: bool| {
            let mut expr_visitor = ExprVisitor::new(
                self.tcx,
                typeck_results,
                body.params,
                caller.func.has_self,
                ret_postproc,
            );
            expr_visitor.visit_expr(expr);
            for callee in expr_visitor.callees {
                let stats = caller.collect_stats(&callee);
                is_delegation = caller.is_delegation(&stats);
                caller.try_emit(self.tcx, &callee, &stats);
            }
        };

        for (i, stmt) in block.stmts.iter().enumerate() {
            if let StmtKind::Expr(expr) | StmtKind::Semi(expr) = stmt.kind {
                // Treat the last statement as non-postprocessing if its type matches the caller
                // return type, this often happens with unit return types.
                let ret_postproc = i + 1 != block.stmts.len()
                    || block.expr.is_some()
                    || !caller.comparator.same(typeck_results.expr_ty(expr), caller.func.output);
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
