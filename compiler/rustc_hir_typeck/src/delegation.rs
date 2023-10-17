use crate::{errors, FnCtxt, Inherited};
use rustc_data_structures::fx::FxIndexMap;
use rustc_hir::def::DefKind;
use rustc_hir::intravisit::{self, FnKind, Visitor};
use rustc_hir::{BodyId, Expr, ExprKind, FnDecl, StmtKind, CRATE_HIR_ID};
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::query::Providers;
use rustc_middle::traits::{DefiningAnchor, ObligationCause};
use rustc_middle::ty::{FnDef, ParamEnv, Ty, TyCtxt, TypeckResults};
use rustc_session::lint;
use rustc_span::def_id::LocalDefId;
use rustc_span::{Span, Symbol};
use rustc_trait_selection::traits::ObligationCtxt;
use std::collections::BTreeMap;
use std::{fmt, iter, slice};

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
        match self {
            StmtsCount::ZeroWithTail => write!(f, "ZeroWithTail"),
            StmtsCount::OneWithoutTail => write!(f, "OneWithoutTail"),
            StmtsCount::OneWithTail => write!(f, "OneWithTail"),
            StmtsCount::Other => write!(f, "Other"),
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum ArgsMatch {
    Same,
    SameUpToSelfType,
    SameNumber,
    #[default]
    Different,
}

impl fmt::Display for ArgsMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgsMatch::Same => write!(f, "Same"),
            ArgsMatch::SameUpToSelfType => write!(f, "SameUpToSelfType"),
            ArgsMatch::SameNumber => write!(f, "SameNumber"),
            ArgsMatch::Different => write!(f, "Different"),
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum RetMatch {
    Same,
    SameUpToSelfType,
    #[default]
    Different,
}

impl fmt::Display for RetMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetMatch::Same => write!(f, "Same"),
            RetMatch::SameUpToSelfType => write!(f, "SameUpToSelfType"),
            RetMatch::Different => write!(f, "Different"),
        }
    }
}

#[derive(Default, Debug, Copy, Clone)]
enum HasSelf {
    Value,
    Type,
    #[default]
    Other,
}

impl fmt::Display for HasSelf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HasSelf::Value => write!(f, "Value"),
            HasSelf::Type => write!(f, "Type"),
            HasSelf::Other => write!(f, "Other"),
        }
    }
}

#[derive(Default)]
struct CalleeStats {
    args_match: ArgsMatch,
    ret_match: RetMatch,
    has_self: HasSelf,
    same_name: bool,
    has_expr_after: bool,
}

#[derive(Debug, Clone)]
struct Fn<'tcx> {
    span: Span,
    sym: Symbol,
    inputs: Vec<Ty<'tcx>>,
    output: Ty<'tcx>,
    self_ty: Option<Ty<'tcx>>,
    has_expr_after: bool,
}

struct ExprVisitor<'tcx> {
    callees: Vec<Fn<'tcx>>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    tcx: TyCtxt<'tcx>,
    has_expr_after: bool,
}

impl<'tcx> ExprVisitor<'tcx> {
    fn new(typeck_results: &'tcx TypeckResults<'tcx>, tcx: TyCtxt<'tcx>, is_tail: bool) -> Self {
        ExprVisitor { callees: Default::default(), typeck_results, tcx, has_expr_after: !is_tail }
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
                let self_ty = Some(inputs[0]);
                self.callees.push(Fn {
                    span: expr.span,
                    sym: seg.ident.name,
                    inputs,
                    output: self.typeck_results.expr_ty_adjusted(expr),
                    self_ty,
                    has_expr_after: self.has_expr_after,
                });
            }
            ExprKind::Call(func, args) => {
                if let FnDef(def_id, ..) = self.typeck_results.node_type(func.hir_id).kind() {
                    let inputs =
                        args.iter().map(|arg| self.typeck_results.expr_ty_adjusted(arg)).collect();

                    self.callees.push(Fn {
                        span: expr.span,
                        sym: self.tcx.item_name(*def_id),
                        inputs,
                        output: self.typeck_results.expr_ty_adjusted(expr),
                        self_ty: None,
                        has_expr_after: self.has_expr_after,
                    });
                }
            }
            ExprKind::Array(_)
            | ExprKind::ConstBlock(..)
            | ExprKind::Tup(..)
            | ExprKind::Binary(..)
            | ExprKind::Unary(..)
            | ExprKind::Lit(_)
            | ExprKind::Type(..)
            | ExprKind::If(..)
            | ExprKind::Loop(..)
            | ExprKind::Match(..)
            | ExprKind::Closure(..)
            | ExprKind::Assign(..)
            | ExprKind::AssignOp(..)
            | ExprKind::Index(..)
            | ExprKind::AddrOf(..)
            | ExprKind::Break(..)
            | ExprKind::Continue(..)
            | ExprKind::InlineAsm(..)
            | ExprKind::OffsetOf(..)
            | ExprKind::Repeat(..)
            | ExprKind::Yield(..)
            | ExprKind::Become(..)
            | ExprKind::Err(..) => {
                return;
            }
            ExprKind::Let(..)
            | ExprKind::Struct(..)
            | ExprKind::Field(..)
            | ExprKind::Cast(..)
            | ExprKind::Path(..)
            | ExprKind::Block(..)
            | ExprKind::DropTemps(..)
            | ExprKind::Ret(..) => {}
        }
        self.has_expr_after = true;
        intravisit::walk_expr(self, expr);
    }
}

struct Caller<'tcx> {
    func: Fn<'tcx>,
    def_id: LocalDefId,
    stmts_count: StmtsCount,
    has_self: HasSelf,
    comparator: Comparator<'tcx>,
}

impl<'tcx> Caller<'tcx> {
    fn compare_inputs(&self, callee: &Fn<'tcx>) -> ArgsMatch {
        fn preproc<'a, 'tcx>(func: &'a Fn<'tcx>) -> (slice::Iter<'a, Ty<'tcx>>, usize) {
            let mut len = func.inputs.len();
            let mut iter = func.inputs.iter();
            if func.self_ty.is_some() {
                assert!(len >= 1);
                iter.next();
                len -= 1;
            }
            (iter, len)
        }

        // skip self if exists
        let (callee_iter, callee_len) = preproc(callee);
        let (caller_iter, caller_len) = preproc(&self.func);
        if callee_len != caller_len {
            return ArgsMatch::Different;
        }

        let cmp = |lhs, rhs| -> bool { self.comparator.compare(lhs, rhs) };
        let mut args_match = ArgsMatch::Same;
        for (&callee_param, &caller_param) in iter::zip(callee_iter, caller_iter) {
            if !cmp(callee_param, caller_param) {
                if let (Some(callee_self_ty), Some(caller_self_ty)) =
                    (callee.self_ty, self.func.self_ty)
                {
                    if cmp(callee_self_ty, callee_param) && cmp(caller_self_ty, caller_param) {
                        args_match = ArgsMatch::SameUpToSelfType;
                    } else {
                        return ArgsMatch::SameNumber;
                    }
                } else {
                    return ArgsMatch::SameNumber;
                }
            }
        }

        args_match
    }

    fn compare_output(&self, callee: &Fn<'tcx>) -> RetMatch {
        let caller = &self.func;
        let cmp = |lhs, rhs| -> bool { self.comparator.compare(lhs, rhs) };

        if cmp(caller.output, callee.output) {
            return RetMatch::Same;
        }

        if let (Some(callee_self_ty), Some(caller_self_ty)) = (callee.self_ty, caller.self_ty) {
            // peel refs due to
            // fn bar(&self) -> Self
            if cmp(callee_self_ty.peel_refs(), callee.output.peel_refs())
                && cmp(caller_self_ty.peel_refs(), caller.output.peel_refs())
            {
                return RetMatch::SameUpToSelfType;
            }
        }

        RetMatch::Different
    }

    fn collect_stats(&self, callee: &Fn<'tcx>) -> CalleeStats {
        CalleeStats {
            args_match: self.compare_inputs(callee),
            ret_match: self.compare_output(callee),
            has_self: if callee.self_ty.is_some() { HasSelf::Value } else { self.has_self },
            same_name: callee.sym == self.func.sym,
            has_expr_after: callee.has_expr_after,
        }
    }

    fn try_emit(&self, tcx: TyCtxt<'tcx>, callee: &Fn<'tcx>, stats: &CalleeStats) {
        let caller = &self.func;
        let hir_id = tcx.hir().local_def_id_to_hir_id(self.def_id);
        tcx.emit_spanned_lint(
            lint::builtin::DELEGATIONS_DETAILED,
            hir_id,
            callee.span,
            errors::DelegationDetailed {
                caller: caller.span,
                callee: callee.span,
                args_match: stats.args_match.to_string(),
                ret_match: stats.ret_match.to_string(),
                stmts: self.stmts_count.to_string(),
                self_arg: stats.has_self.to_string(),
                same_name: stats.same_name,
                has_expr_after: stats.has_expr_after,
            },
        );

        if !self.is_detailed(stats) {
            tcx.emit_spanned_lint(
                lint::builtin::DELEGATIONS,
                hir_id,
                callee.span,
                errors::Delegation {
                    callee: callee.span,
                    args_match: stats.args_match.to_string(),
                    ret_match: stats.ret_match.to_string(),
                    self_arg: stats.has_self.to_string(),
                    has_expr_after: stats.has_expr_after,
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

    fn is_subtype(&self, src: Ty<'tcx>, dest: Ty<'tcx>) -> bool {
        if src == dest {
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
        let dest = ocx.normalize(&cause, self.param_env, dest);
        match ocx.sub(&cause, self.param_env, src, dest) {
            Ok(()) => {}
            Err(_) => return false,
        };
        let errors = ocx.select_all_or_error();
        let _ = infcx.take_opaque_types();
        errors.is_empty()
    }

    fn compare(&self, src: Ty<'tcx>, dest: Ty<'tcx>) -> bool {
        if src == dest {
            return true;
        }

        let inh = Inherited::new(self.tcx, self.def_id);
        let fcx = FnCtxt::new(&inh, self.param_env, self.def_id);

        // to avoid `compare` params order errors types
        if fcx.can_coerce(dest, src) {
            return true;
        }
        self.is_subtype(dest, src)
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
        let block = match self.tcx.hir().body(body_id).value.kind {
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

        // Caller self parameter
        let mut self_ty = None;
        let mut has_self = HasSelf::Other;
        if self.tcx.def_kind(def_id) == DefKind::AssocFn {
            has_self = HasSelf::Type;
            if self.tcx.associated_item(def_id).fn_has_self_parameter {
                self_ty = Some(inputs[0]);
            }
        }

        // Combine caller data
        let func = Fn {
            span,
            sym: self.tcx.item_name(def_id.to_def_id()),
            inputs,
            output: sig.output(),
            self_ty,
            has_expr_after: false, // not matter for caller
        };
        let stmts_count = match (block.expr, block.stmts.len()) {
            (Some(_), 0) => StmtsCount::ZeroWithTail,
            (None, 1) => StmtsCount::OneWithoutTail,
            (Some(_), 1) => StmtsCount::OneWithTail,
            _ => StmtsCount::Other,
        };
        let comparator = Comparator::new(self.tcx, def_id);
        let caller = Caller { func, def_id, stmts_count, has_self, comparator };

        let typeck_results = self.tcx.typeck_body(body_id);

        let mut is_delegation = false;
        let mut process_expr = |expr, is_tail: bool| {
            let mut expr_visitor = ExprVisitor::new(typeck_results, self.tcx, is_tail);
            expr_visitor.visit_expr(expr);
            for callee in expr_visitor.callees {
                let stats = caller.collect_stats(&callee);
                is_delegation = caller.is_delegation(&stats);
                caller.try_emit(self.tcx, &callee, &stats);
            }
        };

        let mut it = block.stmts.iter().peekable();
        while let Some(stmt) = it.next() {
            if let StmtKind::Expr(expr) | StmtKind::Semi(expr) = stmt.kind {
                process_expr(expr, it.peek().is_none() && block.expr.is_none());
            }
        }

        if let Some(expr) = block.expr {
            process_expr(expr, true);
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
