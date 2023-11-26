use crate::{ImplTraitPosition, ResolverAstLoweringExt};

use super::{ImplTraitContext, LoweringContext, ParamMode};

use ast::visit::Visitor;
use hir::def::{DefKind, PartialRes, Res};
use hir::{BodyId, HirId};
use rustc_ast as ast;
use rustc_ast::*;
use rustc_hir as hir;
use rustc_hir::def_id::DefId;
use rustc_middle::span_bug;
use rustc_middle::ty::ResolverAstLowering;
use rustc_span::symbol::kw;
use rustc_span::{symbol::Ident, Span};
use rustc_target::spec::abi;
use std::iter;

pub struct DelegationResults<'hir> {
    pub body_id: hir::BodyId,
    pub sig: hir::FnSig<'hir>,
    pub generics: &'hir hir::Generics<'hir>,
}

impl<'hir> LoweringContext<'_, 'hir> {
    pub fn delegation_has_self(&self, node_id: NodeId, span: Span) -> bool {
        let res_id = self.get_delegation_res_id(node_id, span);
        let Some(res_id) = res_id else {
            return false;
        };

        if let Some(local_res_id) = res_id.as_local() {
            self.resolver.has_self.contains(&local_res_id)
        } else {
            match self.tcx.def_kind(res_id) {
                DefKind::Fn => false,
                DefKind::AssocFn => self.tcx.associated_item(res_id).fn_has_self_parameter,
                _ => span_bug!(span, "unexpected DefKind for delegation item"),
            }
        }
    }

    pub fn lower_delegation(
        &mut self,
        delegation: &Delegation,
        node_id: NodeId,
        span: Span,
    ) -> DelegationResults<'hir> {
        let res_id = self.get_delegation_res_id(node_id, span);
        let Some(res_id) = res_id else {
            return self.generate_delegation_error(span);
        };

        let decl = self.lower_delegation_decl(res_id, span);
        let sig = self.lower_delegation_sig(span, decl);
        let body_id = self.lower_delegation_body(sig.decl, delegation, node_id, span);

        let generics = self.lower_delegation_generics(span);
        DelegationResults { body_id, sig, generics }
    }

    fn get_delegation_res_id(&self, node_id: NodeId, span: Span) -> Option<DefId> {
        let res = self
            .resolver
            .get_partial_res(node_id)
            .map(|or| or.full_res().map(|r| r.opt_def_id()).unwrap_or(None))
            .unwrap_or(None);

        if res.is_none() {
            self.tcx
                .sess
                .span_delayed_bug(span, "LoweringContext: couldn't resolve delegation item");
        }

        res
    }

    fn lower_delegation_generics(&mut self, span: Span) -> &'hir hir::Generics<'hir> {
        self.arena.alloc(hir::Generics {
            params: &[],
            predicates: &[],
            has_where_clause_predicates: false,
            where_clause_span: span,
            span,
        })
    }

    fn lower_delegation_decl(&mut self, res_id: DefId, span: Span) -> &'hir hir::FnDecl<'hir> {
        let args_count = if let Some(local_res_id) = res_id.as_local() {
            // Map may be filled incorrectly due to recursive delegation.
            // Error will be emmited later in astconv.
            self.resolver.fn_parameter_counts.get(&local_res_id).cloned().unwrap_or_default()
        } else {
            self.tcx.fn_arg_names(res_id).len()
        };
        let inputs = self.arena.alloc_from_iter((0..args_count).into_iter().map(|arg| hir::Ty {
            hir_id: self.next_id(),
            kind: hir::TyKind::InferDelegation(res_id, hir::InferDelegationKind::Input(arg)),
            span,
        }));

        let output = self.arena.alloc(hir::Ty {
            hir_id: self.next_id(),
            kind: hir::TyKind::InferDelegation(res_id, hir::InferDelegationKind::Output),
            span,
        });

        self.arena.alloc(hir::FnDecl {
            inputs,
            output: hir::FnRetTy::Return(output),
            c_variadic: false,
            lifetime_elision_allowed: true,
            implicit_self: hir::ImplicitSelfKind::None,
        })
    }

    fn lower_delegation_sig(
        &mut self,
        span: Span,
        decl: &'hir hir::FnDecl<'hir>,
    ) -> hir::FnSig<'hir> {
        hir::FnSig {
            decl,
            header: hir::FnHeader {
                unsafety: hir::Unsafety::Normal,
                constness: hir::Constness::NotConst,
                asyncness: hir::IsAsync::NotAsync,
                abi: abi::Abi::Rust,
            },
            span: self.lower_span(span),
        }
    }

    fn generate_param(&mut self, ty: &'hir hir::Ty<'hir>) -> (hir::Param<'hir>, NodeId) {
        let pat_node_id = self.next_node_id();
        let pat_id = self.lower_node_id(pat_node_id);
        let pat = self.arena.alloc(hir::Pat {
            hir_id: pat_id,
            kind: hir::PatKind::Binding(hir::BindingAnnotation::NONE, pat_id, Ident::empty(), None),
            span: ty.span,
            default_binding_modes: false,
        });

        (hir::Param { hir_id: self.next_id(), pat, ty_span: ty.span, span: ty.span }, pat_node_id)
    }

    fn generate_arg(&mut self, ty: &'hir hir::Ty<'hir>, param_id: HirId) -> hir::Expr<'hir> {
        let segments = self.arena.alloc_from_iter(iter::once(hir::PathSegment {
            ident: Ident::empty(),
            hir_id: self.next_id(),
            res: Res::Local(param_id),
            args: None,
            infer_args: false,
        }));

        let path =
            self.arena.alloc(hir::Path { span: ty.span, res: Res::Local(param_id), segments });

        hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(hir::QPath::Resolved(None, path)),
            span: ty.span,
        }
    }

    fn lower_delegation_body(
        &mut self,
        decl: &'hir hir::FnDecl<'hir>,
        delegation: &Delegation,
        node_id: NodeId,
        span: Span,
    ) -> BodyId {
        let path_id = if self.is_in_trait_impl { delegation.id } else { node_id };
        let path = self.lower_qpath(
            path_id,
            &delegation.qself,
            &delegation.path,
            ParamMode::Optional,
            &ImplTraitContext::Disallowed(ImplTraitPosition::Path),
            None,
        );
        let block = delegation.body.as_deref();

        self.lower_body(|this| {
            let mut parameters: Vec<hir::Param<'_>> = Vec::new();
            let mut args: Vec<hir::Expr<'hir>> = Vec::new();

            for (idx, param_ty) in decl.inputs.iter().enumerate() {
                let (param, pat_node_id) = this.generate_param(param_ty);
                parameters.push(param);

                let arg = if let Some(block) = block
                    && idx == 0
                {
                    let mut self_resolver = SelfResolver {
                        resolver: this.resolver,
                        delegation_id: delegation.id,
                        self_param_id: pat_node_id,
                    };
                    self_resolver.visit_block(block);
                    let block = this.lower_block(block, false);
                    hir::Expr {
                        hir_id: this.next_id(),
                        kind: hir::ExprKind::Block(block, None),
                        span,
                    }
                } else {
                    let pat_hir_id = this.lower_node_id(pat_node_id);
                    this.generate_arg(param_ty, pat_hir_id)
                };
                args.push(arg);
            }

            let args = self.arena.alloc_from_iter(args);
            let final_expr = this.generate_call(path, args);
            (this.arena.alloc_from_iter(parameters), final_expr)
        })
    }

    fn generate_call(
        &mut self,
        path: hir::QPath<'hir>,
        args: &'hir [hir::Expr<'hir>],
    ) -> hir::Expr<'hir> {
        let callee = self.arena.alloc(hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Path(path),
            span: path.span(),
        });

        let expr = self.arena.alloc(hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Call(callee, args),
            span: path.span(),
        });

        let block = self.arena.alloc(hir::Block {
            stmts: &[],
            expr: Some(expr),
            hir_id: self.next_id(),
            rules: hir::BlockCheckMode::DefaultBlock,
            span: path.span(),
            targeted_by_break: false,
        });

        hir::Expr {
            hir_id: self.next_id(),
            kind: hir::ExprKind::Block(block, None),
            span: path.span(),
        }
    }

    fn generate_delegation_error(&mut self, span: Span) -> DelegationResults<'hir> {
        let generics = self.lower_delegation_generics(span);

        let decl = self.arena.alloc(hir::FnDecl {
            inputs: &[],
            output: hir::FnRetTy::DefaultReturn(span),
            c_variadic: false,
            lifetime_elision_allowed: true,
            implicit_self: hir::ImplicitSelfKind::None,
        });

        let sig = self.lower_delegation_sig(span, decl);
        let body_id = self.lower_body(|this| {
            let err = this
                .tcx
                .sess
                .span_delayed_bug(span, "LoweringContext: couldn't resolve delegation item");
            let expr = hir::Expr { hir_id: this.next_id(), kind: hir::ExprKind::Err(err), span };
            (&[], expr)
        });
        DelegationResults { generics, body_id, sig }
    }
}

struct SelfResolver<'a> {
    resolver: &'a mut ResolverAstLowering,
    delegation_id: NodeId,
    self_param_id: NodeId,
}

impl<'a> SelfResolver<'a> {
    fn try_replace_id(&mut self, id: NodeId) {
        if let Some(res) = self.resolver.partial_res_map.get(&id)
            && let Some(Res::Local(res_id)) = res.full_res()
            && res_id == self.delegation_id
        {
            let new_res = PartialRes::new(Res::Local(self.self_param_id));
            self.resolver.partial_res_map.insert(id, new_res);
        }
    }
}

impl<'ast, 'a> Visitor<'ast> for SelfResolver<'a> {
    fn visit_path(&mut self, path: &'ast Path, id: NodeId) {
        self.try_replace_id(id);
        visit::walk_path(self, path);
    }

    fn visit_path_segment(&mut self, path_segment: &'ast PathSegment) {
        if kw::SelfLower == path_segment.ident.name {
            self.try_replace_id(path_segment.id);
        }
    }
}
