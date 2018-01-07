// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use rustc::hir::def::{Def, PathResolution};
use rustc::hir::lowering::Resolver;
use rustc::session::Session;
use rustc::util::nodemap::FxHashMap;
use syntax::ast::*;
use syntax::codemap::respan;
use syntax::fold::{self, Folder};
use syntax::visit::{self, Visitor};
use syntax::ptr::P;
use syntax::symbol::{keywords, Symbol};
use syntax::util::small_vector::SmallVector;
use syntax_pos::Span;

use std::mem::replace;

#[derive(Debug)]
struct Binding {
    bmode: BindingMode,
    ident: SpannedIdent,
    pat_span: Span,
    node_id: NodeId,
}

struct LargeDesugarIs<'a> {
    session: &'a Session,
    resolver: &'a mut Resolver,
    binding_id_remapping: FxHashMap<NodeId, NodeId>,
    rename_bindings: bool,
}

struct SmallDesugarIs<'a> {
    resolver: &'a mut Resolver,
    bindings: Vec<Binding>,
}

struct MicroDesugarIs<'a, 'b: 'a> {
    small: &'a mut SmallDesugarIs<'b>,
}

fn add_underscores(mut ident: Ident) -> Ident {
    if ident.name != keywords::Invalid.name() {
        ident.name = Symbol::intern(&(String::from("__") + &ident.name.as_str()));
    }
    ident
}

impl<'a, 'b, 'ast> Visitor<'ast> for MicroDesugarIs<'a, 'b> {
    fn visit_pat(&mut self, pat: &'ast Pat) {
        match pat.node {
            PatKind::Ident(bmode, ident, _) => {
                match self.small.resolver.get_resolution(pat.id).expect("ubulala").base_def() {
                    Def::Local(node_id) => self.small.bindings.push(Binding {
                        bmode, ident, node_id, pat_span: pat.span
                    }),
                    _ => {}
                }
            }
            _ => {}
        }

        visit::walk_pat(self, pat);
    }

    // Do not recurse too far
    fn visit_ty(&mut self, _ty: &'ast Ty) {}
    fn visit_expr(&mut self, _expr: &'ast Expr) {}
}

impl<'a, 'ast> Visitor<'ast> for SmallDesugarIs<'a> {
    // Do not recurse into blocks (including `match`) and
    // "bodies" (including closures and array lengths)
    fn visit_block(&mut self, _block: &'ast Block) {}
    fn visit_arm(&mut self, _arm: &'ast Arm) {}
    fn visit_ty(&mut self, _ty: &'ast Ty) {}
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match &expr.node {
            ExprKind::Closure(..) => {}
            ExprKind::Repeat(expr, _) => self.visit_expr(expr),
            ExprKind::Is(expr, pat) => {
                self.visit_expr(expr);
                MicroDesugarIs { small: self }.visit_pat(pat);
            }
            _ => visit::walk_expr(self, expr),
        }
    }
}

impl<'a> Folder for LargeDesugarIs<'a> {
    fn fold_pat(&mut self, mut pat: P<Pat>) -> P<Pat> {
        if self.rename_bindings {
            if let PatKind::Ident(_, ident, _) = &mut pat.node {
                ident.node = add_underscores(ident.node);
            }
        }
        fold::noop_fold_pat(pat, self)
    }

    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
        let orig_rename_bindings = replace(&mut self.rename_bindings, true);
        let ty = fold::noop_fold_ty(ty, self);
        self.rename_bindings = orig_rename_bindings;
        ty
    }

    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        let expr = expr.into_inner();
        let orig_rename_bindings = replace(&mut self.rename_bindings, false);
        let expr = match expr.node {
            ExprKind::Paren(expr) => self.fold_expr(expr),
            ExprKind::Is(inner_expr, pat) => {
                // EXPR is PAT(binding1, binding2, ...)
                // =>
                // match EXPR {
                //     PAT(__binding1, __binding2, ...) => {
                //         binding1 = __binding1;
                //         binding2 = __binding2;
                //         ...
                //         true
                //     }
                //     _ => false
                // }

                let bindings = {
                    let mut small_desugar_is = SmallDesugarIs {
                        resolver: self.resolver,
                        bindings: Vec::new(),
                    };
                    MicroDesugarIs { small: &mut small_desugar_is }.visit_pat(&pat);
                    small_desugar_is.bindings
                };

                // binding1 = __binding1; binding2 = __binding2; ...
                let mut stmts = Vec::new();
                for binding in bindings {
                    let path_lhs = Path::from_ident(expr.span, binding.ident.node);
                    let path_rhs = Path::from_ident(expr.span, add_underscores(binding.ident.node));
                    let expr_lhs = Expr { node: ExprKind::Path(None, path_lhs), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                    let expr_rhs = Expr { node: ExprKind::Path(None, path_rhs), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                    self.resolver.set_resolution(expr_lhs.id, PathResolution::new(Def::Local(self.binding_id_remapping.get(&binding.node_id).cloned().expect("missing NodeId in binding_id_remapping"))));
                    self.resolver.set_resolution(expr_rhs.id, PathResolution::new(Def::Local(binding.node_id)));
                    let expr_assign = Expr { node: ExprKind::Assign(P(expr_lhs), P(expr_rhs)), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                    let stmt_assign = Stmt { node: StmtKind::Semi(P(expr_assign)), span: expr.span, id: self.session.next_node_id() };
                    stmts.push(stmt_assign);
                }

                // true
                let lit_true = ExprKind::Lit(P(respan(expr.span, LitKind::Bool(true))));
                let expr_true = Expr { node: lit_true, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let stmt_true = Stmt { node: StmtKind::Expr(P(expr_true)), span: expr.span, id: self.session.next_node_id() };
                stmts.push(stmt_true);

                // { STMTS; true }
                let orig_rename_bindings = replace(&mut self.rename_bindings, true);
                let pat = self.fold_pat(pat);
                self.rename_bindings = orig_rename_bindings;
                let block = Block { stmts, rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let expr_block = Expr { node: ExprKind::Block(P(block)), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let arm_true = Arm { pats: vec![pat], guard: None, body: P(expr_block), beginning_vert: None, attrs: Vec::new() };

                // _ => false
                let lit_false = ExprKind::Lit(P(respan(expr.span, LitKind::Bool(false))));
                let expr_false = Expr { node: lit_false, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let pat_wild = Pat { node: PatKind::Wild, span: expr.span, id: self.session.next_node_id() };
                let arm_false = Arm { pats: vec![P(pat_wild)], guard: None, body: P(expr_false), beginning_vert: None, attrs: Vec::new() };

                // match
                let node = ExprKind::Match(self.fold_expr(inner_expr), vec![arm_true, arm_false]);
                let expr_match = Expr { node, span: expr.span, id: expr.id, attrs: expr.attrs };
                P(expr_match)
            }
            _ => P(fold::noop_fold_expr(expr, self)),
        };
        self.rename_bindings = orig_rename_bindings;
        expr
    }

    fn fold_stmt(&mut self, stmt: Stmt) -> SmallVector<Stmt> {
        let bindings = {
            let mut small_desugar_is = SmallDesugarIs {
                resolver: self.resolver,
                bindings: Vec::new(),
            };
            match &stmt.node {
                StmtKind::Local(local) => {
                    if let Some(expr) = &local.init {
                        small_desugar_is.visit_expr(expr);
                    }
                }
                StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
                    small_desugar_is.visit_expr(expr);
                }
                _ => {}
            }
            small_desugar_is.bindings
        };

        // if !bindings.is_empty() {
        //     self.session.span_warn(stmt.span, &format!("Collected bindings: {:?}", bindings));
        // }

        let orig_binding_id_remapping = replace(&mut self.binding_id_remapping, FxHashMap());
        let let_stmts = bindings.iter().map(|binding| {
            let pat = Pat {
                id: self.session.next_node_id(),
                node: PatKind::Ident(binding.bmode, binding.ident, None),
                span: binding.pat_span,
            };
            self.binding_id_remapping.insert(binding.node_id, pat.id);
            let local = Local {
                id: self.session.next_node_id(),
                pat: P(pat),
                ty: None,
                init: None,
                span: binding.pat_span,
                attrs: ThinVec::new(),
            };
            Stmt {
                id: self.session.next_node_id(),
                node: StmtKind::Local(P(local)),
                span: binding.pat_span,
            }
        }).collect::<Vec<_>>();

        for (&old_id, &new_id) in &self.binding_id_remapping {
            self.resolver.remap_binding_id(old_id, new_id);
        }

        let stmt = fold::noop_fold_stmt(stmt, self);
        self.binding_id_remapping = orig_binding_id_remapping;
        SmallVector::many(let_stmts.into_iter().chain(stmt.into_iter()))
    }
}

pub fn fold_crate(session: &Session, resolver: &mut Resolver, krate: Crate) -> Crate {
    let large_desugar_is = &mut LargeDesugarIs { session, resolver, binding_id_remapping: FxHashMap(), rename_bindings: false };
    large_desugar_is.fold_crate(krate)
}
