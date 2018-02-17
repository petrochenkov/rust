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
use rustc::hir::map::definitions::DefPathData;
use rustc::hir::map::REGULAR_SPACE;
use rustc::session::Session;
use rustc::util::nodemap::FxHashMap;
use syntax::ast::*;
use syntax::attr;
use syntax::codemap::{respan, Spanned};
use syntax::fold::{self, Folder};
use syntax::visit::{self, Visitor};
use syntax::ptr::P;
use syntax::symbol::{keywords, Symbol};
use syntax::util::move_map::MoveMap;
use syntax::util::small_vector::SmallVector;
use syntax_pos::hygiene::Mark;
use syntax_pos::Span;

use std::iter;
use std::mem::replace;

struct Binding {
    bmode: BindingMode,
    ident: SpannedIdent,
    pat_span: Span,
    node_id: NodeId,
}

#[derive(PartialEq)]
enum IsBindingsScope {
    Block,
    Expr,
}

struct DesugarIs<'a> {
    session: &'a Session,
    resolver: &'a mut Resolver,
    is_bindings_scope: IsBindingsScope,
    binding_id_remapping: FxHashMap<NodeId, NodeId>,
    is_top_level_expr: bool,
}

struct CollectPatBindings<'a, 'b: 'a> {
    parent: &'a mut CollectExprBindings<'b>,
}

struct CollectExprBindings<'a> {
    resolver: &'a mut Resolver,
    bindings: Vec<Binding>,
    has_is: bool,
}

struct RenamePatBindings;

struct CaninicalizeCondition<'a> {
    session: &'a Session,
}

fn add_underscores(mut ident: Ident) -> Ident {
    if ident.name != keywords::Invalid.name() {
        ident.name = Symbol::intern(&(String::from("__") + &ident.name.as_str()));
    }
    ident
}

impl<'a, 'b, 'ast> Visitor<'ast> for CollectPatBindings<'a, 'b> {
    fn visit_pat(&mut self, pat: &'ast Pat) {
        match pat.node {
            PatKind::Ident(bmode, ident, _) => {
                match self.parent.resolver.get_resolution(pat.id).expect("no resolution for ident pattern").base_def() {
                    Def::Local(node_id) => self.parent.bindings.push(Binding {
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

impl<'a, 'ast> Visitor<'ast> for CollectExprBindings<'a> {
    // Do not recurse into blocks (including `match`) and
    // "bodies" (including closures and array lengths)
    fn visit_stmt(&mut self, _stmt: &'ast Stmt) {}
    fn visit_ty(&mut self, _ty: &'ast Ty) {}
    fn visit_arm(&mut self, arm: &'ast Arm) {
        // HACK: Pull bindings from if guards out of the match,
        // they don't work anyway, but can do less harm this way.
        walk_list!(self, visit_expr, &arm.guard);
    }
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match &expr.node {
            ExprKind::Closure(..) => {}
            ExprKind::Repeat(expr, _) => self.visit_expr(expr),
            ExprKind::Is(_, pats) => {
                self.has_is = true;
                CollectPatBindings { parent: self }.visit_pat(&pats[0]);
                visit::walk_expr(self, expr)
            }
            _ => visit::walk_expr(self, expr),
        }
    }
}

impl Folder for RenamePatBindings {
    fn fold_pat(&mut self, mut pat: P<Pat>) -> P<Pat> {
        if let PatKind::Ident(_, ident, _) = &mut pat.node {
            ident.node = add_underscores(ident.node);
        }
        fold::noop_fold_pat(pat, self)
    }

    // Do not recurse too far
    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> { ty }
    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> { expr }
}

impl<'a> Folder for CaninicalizeCondition<'a> {
    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        let expr = expr.into_inner();
        match expr.node {
            ExprKind::Paren(inner_expr) => {
                // (epxr) => expr
                self.fold_expr(inner_expr)
            }
            ExprKind::Binary(Spanned { node: BinOpKind::And, span }, lhs_expr, rhs_expr) => {
                // (expr1 && expr2) && expr3 => expr1 && (expr2 && expr3), recursively
                let lhs_expr = self.fold_expr(lhs_expr).into_inner();
                let node = match lhs_expr.node {
                    ExprKind::Binary(Spanned { node: BinOpKind::And, span: span2 }, lhs_expr2, rhs_expr2) => {
                        let expr_and = Expr { node: ExprKind::Binary(respan(span, BinOpKind::And), rhs_expr2, rhs_expr), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                        ExprKind::Binary(respan(span2, BinOpKind::And), lhs_expr2, self.fold_expr(P(expr_and)))
                    }
                    _ => ExprKind::Binary(respan(span, BinOpKind::And), P(lhs_expr), self.fold_expr(rhs_expr))
                };
                P(Expr { node, span: expr.span, id: expr.id, attrs: expr.attrs })
            }
            _ => P(expr),
        }
    }
}

impl<'a> DesugarIs<'a> {
    fn has_is(&mut self, expr: &Expr) -> bool {
        let mut collect_expr_bindings = CollectExprBindings {
            resolver: self.resolver,
            bindings: Vec::new(),
            has_is: false
        };
        collect_expr_bindings.visit_expr(&expr);
        collect_expr_bindings.has_is
    }

    fn blockify(&mut self, expr: P<Expr>, mut stmts: Vec<Stmt>) -> P<Expr> {
        let span = expr.span;
        let id = expr.id;
        let stmt = Stmt { span, node: StmtKind::Expr(expr), id: self.session.next_node_id() };
        stmts.push(stmt);
        let block = Block { stmts, rules: BlockCheckMode::Default, recovered: false, span, id: self.session.next_node_id() };
        let block_expr = Expr { node: ExprKind::Block(P(block)), span, id: self.session.next_node_id(), attrs: ThinVec::new() };
        if let Some(def_index) = self.resolver.definitions().opt_def_index(id) {
            if let Some(parent) = self.resolver.definitions().def_key(def_index).parent {
                self.resolver.definitions().create_def_with_parent(
                    parent,
                    block_expr.id,
                    DefPathData::Initializer,
                    REGULAR_SPACE,
                    Mark::root()
                );
            }
        }
        P(block_expr)
    }
}

impl<'a> Folder for DesugarIs<'a> {
    // Sigh, expressions can now sneak into attributes, ignore them for now.
    // (See libcore/sync/atomic.rs:955:15 `#[doc = $s_int_type]`)
    fn fold_attribute(&mut self, attr: Attribute) -> Option<Attribute> {
        Some(attr)
    }
    // Analogous to `CollectExprBindings`
    fn fold_ty(&mut self, ty: P<Ty>) -> P<Ty> {
        let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, true);
        let ty = fold::noop_fold_ty(ty, self);
        self.is_top_level_expr = orig_is_top_level_expr;
        ty
    }
    fn fold_arm(&mut self, arm: Arm) -> Arm {
        Arm {
            attrs: fold::fold_attrs(arm.attrs, self),
            pats: arm.pats.move_map(|x| self.fold_pat(x)),
            guard: arm.guard.map(|x| self.fold_expr(x)),
            body: {
                let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, true);
                let body = self.fold_expr(arm.body);
                self.is_top_level_expr = orig_is_top_level_expr;
                body
            }
        }
    }

    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        let has_is = self.has_is(&expr);
        let expr = if self.is_top_level_expr && has_is { self.blockify(expr, Vec::new()) } else { expr };
        let expr = expr.into_inner();
        let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, false);
        let expr = match expr.node {
            // Analogous to `CollectExprBindings`
            ExprKind::Closure(capture_clause, movability, decl, body, span) => {
                P(Expr {
                    node: ExprKind::Closure(
                        capture_clause,
                        movability,
                        self.fold_fn_decl(decl),
                        {
                            let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, true);
                            let body = self.fold_expr(body);
                            self.is_top_level_expr = orig_is_top_level_expr;
                            body
                        },
                        self.new_span(span),
                    ),
                    id: self.new_id(expr.id),
                    span: self.new_span(expr.span),
                    attrs: fold::fold_attrs(expr.attrs.into(), self).into(),
                })
            }
            ExprKind::Repeat(inner_expr, count) => {
                P(Expr {
                    node: ExprKind::Repeat(
                        self.fold_expr(inner_expr),
                        {
                            let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, true);
                            let count = self.fold_expr(count);
                            self.is_top_level_expr = orig_is_top_level_expr;
                            count
                        },
                    ),
                    id: self.new_id(expr.id),
                    span: self.new_span(expr.span),
                    attrs: fold::fold_attrs(expr.attrs.into(), self).into(),
                })
            }

            ExprKind::Path(None, ..) => {
                self.resolver.remap_binding_id(expr.id, &self.binding_id_remapping);
                P(fold::noop_fold_expr(expr, self))
            }

            _ if !has_is => P(fold::noop_fold_expr(expr, self)),
            ExprKind::While(inner_expr, block, label) => {
                // 'label: while cond {
                //    stmts
                // }
                // =>
                // 'label: loop {
                //     if cond {
                //         stmts
                //     } else {
                //         break
                //     }
                // }
                let expr_break = Expr { node: ExprKind::Break(None, None), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let node = ExprKind::If(inner_expr, block, Some(P(expr_break)));
                let expr_ifelse = Expr { node, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let stmt_ifelse = Stmt { node: StmtKind::Expr(self.fold_expr(P(expr_ifelse))), span: expr.span, id: self.session.next_node_id() };
                let block_loop = Block { stmts: vec![stmt_ifelse], rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let expr_loop = Expr { node: ExprKind::Loop(P(block_loop), label, false), span: expr.span, id: expr.id, attrs: expr.attrs };
                P(expr_loop)
            }
            ExprKind::If(cond, block, opt_else) => {
                // '__end: loop {
                //     '__else: loop {
                //         break '__end match expr1 {
                //             pat1(bindings1) => {
                //                 if expr2 is pat2(bindings2) && ... && exprN is patN(bindingsN) {
                //                     stmts1
                //                 } else {
                //                     break '__else
                //                 }
                //             }
                //             _ => break '__else
                //         }
                //     }
                //     break '__end stmts2
                // }
                let canon_cond = CaninicalizeCondition { session: self.session }.fold_expr(cond).into_inner();
                let lit_true = ExprKind::Lit(P(respan(expr.span, LitKind::Bool(true))));
                let expr_true = Expr { node: lit_true, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let pat_true = Pat { node: PatKind::Lit(P(expr_true)), span: expr.span, id: self.session.next_node_id() };

                let (canon_lhs_expr, canon_lhs_pats, canon_rhs) = match canon_cond.node {
                    ExprKind::Binary(Spanned { node: BinOpKind::And, .. }, lhs_expr, rhs_expr) => {
                        let lhs_expr = lhs_expr.into_inner();
                        match lhs_expr.node {
                            // if expr is pat && other_conds { ... }
                            ExprKind::Is(expr, pats) => (expr, pats, Some(rhs_expr)),
                            // if cond && other_conds { ... }
                            _ => (P(lhs_expr), vec![P(pat_true)], Some(rhs_expr)),
                        }

                    }
                    // if expr is pat { ... }
                    ExprKind::Is(expr, pats) => (expr, pats, None),
                    // if expr { ... }
                    _ => (P(canon_cond), vec![P(pat_true)], None),
                };

                let end_label = Some(Label { ident: Ident::from_str("'__end"), span: expr.span });
                let else_label = Some(Label { ident: Ident::from_str("'__else"), span: expr.span });

                // if expr2 is pat2(bindings2) && ... && exprN is patN(bindingsN) { stmts1 } else { break '__else }
                // OR
                // { stmts1 }
                // if there are no conditions left
                let (node, break_else1_id) = match canon_rhs {
                    Some(canon_rhs) => {
                        let break_else1 = Expr { node: ExprKind::Break(else_label, None), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                        let break_else1_id = break_else1.id;
                        (ExprKind::If(canon_rhs, block, Some(P(break_else1))), Some(break_else1_id))
                    }
                    _ => {
                        (ExprKind::Block(block), None)
                    }
                };
                let inner = Expr { node, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };

                // Binding renaming and assignments
                let bindings = {
                    let mut collect_expr_bindings = CollectExprBindings {
                        resolver: self.resolver,
                        bindings: Vec::new(),
                        has_is: false,
                    };
                    CollectPatBindings { parent: &mut collect_expr_bindings }.visit_pat(&canon_lhs_pats[0]);
                    collect_expr_bindings.bindings
                };
                let canon_lhs_pats = canon_lhs_pats.move_map(|pat| RenamePatBindings.fold_pat(pat));

                let stmt_inner = Stmt { node: StmtKind::Expr(self.fold_expr(P(inner))), span: expr.span, id: self.session.next_node_id() };
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
                stmts.push(stmt_inner);
                let block_inner = Block { stmts, rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let inner = Expr { node: ExprKind::Block(P(block_inner)), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };

                // break '__else
                let break_else2 = Expr { node: ExprKind::Break(else_label, None), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let break_else2_id = break_else2.id;

                // match expr1 { pat1(bindings1) => ..., _ => ... }
                let pat_wild = Pat { node: PatKind::Wild, span: expr.span, id: self.session.next_node_id() };
                let arm_inner = Arm { pats: canon_lhs_pats.move_map(|pat| self.fold_pat(pat)), guard: None, body: P(inner), attrs: Vec::new() };
                let arm_break = Arm { pats: vec![P(pat_wild)], guard: None, body: P(break_else2), attrs: Vec::new() };
                let inner_match = Expr { node: ExprKind::Match(self.fold_expr(canon_lhs_expr), vec![arm_inner, arm_break]), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };

                // break '__end match { ... }
                let break_end1 = Expr { node: ExprKind::Break(end_label, Some(P(inner_match))), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let break_end1_id = break_end1.id;
                let stmt_break_match = Stmt { node: StmtKind::Expr(P(break_end1)), span: expr.span, id: self.session.next_node_id() };

                // '__else: loop { ... }
                let else_block = Block { stmts: vec![stmt_break_match], rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let else_loop = Expr { node: ExprKind::Loop(P(else_block), else_label, true), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                if let Some(break_else1_id) = break_else1_id {
                    self.resolver.set_resolution(break_else1_id, PathResolution::new(Def::Label(else_loop.id)));
                }
                self.resolver.set_resolution(break_else2_id, PathResolution::new(Def::Label(else_loop.id)));
                let stmt_else_loop = Stmt { node: StmtKind::Semi(P(else_loop)), span: expr.span, id: self.session.next_node_id() };

                // break '__end stmts2
                let opt_else = opt_else.map(|e| self.fold_expr(e));
                let break_end2 = Expr { node: ExprKind::Break(end_label, opt_else), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let break_end2_id = break_end2.id;
                let stmt_break_end = Stmt { node: StmtKind::Expr(P(break_end2)), span: expr.span, id: self.session.next_node_id() };

                // '__end: loop { ... }
                let end_block = Block { stmts: vec![stmt_else_loop, stmt_break_end], rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let end_loop = Expr { node: ExprKind::Loop(P(end_block), end_label, true), span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                self.resolver.set_resolution(break_end1_id, PathResolution::new(Def::Label(end_loop.id)));
                self.resolver.set_resolution(break_end2_id, PathResolution::new(Def::Label(end_loop.id)));
                P(end_loop)
            }
            ExprKind::Binary(Spanned { node: BinOpKind::And, .. }, ..) |
            ExprKind::Is(..) => {
                // expr1 && expr2 or expr is pat not in if/while context
                // expr1 && expr2 => if expr1 && expr2 { true } else { false }
                // expr is pat => if expr is pat { true } else { false }
                let lit_true = ExprKind::Lit(P(respan(expr.span, LitKind::Bool(true))));
                let expr_true = Expr { node: lit_true, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let stmt_true = Stmt { node: StmtKind::Expr(P(expr_true)), span: expr.span, id: self.session.next_node_id() };
                let block_true = Block { stmts: vec![stmt_true], rules: BlockCheckMode::Default, recovered: false, span: expr.span, id: self.session.next_node_id() };
                let lit_false = ExprKind::Lit(P(respan(expr.span, LitKind::Bool(false))));
                let expr_false = Expr { node: lit_false, span: expr.span, id: self.session.next_node_id(), attrs: ThinVec::new() };
                let expr_if = Expr { span: expr.span, node: ExprKind::If(P(expr), P(block_true), Some(P(expr_false))), id: self.session.next_node_id(), attrs: ThinVec::new() };
                self.fold_expr(P(expr_if))
            }
            _ => P(fold::noop_fold_expr(expr, self)),
        };
        self.is_top_level_expr = orig_is_top_level_expr;
        expr
    }

    fn fold_stmt(&mut self, stmt: Stmt) -> SmallVector<Stmt> {
        let bindings = {
            let mut collect_expr_bindings = CollectExprBindings {
                resolver: self.resolver,
                bindings: Vec::new(),
                has_is: false,
            };
            match &stmt.node {
                StmtKind::Local(local) => {
                    if let Some(expr) = &local.init {
                        collect_expr_bindings.visit_expr(expr);
                    }
                }
                StmtKind::Expr(expr) | StmtKind::Semi(expr) => {
                    collect_expr_bindings.visit_expr(expr);
                }
                _ => {}
            }
            collect_expr_bindings.bindings
        };

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

        let orig_is_top_level_expr = replace(&mut self.is_top_level_expr, false);
        let stmt = fold::noop_fold_stmt(stmt, self);
        self.is_top_level_expr = orig_is_top_level_expr;

        let stmt = stmt.into_iter().next().expect("statement unwrapped into nothing");
        let (stmt, let_stmts) = if self.is_bindings_scope == IsBindingsScope::Expr && !let_stmts.is_empty() {
            (Stmt {
                node: match stmt.node {
                    StmtKind::Local(local) => StmtKind::Local(local.map(|local| Local {
                        init: local.init.map(|expr| self.blockify(expr, let_stmts)),
                        ..local
                    })),
                    StmtKind::Expr(expr) => StmtKind::Expr(self.blockify(expr, let_stmts)),
                    StmtKind::Semi(expr) => StmtKind::Semi(self.blockify(expr, let_stmts)),
                    node => node,
                },
                ..stmt
            }, Vec::new())
        } else {
            (stmt, let_stmts)
        };

        SmallVector::many(let_stmts.into_iter().chain(iter::once(stmt)))
    }
}

pub fn fold_crate(session: &Session, resolver: &mut Resolver, krate: Crate) -> Crate {
    let is_bindings_scope = if attr::contains_name(&krate.attrs, "rustc_alternative_is_bindings_scope") { IsBindingsScope::Expr } else { IsBindingsScope::Block };
    let mut desugar_is = DesugarIs { session, resolver, is_bindings_scope, is_top_level_expr: true, binding_id_remapping: FxHashMap() };
    desugar_is.fold_crate(krate)
}
