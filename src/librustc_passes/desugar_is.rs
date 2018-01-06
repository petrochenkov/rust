// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![allow(unused)]

// use rustc::lint;
use rustc::hir::def::{Def, PathResolution};
use rustc::hir::lowering::Resolver;
use rustc::session::Session;
use rustc::util::nodemap::FxHashMap;
use syntax::ast::*;
// use syntax::attr;
// use syntax::codemap::Spanned;
// use syntax::parse::token;
// use syntax::symbol::keywords;
use syntax::fold::{self, Folder};
use syntax::visit::{self, Visitor};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use syntax_pos::Span;
// use errors;

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
}

struct SmallDesugarIs<'a> {
    session: &'a Session,
    resolver: &'a mut Resolver,
    bindings: Vec<Binding>,
}

struct MicroDesugarIs<'a, 'b: 'a> {
    small: &'a mut SmallDesugarIs<'b>,
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
    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        let expr = expr.into_inner();
        match expr.node {
            ExprKind::Paren(expr) => self.fold_expr(expr),
            _ => P(fold::noop_fold_expr(expr, self)),
        }
    }

    fn fold_stmt(&mut self, stmt: Stmt) -> SmallVector<Stmt> {
        let bindings = {
            let mut small_desugar_is = SmallDesugarIs {
                session: self.session,
                resolver: self.resolver,
                bindings: Vec::new()
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

        let stmt_span = stmt.span;
        // if !bindings.is_empty() {
        //     self.session.span_warn(stmt_span, &format!("Collected bindings: {:?}", bindings));
        // }

        let mut binding_id_remapping = FxHashMap();

        let old_stmts = fold::noop_fold_stmt(stmt, self).into_iter().collect::<Vec<_>>();
        let new_stmts = bindings.iter().map(|binding| {
            let pat = Pat {
                id: self.session.next_node_id(),
                node: PatKind::Ident(binding.bmode, binding.ident, None),
                span: binding.pat_span,
            };
            binding_id_remapping.insert(binding.node_id, pat.id);
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

        for (old_id, new_id) in binding_id_remapping {
            self.resolver.remap_binding_id(old_id, new_id);
        }

        SmallVector::many(new_stmts.into_iter().chain(old_stmts.clone().into_iter()))
    }
}

pub fn fold_crate(session: &Session, resolver: &mut Resolver, krate: Crate) -> Crate {
    let large_desugar_is = &mut LargeDesugarIs { session, resolver };
    // visit::walk_crate(large_desugar_is, &krate);
    large_desugar_is.fold_crate(krate)
}
