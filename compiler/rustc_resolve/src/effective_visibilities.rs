use crate::{ImportKind, NameBinding, NameBindingKind, Resolver};
use rustc_ast::ast;
use rustc_ast::visit;
use rustc_ast::visit::Visitor;
use rustc_ast::Crate;
use rustc_ast::EnumDef;
use rustc_data_structures::intern::Interned;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::LocalDefId;
use rustc_hir::def_id::CRATE_DEF_ID;
use rustc_middle::middle::privacy::{EffectiveVisibilities, EffectiveVisibility, Level};
use rustc_middle::ty::Visibility;
use std::mem;

type Patrikey<'a> = Interned<'a, NameBinding<'a>>;

pub struct EffectiveVisibilitiesVisitor<'r, 'a> {
    r: &'r mut Resolver<'a>,
    #[allow(unused)]
    my_map: EffectiveVisibilities<Patrikey<'a>>,
    // It's possible to recalculate this at any point, but it's relatively expensive.
    current_normal_mod: LocalDefId,
    changed: bool,
}

#[derive(Clone, Copy)]
enum Parent<'a> {
    DefId(LocalDefId),
    Binding(&'a NameBinding<'a>),
}

impl<'r, 'a> EffectiveVisibilitiesVisitor<'r, 'a> {
    /// Fills the `Resolver::effective_visibilities` table with public & exported items
    /// For now, this doesn't resolve macros (FIXME) and cannot resolve Impl, as we
    /// need access to a TyCtxt for that.
    pub fn compute_effective_visibilities<'c>(r: &'r mut Resolver<'a>, krate: &'c Crate) {
        let mut visitor = EffectiveVisibilitiesVisitor {
            r,
            my_map: Default::default(),
            current_normal_mod: CRATE_DEF_ID,
            changed: false,
        };

        visitor.update(CRATE_DEF_ID, CRATE_DEF_ID);
        visitor.set_bindings_effective_visibilities(CRATE_DEF_ID);

        while visitor.changed {
            visitor.changed = false;
            visit::walk_crate(&mut visitor, krate);
        }

        info!("resolve::effective_visibilities: {:#?}", r.effective_visibilities);
    }

    fn nearest_normal_mod(&mut self, def_id: LocalDefId) -> LocalDefId {
        self.r.get_nearest_non_block_module(def_id.to_def_id()).nearest_parent_mod().expect_local()
    }

    /// Update effective visibilities of bindings in the given module,
    /// including their whole reexport chains.
    fn set_bindings_effective_visibilities(&mut self, module_id: LocalDefId) {
        assert!(self.r.module_map.contains_key(&&module_id.to_def_id()));
        let module = self.r.get_module(module_id.to_def_id()).unwrap();
        let resolutions = self.r.resolutions(module);

        for (_, name_resolution) in resolutions.borrow().iter() {
            if let Some(mut binding) = name_resolution.borrow().binding() && !binding.is_ambiguity() {
                // Set the given effective visibility level to `Level::Direct` and
                // sets the rest of the `use` chain to `Level::Reexported` until
                // we hit the actual exported item.
                let mut normal_mod_id = self.current_normal_mod;
                let mut parent_id = Parent::DefId(module_id);
                let mut level = Level::Direct;
                while let NameBindingKind::Import { binding: nested_binding, import, .. } =
                    binding.kind
                {
                    if level != Level::Direct {
                        // Not a first binding in the chain, recalculate parent module.
                        normal_mod_id = self.nearest_normal_mod(match parent_id {
                            Parent::DefId(def_id) => def_id,
                            Parent::Binding(binding) => match binding.kind {
                                NameBindingKind::Import { import, .. } => {
                                    import.id().map_or(CRATE_DEF_ID, |id| self.r.local_def_id(id))
                                }
                                _ => unreachable!()
                            }
                        });
                    }

                    self.update_ext2(Interned::new_unchecked(binding), parent_id, normal_mod_id, level);

                    // In theory we should reset the parent id to something private
                    // here, but `macro_use` imports always refer to external items,
                    // so it doesn't matter and we can just do nothing.
                    // In theory we should reset the parent id to something public
                    // here, but it has the same effect as leaving the previous parent,
                    // so we can just do nothing.
                    if let Some(node_id) = import.id() {
                        let mut update = |node_id| {
                            self.update_ext(
                                self.r.local_def_id(node_id),
                                binding.vis.expect_local(),
                                parent_id,
                                normal_mod_id,
                                level,
                            )
                        };
                        update(node_id);
                        if let ImportKind::Single { additional_ids: (id1, id2), .. } = import.kind {
                            // In theory all the import IDs have individual visibilities and
                            // effective visibilities, but in practice these IDs go straigth to HIR
                            // where all their few uses assume that their (effective) visibility
                            // applies to the whole syntactic `use` item. So we update them all to
                            // the maximum value among the potential individual effective
                            // visibilities. Maybe HIR for imports shouldn't use three IDs at all.
                            if id1 != ast::DUMMY_NODE_ID {
                                update(id1);
                            }
                            if id2 != ast::DUMMY_NODE_ID {
                                update(id2);
                            }
                        }
                    }

                    parent_id = Parent::Binding(binding);
                    binding = nested_binding;
                    level = Level::Reexported;
                }

                let res = binding.res();
                if let Some(def_id) = res.opt_def_id().and_then(|id| id.as_local()) {
                    // FIXME: The `DefKind::Mod` condition is incorrect, but it preserves
                    // pre-existing logic. Rewrite update logic to support parent nodes that are
                    // fully private and not in the table (which makes sense for `mod` items).
                    if level != Level::Direct || matches!(res, Res::Def(DefKind::Mod, _)) {
                        // Not a first binding in the chain, recalculate parent module.
                        normal_mod_id = self.nearest_normal_mod(def_id);
                    }
                    self.update_ext(
                        def_id,
                        binding.vis.expect_local(),
                        parent_id,
                        normal_mod_id,
                        level,
                    );
                }
            }
        }
    }

    fn visibility(&self, parent_id: Parent<'_>) -> Visibility {
        match parent_id {
            Parent::DefId(def_id) => self.r.visibilities[&def_id],
            Parent::Binding(binding) => binding.vis.expect_local(),
        }
    }

    fn effective_vis(&self, parent_id: Parent<'a>) -> Option<&EffectiveVisibility> {
        match parent_id {
            Parent::DefId(def_id) => self.r.effective_visibilities.effective_vis(def_id),
            Parent::Binding(binding) => self.my_map.effective_vis(Interned::new_unchecked(binding)),
        }
    }

    fn update_ext2(
        &mut self,
        binding: Patrikey<'a>,
        parent_id: Parent<'a>,
        normal_mod_id: LocalDefId,
        level: Level,
    ) {
        let nominal_vis = binding.vis.expect_local();
        if nominal_vis == Visibility::Restricted(normal_mod_id)
            || self.visibility(parent_id) == Visibility::Restricted(normal_mod_id)
        {
            return;
        }
        let parent_eff_vis = self.effective_vis(parent_id).copied();
        self.changed |= self.my_map.update(
            binding,
            nominal_vis,
            || Visibility::Restricted(normal_mod_id),
            parent_eff_vis.as_ref(),
            level,
            &*self.r,
        );
    }

    fn update_ext(
        &mut self,
        def_id: LocalDefId,
        nominal_vis: Visibility,
        parent_id: Parent<'a>,
        normal_mod_id: LocalDefId,
        level: Level,
    ) {
        if nominal_vis == Visibility::Restricted(normal_mod_id)
            || self.visibility(parent_id) == Visibility::Restricted(normal_mod_id)
        {
            return;
        }
        let parent_eff_vis = self.effective_vis(parent_id).copied();
        let mut effective_visibilities = std::mem::take(&mut self.r.effective_visibilities);
        self.changed |= effective_visibilities.update(
            def_id,
            nominal_vis,
            || {
                if def_id == CRATE_DEF_ID {
                    Visibility::Public
                } else {
                    Visibility::Restricted(normal_mod_id)
                }
            },
            parent_eff_vis.as_ref(),
            level,
            &*self.r,
        );
        self.r.effective_visibilities = effective_visibilities;
    }

    fn update(&mut self, def_id: LocalDefId, parent_id: LocalDefId) {
        self.update_ext(
            def_id,
            self.r.visibilities[&def_id],
            Parent::DefId(parent_id),
            self.current_normal_mod,
            Level::Direct,
        );
    }
}

impl<'r, 'ast> Visitor<'ast> for EffectiveVisibilitiesVisitor<'ast, 'r> {
    fn visit_item(&mut self, item: &'ast ast::Item) {
        let def_id = self.r.local_def_id(item.id);
        // Update effective visibilities of nested items.
        // If it's a mod, also make the visitor walk all of its items
        match item.kind {
            // Resolved in rustc_privacy when types are available
            ast::ItemKind::Impl(..) => return,

            // Should be unreachable at this stage
            ast::ItemKind::MacCall(..) => panic!(
                "ast::ItemKind::MacCall encountered, this should not anymore appear at this stage"
            ),

            ast::ItemKind::Mod(..) => {
                let prev_normal_mod = mem::replace(&mut self.current_normal_mod, def_id);
                self.set_bindings_effective_visibilities(def_id);
                visit::walk_item(self, item);
                self.current_normal_mod = prev_normal_mod;
            }

            ast::ItemKind::Enum(EnumDef { ref variants }, _) => {
                self.set_bindings_effective_visibilities(def_id);
                for variant in variants {
                    let variant_def_id = self.r.local_def_id(variant.id);
                    for field in variant.data.fields() {
                        self.update(self.r.local_def_id(field.id), variant_def_id);
                    }
                }
            }

            ast::ItemKind::Struct(ref def, _) | ast::ItemKind::Union(ref def, _) => {
                for field in def.fields() {
                    self.update(self.r.local_def_id(field.id), def_id);
                }
            }

            ast::ItemKind::Trait(..) => {
                self.set_bindings_effective_visibilities(def_id);
            }

            ast::ItemKind::ExternCrate(..)
            | ast::ItemKind::Use(..)
            | ast::ItemKind::Static(..)
            | ast::ItemKind::Const(..)
            | ast::ItemKind::GlobalAsm(..)
            | ast::ItemKind::TyAlias(..)
            | ast::ItemKind::TraitAlias(..)
            | ast::ItemKind::MacroDef(..)
            | ast::ItemKind::ForeignMod(..)
            | ast::ItemKind::Fn(..) => return,
        }
    }
}
