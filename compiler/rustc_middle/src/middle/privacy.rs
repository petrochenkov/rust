//! A pass that checks to make sure private fields and methods aren't used
//! outside their scopes. This pass will also generate a set of exported items
//! which are available for use externally when compiled as a library.
use crate::ty::{DefIdTree, Visibility};
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::stable_hasher::{HashStable, StableHasher};
use rustc_macros::HashStable;
use rustc_query_system::ich::StableHashingContext;
use rustc_span::def_id::LocalDefId;
use std::hash::Hash;

/// Represents the levels of accessibility an item can have.
///
/// The variants are sorted in ascending order of accessibility.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, HashStable)]
pub enum AccessLevel {
    /// Superset of `AccessLevel::Reachable` used to mark impl Trait items.
    ReachableFromImplTrait,
    /// Exported items + items participating in various kinds of public interfaces,
    /// but not directly nameable. For example, if function `fn f() -> T {...}` is
    /// public, then type `T` is reachable. Its values can be obtained by other crates
    /// even if the type itself is not nameable.
    Reachable,
    /// Public items + items accessible to other crates with the help of `pub use` re-exports.
    Exported,
    /// Items accessible to other crates directly, without the help of re-exports.
    Public,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, HashStable, Default)]
pub struct EffectiveVisibility {
    public: Option<Visibility>,
    exported: Option<Visibility>,
    reachable: Option<Visibility>,
    reachable_from_impl_trait: Option<Visibility>,
}

impl EffectiveVisibility {
    fn get_field(&self, tag: AccessLevel) -> &Option<Visibility> {
        match tag {
            AccessLevel::Public => &self.public,
            AccessLevel::Exported => &self.exported,
            AccessLevel::Reachable => &self.reachable,
            AccessLevel::ReachableFromImplTrait => &self.reachable_from_impl_trait,
        }
    }

    fn get_mut_field(&mut self, tag: AccessLevel) -> &mut Option<Visibility> {
        match tag {
            AccessLevel::Public => &mut self.public,
            AccessLevel::Exported => &mut self.exported,
            AccessLevel::Reachable => &mut self.reachable,
            AccessLevel::ReachableFromImplTrait => &mut self.reachable_from_impl_trait,
        }
    }

    pub fn get(&self, tag: AccessLevel) -> Option<&Visibility> {
        (*self.get_field(tag)).as_ref()
    }

    fn get_mut(&mut self, tag: AccessLevel) -> Option<&mut Visibility> {
        (*self.get_mut_field(tag)).as_mut()
    }

    fn set(&mut self, effective_vis: Option<Visibility>, tag: AccessLevel) {
        *self.get_mut_field(tag) = effective_vis;
    }

    fn merge(
        &mut self,
        effective_vis_opt: Option<Visibility>,
        tag: AccessLevel,
        tree: impl DefIdTree,
    ) {
        let current_effective_vis = self.get_mut(tag);
        if let Some(effective_vis) = effective_vis_opt {
            if let Some(current_effective_vis) = current_effective_vis {
                if current_effective_vis.is_at_least(effective_vis, tree) {
                    *current_effective_vis = effective_vis;
                }
            } else {
                self.set(effective_vis_opt, tag);
            }
        }
    }

    pub fn update(
        &mut self,
        effective_vis_opt: Option<Visibility>,
        tag: AccessLevel,
        tree: impl DefIdTree,
    ) {
        for level in [
            AccessLevel::Public,
            AccessLevel::Exported,
            AccessLevel::Reachable,
            AccessLevel::ReachableFromImplTrait,
        ] {
            if level <= tag {
                self.merge(effective_vis_opt, level, tree);
            }
        }
    }

    fn is_public(&self, tag: AccessLevel) -> bool {
        self.get(tag).map_or(false, |vis| vis.is_public())
    }
}

/// Holds a map of accessibility levels for reachable HIR nodes.
#[derive(Debug, Clone)]
pub struct AccessLevels<Id = LocalDefId> {
    pub map: FxHashMap<Id, EffectiveVisibility>,
}

impl<Id: Hash + Eq + Copy> AccessLevels<Id> {
    fn is_smth(&self, id: Id, tag: AccessLevel) -> bool {
        self.get_effective_vis(id).map_or(false, |effective_vis| effective_vis.is_public(tag))
    }

    /// See `AccessLevel::Reachable`.
    pub fn is_reachable(&self, id: Id) -> bool {
        self.is_smth(id, AccessLevel::Reachable)
    }

    /// See `AccessLevel::Exported`.
    pub fn is_exported(&self, id: Id) -> bool {
        self.is_smth(id, AccessLevel::Exported)
    }

    /// See `AccessLevel::Public`.
    pub fn is_public(&self, id: Id) -> bool {
        self.is_smth(id, AccessLevel::Public)
    }

    pub fn get_access_level(&self, id: Id) -> Option<AccessLevel> {
        self.get_effective_vis(id).and_then(|effective_vis| {
            for level in [
                AccessLevel::Public,
                AccessLevel::Exported,
                AccessLevel::Reachable,
                AccessLevel::ReachableFromImplTrait,
            ] {
                if effective_vis.is_public(level) {
                    return Some(level);
                }
            }
            None
        })
    }

    pub fn set_access_level(
        &mut self,
        id: Id,
        access_level: Option<AccessLevel>,
        tree: impl DefIdTree,
    ) -> Option<AccessLevel> {
        if let Some(tag) = access_level {
            let mut effective_vis = self.get_effective_vis(id).copied().unwrap_or_default();
            effective_vis.update(Some(Visibility::Public), tag, tree);
            self.set_effective_vis(id, effective_vis);
        }
        self.get_access_level(id)
    }

    pub fn set_effective_vis(&mut self, id: Id, effective_vis: EffectiveVisibility) {
        self.map.insert(id, effective_vis);
    }

    pub fn get_effective_vis(&self, id: Id) -> Option<&EffectiveVisibility> {
        self.map.get(&id)
    }
}

impl<Id> Default for AccessLevels<Id> {
    fn default() -> Self {
        AccessLevels { map: Default::default() }
    }
}

impl<'a> HashStable<StableHashingContext<'a>> for AccessLevels {
    fn hash_stable(&self, hcx: &mut StableHashingContext<'a>, hasher: &mut StableHasher) {
        let AccessLevels { ref map } = *self;
        map.hash_stable(hcx, hasher);
    }
}
