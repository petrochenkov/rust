use crate::ty::{DefIdTree, TyCtxt, Visibility};
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::stable_hasher::{HashStable, StableHasher};
use rustc_hir::def::DefKind;
use rustc_hir::def_id::CRATE_DEF_ID;
use rustc_macros::HashStable;
use rustc_query_system::ich::StableHashingContext;
use rustc_span::def_id::LocalDefId;

/// Represents the levels of effective visibility an item can have.
///
/// The variants are sorted in ascending order of directness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, HashStable)]
pub enum Level {
    /// Superset of `Reachable` including items leaked through return position `impl Trait`.
    ReachableThroughImplTrait,
    /// Item is either reexported, or leaked through any kind of interface.
    /// For example, if function `fn f() -> T {...}` is directly public, then type `T` is publicly
    /// reachable and its values can be obtained by other crates even if the type itself is not
    /// nameable.
    Reachable,
    /// Item is accessible either directly, or with help of `use` reexports.
    Reexported,
    /// Item is directly accessible, without help of reexports.
    Direct,
}

impl Level {
    fn all_levels() -> [Level; 4] {
        [Level::Direct, Level::Reexported, Level::Reachable, Level::ReachableThroughImplTrait]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, HashStable)]
pub struct EffectiveVisibility {
    direct: Visibility,
    reexported: Visibility,
    reachable: Visibility,
    reachable_through_impl_trait: Visibility,
    parent_id: LocalDefId,
}

impl EffectiveVisibility {
    fn at_level(&self, level: Level) -> &Visibility {
        match level {
            Level::Direct => &self.direct,
            Level::Reexported => &self.reexported,
            Level::Reachable => &self.reachable,
            Level::ReachableThroughImplTrait => &self.reachable_through_impl_trait,
        }
    }

    fn at_level_mut(&mut self, level: Level) -> &mut Visibility {
        match level {
            Level::Direct => &mut self.direct,
            Level::Reexported => &mut self.reexported,
            Level::Reachable => &mut self.reachable,
            Level::ReachableThroughImplTrait => &mut self.reachable_through_impl_trait,
        }
    }

    pub fn is_public_at_level(&self, level: Level) -> bool {
        self.at_level(level).is_public()
    }

    fn from_vis(vis: Visibility) -> EffectiveVisibility {
        EffectiveVisibility {
            direct: vis,
            reexported: vis,
            reachable: vis,
            reachable_through_impl_trait: vis,
            parent_id: CRATE_DEF_ID,
        }
    }
}

/// Holds a map of effective visibilities for reachable HIR nodes.
#[derive(Clone, Debug)]
pub struct EffectiveVisibilities<Id = LocalDefId> {
    map: FxHashMap<Id, EffectiveVisibility>,
}

impl<Id> Default for EffectiveVisibilities<Id> {
    fn default() -> Self {
        EffectiveVisibilities { map: Default::default() }
    }
}

impl EffectiveVisibilities {
    fn is_public_at_level(&self, id: LocalDefId, level: Level) -> bool {
        self.map.get(&id).map_or(false, |effective_vis| effective_vis.is_public_at_level(level))
    }

    /// See `Level::Reachable`.
    pub fn is_reachable(&self, id: LocalDefId) -> bool {
        self.is_public_at_level(id, Level::Reachable)
    }

    /// See `Level::Reexported`.
    pub fn is_exported(&self, id: LocalDefId) -> bool {
        self.is_public_at_level(id, Level::Reexported)
    }

    /// See `Level::Direct`.
    pub fn is_directly_public(&self, id: LocalDefId) -> bool {
        self.is_public_at_level(id, Level::Direct)
    }

    pub fn public_at_level(&self, id: LocalDefId) -> Option<Level> {
        self.map.get(&id).and_then(|effective_vis| {
            for level in Level::all_levels() {
                if effective_vis.is_public_at_level(level) {
                    return Some(level);
                }
            }
            None
        })
    }

    pub fn iter(&self) -> impl Iterator<Item = (&LocalDefId, &EffectiveVisibility)> {
        self.map.iter()
    }

    pub fn set_public_at_level(
        &mut self,
        id: LocalDefId,
        default_vis: impl FnOnce() -> Visibility,
        level: Level,
    ) {
        let mut effective_vis = self
            .map
            .get(&id)
            .copied()
            .unwrap_or_else(|| EffectiveVisibility::from_vis(default_vis()));
        for l in Level::all_levels() {
            if l <= level {
                *effective_vis.at_level_mut(l) = Visibility::Public;
            }
        }
        self.map.insert(id, effective_vis);
    }
}

impl<Id: Eq + std::hash::Hash> EffectiveVisibilities<Id> {
    pub fn effective_vis(&self, id: Id) -> Option<&EffectiveVisibility> {
        self.map.get(&id)
    }

    // `parent_id` is not necessarily a parent in source code tree,
    // it is the node from which the maximum effective visibility is inherited.
    pub fn update(
        &mut self,
        id: Id,
        nominal_vis: Visibility,
        default_vis: impl FnOnce() -> Visibility,
        parent_eff_vis: Option<&EffectiveVisibility>,
        level: Level,
        tree: impl DefIdTree,
    ) -> bool {
        let mut changed = false;
        let mut current_effective_vis = self
            .map
            .get(&id)
            .copied()
            .unwrap_or_else(|| EffectiveVisibility::from_vis(default_vis()));
        if let Some(inherited_effective_vis) = parent_eff_vis {
            let mut inherited_effective_vis_at_prev_level =
                *inherited_effective_vis.at_level(level);
            let mut calculated_effective_vis = inherited_effective_vis_at_prev_level;
            for l in Level::all_levels() {
                if level >= l {
                    let inherited_effective_vis_at_level = *inherited_effective_vis.at_level(l);
                    let current_effective_vis_at_level = current_effective_vis.at_level_mut(l);
                    // effective visibility for id shouldn't be recalculated if
                    // inherited from parent_id effective visibility isn't changed at next level
                    if !(inherited_effective_vis_at_prev_level == inherited_effective_vis_at_level
                        && level != l)
                    {
                        calculated_effective_vis =
                            if nominal_vis.is_at_least(inherited_effective_vis_at_level, tree) {
                                inherited_effective_vis_at_level
                            } else {
                                nominal_vis
                            };
                    }
                    // effective visibility can't be decreased at next update call for the
                    // same id
                    if *current_effective_vis_at_level != calculated_effective_vis
                        && calculated_effective_vis
                            .is_at_least(*current_effective_vis_at_level, tree)
                    {
                        changed = true;
                        *current_effective_vis_at_level = calculated_effective_vis;
                    }
                    inherited_effective_vis_at_prev_level = inherited_effective_vis_at_level;
                }
            }
        }
        self.map.insert(id, current_effective_vis);
        changed
    }
}

impl EffectiveVisibilities {
    pub fn debug_string(&self, tcx: TyCtxt<'_>, def_id: LocalDefId) -> String {
        let mut res = String::new();
        if let Some(effective_vis) = self.map.get(&def_id) {
            for level in Level::all_levels() {
                if level != Level::Direct {
                    res.push_str(", ");
                }
                let ev = effective_vis.at_level(level);
                let vis_str = match ev {
                    Visibility::Restricted(restricted_id) => {
                        if restricted_id.is_top_level_module() {
                            "pub(crate)".to_string()
                        } else if *restricted_id == tcx.parent_module_from_def_id(def_id) {
                            "pub(self)".to_string()
                        } else {
                            format!("pub({})", tcx.item_name(restricted_id.to_def_id()))
                        }
                    }
                    Visibility::Public => "pub".to_string(),
                };
                res.push_str(&format!("{:?}: {}", level, vis_str));
            }
            res.push_str(&format!(" / {:?}", effective_vis.parent_id));
        } else {
            res.push_str("not in the table");
        }
        res
    }

    pub fn check_invariants(&self, tcx: TyCtxt<'_>) {
        // if !cfg!(debug_assertions) {
        //     return;
        // }
        for (&def_id, ev) in &self.map {
            let span = tcx.def_span(def_id.to_def_id());
            // More direct visibility levels can never go farther than less direct ones.
            if !ev.reexported.is_at_least(ev.direct, tcx) {
                span_bug!(span, "direct {:?} > reexported {:?}", ev.direct, ev.reexported);
            }
            if !ev.reachable.is_at_least(ev.reexported, tcx) {
                span_bug!(span, "reexported {:?} > reachable {:?}", ev.reexported, ev.reachable);
            }
            if !ev.reachable_through_impl_trait.is_at_least(ev.reachable, tcx) {
                span_bug!(
                    span,
                    "reachable {:?} > reachable_through_impl_trait {:?}",
                    ev.reachable,
                    ev.reachable_through_impl_trait
                );
            }
            // Fully private items are never put into the table, this is important for performance.
            if ev.direct == ev.reachable_through_impl_trait
                && ev.direct == Visibility::Restricted(tcx.parent_module_from_def_id(def_id))
                // FIXME: Fully private `mod` items are currently put into the table.
                && tcx.def_kind(def_id) != DefKind::Mod
            {
                span_bug!(
                    span,
                    "fully private item in the table {:?}: {:?} / {:?}",
                    def_id,
                    ev.direct,
                    ev.parent_id
                );
            }
        }
    }
}

impl<'a> HashStable<StableHashingContext<'a>> for EffectiveVisibilities {
    fn hash_stable(&self, hcx: &mut StableHashingContext<'a>, hasher: &mut StableHasher) {
        let EffectiveVisibilities { ref map } = *self;
        map.hash_stable(hcx, hasher);
    }
}
