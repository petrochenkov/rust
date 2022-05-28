use crate::spec::{CoarseGrainedLinkerFlavor, TargetOptions};

pub fn opts() -> TargetOptions {
    let mut opts = super::windows_msvc_base::opts();

    opts.abi = "uwp".into();
    opts.vendor = "uwp".into();
    let pre_link_args_msvc = vec!["/APPCONTAINER".into(), "mincore.lib".into()];
    opts.pre_link_args
        .entry(CoarseGrainedLinkerFlavor::TargetLinker)
        .or_default()
        .extend(pre_link_args_msvc);

    opts
}
