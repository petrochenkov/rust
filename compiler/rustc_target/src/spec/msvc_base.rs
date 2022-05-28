use crate::spec::{CoarseGrainedLinkerFlavor, LinkArgs, SplitDebuginfo, TargetOptions};

pub fn opts() -> TargetOptions {
    let pre_link_args_msvc = vec![
        // Suppress the verbose logo and authorship debugging output, which would needlessly
        // clog any log files.
        "/NOLOGO".into(),
    ];
    let mut pre_link_args = LinkArgs::new();
    pre_link_args.insert(CoarseGrainedLinkerFlavor::TargetLinker, pre_link_args_msvc);

    TargetOptions {
        executables: true,
        is_like_windows: true,
        is_like_msvc: true,
        linker_is_gnu: false,
        pre_link_args,
        abi_return_struct_as_int: true,
        emit_debug_gdb_scripts: false,

        // Currently this is the only supported method of debuginfo on MSVC
        // where `*.pdb` files show up next to the final artifact.
        split_debuginfo: SplitDebuginfo::Packed,

        ..Default::default()
    }
}
