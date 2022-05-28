use crate::spec::{CoarseGrainedLinkerFlavor, LinkArgs, PanicStrategy, TargetOptions, TlsModel};

pub fn opts() -> TargetOptions {
    let mut pre_link_args = LinkArgs::new();
    pre_link_args.insert(
        CoarseGrainedLinkerFlavor::TargetLinker,
        vec!["--build-id".into(), "--hash-style=gnu".into(), "--Bstatic".into()],
    );

    TargetOptions {
        os: "hermit".into(),
        linker: Some("rust-lld".into()),
        executables: true,
        has_thread_local: true,
        pre_link_args,
        panic_strategy: PanicStrategy::Abort,
        position_independent_executables: true,
        static_position_independent_executables: true,
        tls_model: TlsModel::InitialExec,
        ..Default::default()
    }
}
