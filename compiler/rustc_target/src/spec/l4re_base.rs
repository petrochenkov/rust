use crate::spec::{cvs, PanicStrategy, TargetOptions};

pub fn opts() -> TargetOptions {
    TargetOptions {
        os: "l4re".into(),
        env: "uclibc".into(),
        executables: true,
        panic_strategy: PanicStrategy::Abort,
        linker: Some("l4-bender".into()),
        linker_is_gnu: false,
        families: cvs!["unix"],
        ..Default::default()
    }
}
