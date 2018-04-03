use crate::spec::{PanicStrategy, TargetOptions};

pub fn opts() -> TargetOptions {
    TargetOptions {
        os: "l4re".to_string(),
        env: "uclibc".to_string(),
        executables: true,
        panic_strategy: PanicStrategy::Abort,
        linker: Some("l4-bender".to_string()),
        linker_is_gnu: false,
        families: vec!["unix".to_string()],
        limit_rdylib_exports: false,
        ..Default::default()
    }
}
