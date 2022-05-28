use super::super::*;

// Test target self-consistency and JSON encoding/decoding roundtrip.
pub(super) fn test_target(target: Target) {
    target.check_consistency();
    assert_eq!(Target::from_json(target.to_json()).map(|(j, _)| j), Ok(target));
}

impl Target {
    fn check_consistency(&self) {
        assert!(self.is_like_windows || !self.is_like_msvc);
        assert!(
            (self.pre_link_objects_fallback.is_empty()
                && self.post_link_objects_fallback.is_empty())
                || self.crt_objects_fallback.is_some()
        );
        // Keep the default "unknown" vendor instead.
        assert_ne!(self.vendor, "");
        if !self.can_use_os_unknown() {
            // Keep the default "none" for bare metal targets instead.
            assert_ne!(self.os, "unknown");
        }
    }

    // Add your target to the whitelist if it has `std` library
    // and you certainly want "unknown" for the OS name.
    fn can_use_os_unknown(&self) -> bool {
        self.llvm_target == "wasm32-unknown-unknown"
            || self.llvm_target == "wasm64-unknown-unknown"
            || (self.env == "sgx" && self.vendor == "fortanix")
    }
}
