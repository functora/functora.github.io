# Vendored OpenMLS crypto crates

These crates are local compatibility patches for the published OpenMLS `0.6.0`
crypto-provider crates. Their source files are taken unmodified from the
OpenMLS repository at commit
[`e0b21a70a55a9014b8de2772b36bf3d638adad51`](https://github.com/openmls/openmls/commit/e0b21a70a55a9014b8de2772b36bf3d638adad51):

- `openmls_rust_crypto/` comes from `openmls_rust_crypto/` in that commit.
- `openmls_basic_credential/` comes from `basic_credential/` in that commit.

The published provider versions still depend on HPKE `0.7`, `libcrux-kem`
`0.0.9`, and the RustCrypto `0.13` elliptic-curve stack. The pinned upstream
revision migrates those dependencies to HPKE `0.8`, RustCrypto `0.14`, and
`crypto-bigint 0.7`, removing the vulnerable versions without changing MLS
serialization or credential encodings.

Only the package manifests are adapted for local use:

- Workspace inheritance is replaced by the corresponding published crates.io
  dependency versions.
- The basic-credential package does not forward the unused post-quantum
  ciphersuite feature to other crypto providers.

Each vendored crate includes the upstream MIT license. `LICENSE.openmls` is the
same upstream license at the vendor root.

When updating these sources, replace them from a reviewed upstream commit,
reapply and document the manifest adaptations, regenerate `Cargo.lock`, and
run both `cargo-audit` and `cargo-deny` before accepting the update.
