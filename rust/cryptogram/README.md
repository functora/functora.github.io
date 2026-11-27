# Cryptogram

Cryptogram is a Rust/WASM Telegram Mini App for end-to-end encrypted group conversations. It uses the local `functora-egui` white-label shell, OpenMLS (RFC 9420) group messaging, and a small Telegram Serverless relay.

The relay is intentionally not a chat server. It stores and returns opaque ciphertext for a channel. It never receives plaintext, Telegram identities, room names, participant lists, message kinds, private keys, safety numbers, or readable MLS structures.

## Architecture

- The Rust client owns accounts, pseudonymous identities, MLS state, membership, chat history, replay protection, ordering, and local persistence.
- OpenMLS encrypts application messages and authenticates membership changes inside each group.
- Every relay frame is additionally sealed with ChaCha20-Poly1305. The outer key is derived with HKDF-SHA256 from the invitation secret and is bound to the channel ID through the AEAD associated data.
- The relay exposes exactly `relayPut` and `relayGet`. Its records contain only `channel`, `payload`, and a per-channel `sequence`.
- Invitations carry a random channel ID and a random invitation secret in the URL fragment. The fragment is not sent to the relay.
- Telegram SecureStorage holds only the small vault encryption key when it is available. The encrypted vault is split into bounded local-storage chunks; if SecureStorage is unavailable, the key also falls back to local storage.

A GitHub Pages-only deployment is not sufficient for multi-user messaging. The Mini App must be hosted where Telegram Serverless calls are available, with the relay deployed alongside it.

## Repository layout

- `src/`: Rust client, protocol, MLS, relay, vault, and white-label UI.
- `tgcloud/`: minimal Telegram Serverless relay and its contract tests.
- `assets/`: web application shell, Telegram Web App SDK, CSP, and launcher icons.
- `tests/`: integration tests for MLS exchange, relay behavior, and persistence.
- `vendor/`: pinned OpenMLS crypto-provider sources used to replace vulnerable published dependency versions.
- `desktop/`: generated Linux desktop metadata.
- `android/`: generated Android project (ignored by Git and recreated by the build).

## OpenMLS crypto backend

The published OpenMLS `0.6.0` provider crates resolve to vulnerable HPKE,
libcrux, and RustCrypto dependency versions. Cryptogram patches them with
source from OpenMLS commit
[`e0b21a70a55a9014b8de2772b36bf3d638adad51`](https://github.com/openmls/openmls/commit/e0b21a70a55a9014b8de2772b36bf3d638adad51),
which migrates the cryptographic backend without changing MLS serialization or
credential encodings. The vendored crates retain the upstream MIT license. See
`vendor/README.md` for the exact source paths and local manifest adaptations.

## Local development

The repository's `rust/flake.nix` is the source of truth for the toolchain. Enter the development shell before running Cargo commands:

```sh
cd rust
nix develop
cd cryptogram
```

Useful commands from `rust/cryptogram`:

```sh
cargo check --all-features --all-targets
cargo clippy --all-features --all-targets -- -D warnings
cargo test --all-features --all-targets
cargo fmt
```

The relay contract tests use Node's built-in test runner and a small SDK stub:

```sh
node --experimental-loader ./tests/loader.mjs --test ./tests/relay.test.mjs
```

The same checks and the release WASM package are available through the flake. Run these commands from `rust/`:

```sh
nix run .#relay-test
nix run .#cryptogram-wasm-check
nix build .#cryptogram-wasm
```

`cryptogram-wasm-check` builds with the pinned Rust and `wasm-bindgen` versions, runs `wasm-opt`, and validates the generated package. The `cryptogram-wasm` build output contains the reusable `pkg/` directory.

### Telegram Serverless CLI

The flake pins the official `@tgcloud/cli` package at version `0.2.0` and exposes it as `tgcloud` in the development shell. The command is also available without entering a shell:

```sh
nix develop
tgcloud --version
tgcloud --help
```

```sh
nix run .#tgcloud -- --version
```

Use this command for `login`, `status`, `diff`, `push`, `migrate`, and `run`. Do not use `npx tgcloud`: without a local scoped-package installation, it resolves the unrelated unscoped `tgcloud` package.

## Web and Telegram setup

1. Build the WASM library with the pinned Nix toolchain.
2. Run `wasm-bindgen` for the `wasm32-unknown-unknown` release artifact.
3. Copy `assets/index.html`, `assets/manifest.webmanifest`, `assets/egui.js`, `assets/cryptogram.js`, `assets/version.js`, `assets/telegram-web-app.js`, and the favicon assets next to the generated `pkg/` directory. The release helper regenerates `version.js` from `Cargo.toml` so the service-worker cache matches the packaged application version.
4. Serve the Mini App from a Telegram Serverless-compatible host. The `assets/csp.txt` file records the policy embedded in the generated `index.html`.
5. Deploy the files in `tgcloud/` as a Telegram Serverless project. The relay schema must be applied before calling the endpoints.

The `build.rs` script regenerates the standard `functora-egui` web files during a WASM build and then replaces the generated inline module with the Telegram-aware external module. The checked-in web files are useful for local inspection and for packaging tools.

## Relay contract

`relayPut` accepts exactly:

```json
{"channel":"<22-character-base64url-channel>","payload":"<opaque-base64url-payload>"}
```

It returns:

```json
{"sequence":1}
```

`relayGet` accepts exactly:

```json
{"channel":"<22-character-base64url-channel>","sequence":0}
```

It returns a `messages` array whose entries contain only `sequence` and `payload`. The relay ignores Telegram initialization data and does not log request bodies. See `tgcloud/README.md` for validation and deployment details.

## Invitations

An invitation is a normal application URL with this fragment:

```text
#channel_id=<channel-id>&invite_secret=<secret>
```

The parser accepts exactly those two fields, rejects duplicates and unknown fields, requires canonical unpadded base64url, and requires a 16-byte channel ID and a 32-byte invitation secret. The secret is bearer material: anyone who obtains the complete fragment can derive the outer transport key. The channel ID alone is not sufficient to decrypt payloads.

## Limitations

- Cryptogram does not provide identity authentication, safety-number verification, key transparency, or a recovery mechanism for a lost local vault.
- The relay can observe channel IDs, payload sizes, write timing, read timing, and per-channel sequence numbers. It cannot read the outer ciphertext.
- A compromised Telegram client, browser, device, Telegram account, or relay operator can observe data outside Cryptogram's control. The relay is not an anonymity service.
- The outer transport key is derived from a static invitation secret. The invitation must be transferred through an authentic channel and rotated by creating a new conversation when compromise is suspected.
- Denial-of-service, traffic analysis, malicious clients, and replay attempts are possible at the relay boundary. The client rejects invalid frames, non-increasing sequences, oversized payloads, and duplicate join-request, commit, and application-frame digests.
- OpenMLS and RustCrypto provide the cryptographic implementation. This project does not claim formal verification of the complete application or the Telegram platform.

See `SECURITY.md` for the threat model and the Invitations section above for the invitation format.
