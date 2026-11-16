# Security Policy

Cryptonote is a note-sharing application. This document states what it
protects, what it deliberately does not protect, and which security issues
were found and fixed.

## Reporting a vulnerability

Please report suspected vulnerabilities privately to **functora@proton.me**.
Do not open a public issue for a working exploit. Include the crate version,
the platform, the steps to reproduce, and the impact you observed. You can
expect an acknowledgement within a week.

## Assets

| Asset | Where it lives | Exposure |
| --- | --- | --- |
| Note body | `TemporaryState.note` in memory, rendered into the UI | Copied to the clipboard, exported as an archive, or rendered on screen |
| Password | `TemporaryState.password` in memory, passed to the KDF | Never written to the URL, the archive, the log, or the disk |
| Derived key | `Zeroizing<Vec<u8>>` inside the crypto layer | Wiped on drop |
| Ciphertext | URL query string, `.cryptonote` archive | Public by design once shared |
| Note metadata | URL query string, archive `metadata.json` | Public by design once shared |

## Trust boundaries

1. **The device.** Cryptonote trusts the operating system it runs on. A
   compromised OS, a malicious keyboard, or a screen-sharing session can read
   the note and the password.
2. **Everyone who gets the link.** A shared link is the note. Whoever receives
   it - by mail, chat, a screenshot, or a QR code reader - can open it. In
   Plain mode they read the text directly. In encrypted mode they get the
   ciphertext, and the password is the only thing in the way.
3. **The web page address bar.** In the web build the note rides inside the
   URL, so it is part of the address. Anything that handles URLs - the
   browser history, the network path the page was fetched over - can see it.
   The native builds have no such address.
4. **The archive.** A `.cryptonote` file is untrusted input. Every field in
   it, including the entry names and the declared sizes, is attacker
   controlled.

## Threat model

**Adversaries.**

- *Network observer.* Sees the link, not the plaintext.
- *Link holder.* Has the ciphertext and the salt and can guess passwords
  offline, without a rate limit.
- *Anyone who reads the URL.* The whole note payload sits in the address, so
  everything that handles the address sees the ciphertext - or, in Plain mode,
  the note body.
- *Malicious archive author.* Crafts a `.cryptonote` file to exhaust memory,
  crash the app, or escape the download directory.

**Out of scope.**

- A compromised device, browser extension, or operating system.
- Traffic analysis and link-pattern correlation.
- Malicious recipients who receive a note you chose to share with them.
- Cryptanalysis of AES-GCM, ChaCha20-Poly1305, or Argon2id.

## What Cryptonote guarantees

- **Confidentiality and tamper detection for encrypted notes.** AES-256-GCM
  or ChaCha20-Poly1305 with a 256-bit key, a random 96-bit nonce, and a
  random 32-byte salt per note.
- **Password-based key derivation.** Argon2id with a compile-time cost of
  64 MiB, 3 iterations, and parallelism 1. The cost is a constant in the
  library: no environment variable, build flag, or config file can lower it.
- **Authenticated metadata.** The cipher, the KDF, and the application prefix
  are bound as AEAD associated data, and the archive metadata records the KDF
  that was actually used.
- **The metadata cannot choose the branch.** Every archive payload starts with
  a 16-byte frame that names the branch it was written in. The frame is bound
  as AEAD associated data in the encrypted branch, and metadata that disagrees
  with the frame is refused instead of switching the branch.
- **An empty ciphertext is a decryption failure.** Every ciphertext, including
  the empty plaintext, carries an authentication tag that must verify.
- **Bounded archive parsing.** At most 4096 entries, 256 MiB per entry, and
  512 MiB in total. Declared sizes are never trusted for allocation.
- **An encrypted archive cannot be opened without a password.** An empty
  password is refused outright.
- **Safe attachment names.** Archive entry names are reduced to a single path
  component with no separators and no leading dots before they reach the UI
  or the file system.
- **No accidental password leak.** The password is never part of a URL, an
  archive, a log line, or a `Debug` representation. Neither are the note body
  or the attachment contents.

## Explicit non-goals and accepted risks

These are properties the application does not have. They are design choices,
not bugs.

1. **No sender authenticity.** AEAD detects tampering, not a forged author.
   Anyone who knows the password can produce a note that decrypts cleanly.
   Cryptonote does not sign notes and cannot tell you who wrote one.
2. **No forward secrecy.** The key is derived from the password alone. If the
   password is later disclosed, every note shared with it becomes readable.
3. **No forward secrecy against a recorded ciphertext.** A shared link exposes
   the ciphertext, the salt, and the nonce, so the password can be attacked
   offline at the full Argon2id cost.
4. **Plain mode is not encryption.** A Plain note puts the body in the URL as
   base64url, which is encoding, not encryption. Everyone who gets the link
   can read the text. Cryptonote warns before sharing.
5. **Weak passwords are warned about, not rejected.** A password shorter than
   12 characters triggers a non-blocking warning; the share still proceeds.
   Cryptonote never takes away the user's choice, but the warning is the
   only mitigation the application can offer against offline cracking.
6. **The clipboard is a leak.** Copying the decrypted note puts it where other
   applications can read it. Cryptonote warns when you do.
7. **The unencrypted export is a leak.** `cryptonote-unlocked.zip` contains the
   plain attachments. Cryptonote warns when you export it.
8. **The URL is the transport.** The note payload is in the link itself, so
   everyone the link reaches can read the note: the browser that opens it,
   the chat app that carries it, the person it is forwarded to. This is a
   deliberate trade-off for link-only sharing and is not going to change.
9. **The web build is not offline.** It is a web page, so it needs a
   connection to load. The native builds do not need a network connection.

## Cryptographic parameters

| Parameter | Value |
| --- | --- |
| Ciphers | AES-256-GCM, ChaCha20-Poly1305 |
| KDF | Argon2id, v1.3 |
| Argon2 memory | 65536 KiB (64 MiB) |
| Argon2 iterations | 3 |
| Argon2 parallelism | 1 |
| Key length | 32 bytes |
| Salt length | 32 bytes, random per note |
| Nonce length | 12 bytes, random per note |
| Stream chunk | 64 KiB |
| Stream tag | 16 bytes |
| Archive payload frame | 16 bytes, names the branch |
| Archive budgets | 4096 entries, 256 MiB per entry, 512 MiB total |

### Archive format

A `.cryptonote` archive holds two entries: `metadata.json` and `payload.cpt`.
The payload opens with a 16-byte frame that names the branch it was written
in: `cryptonote-plain` or `cryptonote-encry`. The frame is part of the AEAD
associated data of the encrypted branch, so the ciphertext is authenticated
together with it, and an edited `metadata.json` cannot move an archive into
the other branch. Archives written before the frame was introduced are not
readable by this version.

## Known accepted dependency warnings

`cargo audit` reports a small number of *unmaintained* advisories for
transitive dependencies with no safe upgrade path: `basic-toml` (through
`askama`), `ttf-parser` (through `ab_glyph`), and `zbus_names`, `zvariant`,
and `zvariant_derive` (through the Linux desktop portal stack). None of them
is a vulnerability, and none of them ships in the Cryptonote binary path that
handles note content.

## Verification

From the `rust/` directory:

```sh
nix develop -c audit
```

That runs `cargo audit` and `cargo deny check` over every crate that has a
`Cargo.lock`, using the policy in `rust/deny.toml`. The guarantees above are
covered by the regression tests in the `tests/` directory of the crate that
owns them:

- `functora-core/tests/security_tests.rs` and `functora-core/tests/kdf_policy_tests.rs`
  cover the key derivation, the ciphertext and the archive format.
- `functora-core/tests/files_tests.rs` covers the redaction of the file
  contents.
- `cryptonote/tests/security_crypto_tests.rs` covers the contents of the
  shared URL and of the archive payload.
- `cryptonote/tests/security_archive_tests.rs` covers the archive branch and
  the attachment names.
- `cryptonote/tests/security_warning_tests.rs` covers the password, the
  clipboard and the export advisories.
- `cryptonote/tests/security_screen_tests.rs` covers the Security screen.
- `cryptonote/tests/markdown_tests.rs` keeps the three policy documents in
  sync.

## Scope of this document

This document describes the `cryptonote` application and the parts of
`functora-core` and `functora-egui` that it relies on. It does not describe
the other crates in the monorepo, which are not part of the note-handling
path.
