# Security model

## Status

Cryptogram is an experimental client and relay. The implementation is not a formal security proof. The findings listed here are the boundaries that users and operators must understand before relying on it.

## Assets

- MLS group keys, epoch secrets, private signing keys, and key packages.
- The pseudonymous local identity and its credential.
- Application message plaintext and local chat history.
- The invitation secret and the outer transport key derived from it.
- The encrypted local vault and its encryption key.
- Room membership and the local replay/ordering state.

## Trust boundaries

1. The device and browser execute the Rust/WASM client and the Telegram Web App SDK. Cryptogram trusts this code path to keep secrets out of logs and untrusted storage.
2. Telegram transports the Mini App and may expose the Telegram Web App bridge. Cryptogram does not treat Telegram as an anonymity or independent audit boundary.
3. Telegram SecureStorage is preferred for the vault encryption key. The encrypted vault itself is split into bounded local-storage chunks. If SecureStorage is unavailable, the key also falls back to local storage and loses the stronger device-key protection offered by Telegram.
4. The Telegram Serverless relay stores opaque payloads. It is trusted to enforce availability, size limits, and sequence uniqueness, but not to preserve confidentiality.
5. Invitation recipients are outside the local trust boundary until the invitation secret has arrived through an authentic channel.

## Adversaries

- A relay operator or database reader who obtains the relay database.
- A network observer who sees the Mini App host and Telegram transport.
- A malicious participant who submits crafted MLS frames or opaque relay payloads.
- An attacker who obtains an invitation URL or local vault storage.
- A compromised browser, operating system, Telegram client, or build environment.

## Guarantees provided by this implementation

- MLS application messages and membership changes use OpenMLS with the selected MLS 128-bit DHKEM/X25519, AES-128-GCM, SHA-256, and Ed25519 ciphersuite.
- Relay payloads use ChaCha20-Poly1305 with a fresh random nonce. The key is derived using HKDF-SHA256 from the invitation secret and the channel ID, and the channel ID is authenticated as AEAD associated data.
- The relay contract accepts and stores only channel, opaque payload, and per-channel sequence values. The relay code does not read Telegram initialization data and does not log request bodies.
- The client validates canonical base64url, frame tags, payload sizes, sequence monotonicity, key packages, and duplicate key-package digests.
- The local vault is encrypted with ChaCha20-Poly1305 before it is passed to the storage layer. The small encryption key is kept in Telegram SecureStorage when available, while the encrypted payload is stored in bounded local chunks.
- Invitation values are carried in the URL fragment. The fragment is not included in relay requests.

## Explicit non-goals

Cryptogram does not provide:

- Sender identity authentication or a verified safety-number ceremony.
- Protection against a malicious or compromised client that shares plaintext after decryption.
- Traffic-analysis resistance, anonymity, metadata minimization, or resistance to channel correlation.
- Forward secrecy for the outer transport key when the invitation secret remains available.
- Secure deletion guarantees for browser, operating-system, swap, crash-dump, or Telegram storage layers.
- Recovery after loss of the local vault or the private MLS state.
- Availability, censorship resistance, or protection against relay flooding.
- A guarantee that the Telegram platform, its SDK, or its Serverless runtime is free of vulnerabilities.

## Operational requirements

- Use a unique invitation secret for every conversation. Transfer it over an authentic channel.
- Do not publish an invitation fragment in a public channel, issue tracker, or log.
- Treat the local device and Telegram account as part of the security boundary.
- Keep the relay's payload and request limits enabled. Do not add plaintext logging or analytics to the relay.
- Rotate a conversation by creating a new one if an invitation secret or local device is suspected compromised.
- Review dependency advisories and rebuild the WASM artifact from the pinned lockfile before releases.
- Keep the local OpenMLS provider patches pinned to the reviewed upstream revision documented in `vendor/README.md`; do not silently replace them with the published crates while vulnerable dependency versions remain in the graph.

## Reporting a vulnerability

Do not disclose a working exploit in a public issue. Report suspected vulnerabilities privately through the repository's private security contact or the maintainers' verified private channel. Include the affected version, platform, reproduction steps, and the impact you observed. Do not include real invitation secrets, private keys, message plaintext, or vault contents in a report.
