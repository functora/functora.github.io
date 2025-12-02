# Cryptonote

Cryptonote is a cross-platform, fully offline application for creating, storing, and sharing encrypted notes. It is completely serverless and runs entirely on your device or in your web browser - no internet connection or external services are required.

With Cryptonote you can:

- Write a short plain-text note
- Optionally encrypt it using strong, well-established algorithms:
  - Symmetric (e.g., AES-GCM, ChaCha20-Poly1305)
  - Asymmetric (e.g., X25519 + AES-GCM hybrid encryption)
- Or leave it unencrypted
- Share the note instantly via a URL or a scannable QR code

All content - whether ciphertext or plaintext - is embedded directly in the URL itself, making sharing as simple as sending a link or displaying a QR code.

Cryptonote follows modern cryptographic best practices:

- Strong key derivation with HKDF, allowing users to supply just a password (which is used directly as the initial keying material)
- Authenticated encryption for confidentiality, integrity, and authenticity
- No data ever leaves your device unless you explicitly choose to share it

Secure, private, and truly offline - your notes remain yours alone.

The Cryptonote interface is fully internationalized and currently supports English, Spanish, and Russian.
