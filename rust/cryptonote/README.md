# Cryptonote

Cryptonote is a cross-platform, fully offline application for creating, storing, and sharing encrypted notes. It is completely serverless and runs entirely on your device or in your web browser - no internet connection or external services are required.

With Cryptonote, you can:

- Write a short plain-text note
- Optionally encrypt it using strong, well-established symmetric (e.g., AES-GCM, ChaCha20-Poly1305) or asymmetric algorithms (e.g., X25519 + AES-GCM hybrid)
- Or leave it unencrypted
- Share the note instantly via a URL or scannable QR code

All content, including the ciphertext or plaintext, is embedded directly in the URL itself, making sharing as simple as sending a link or showing a QR code.

Cryptonote follows modern cryptographic best practices:

- Strong key derivation using HKDF
- Authenticated encryption ensuring confidentiality, integrity, and authenticity
- No data ever leaves your device unless you explicitly share it

Secure, private, and truly offline - your notes stay yours.
