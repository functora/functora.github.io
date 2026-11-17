# functora-core

Pure Rust core logic shared by the Functora applications and libraries.

The crate provides:

- Password-based encryption with Argon2id, AES-256-GCM, and ChaCha20-Poly1305
- Bounded ZIP archive creation and extraction
- Base64 and URL-safe encoding helpers
- Internationalization and Markdown rendering
- QR-code, image, and video thumbnail support

## Usage

```toml
[dependencies]
functora-core = "0.4.0"
```

## Security

Authenticated encryption detects modification of protected data, but it does not prove authorship. Password-based encryption also does not provide forward secrecy, and a weak password can be attacked offline.

## License

MIT
