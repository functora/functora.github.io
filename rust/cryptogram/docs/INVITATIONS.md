# Invitation format

An invitation is an application URL whose fragment contains exactly two values:

```text
https://<mini-app-host>/#channel_id=<channel-id>&invite_secret=<secret>
```

The fragment is parsed by the client, not by the relay.

## Fields

- `channel_id`: 16 random bytes encoded with unpadded canonical base64url. The encoded length is 22 characters.
- `invite_secret`: 32 random bytes encoded with unpadded canonical base64url.

The parser rejects:

- missing, duplicate, or unknown fields;
- an empty field or an invalid `=` separator;
- padded or non-canonical base64url;
- a channel ID with the wrong decoded length;
- an invitation secret with the wrong decoded length.

## Transport

Only `channel_id` is sent in `relayPut` and `relayGet` requests. The invitation secret is used locally to derive the outer ChaCha20-Poly1305 key with HKDF-SHA256. The channel ID is also included in the AEAD associated data, preventing a ciphertext from being moved to another channel without authentication failure.

The complete fragment is bearer invitation material. Anyone who obtains it can derive the outer transport key and join the intended conversation. Transfer it through an authentic channel and create a new conversation if it is exposed.

The relay cannot recover the secret from a request because the secret is never included in the request body.
