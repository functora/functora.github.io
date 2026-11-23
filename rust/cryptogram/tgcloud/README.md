# Telegram Serverless relay

This directory contains the minimal relay used by Cryptogram. It is intentionally a ciphertext transport, not a chat backend.

## Contract

There are exactly two endpoints:

- `relayPut({ channel, payload })` stores one opaque payload and returns its per-channel `sequence`.
- `relayGet({ channel, sequence })` returns messages with a sequence greater than the cursor.

Requests must contain exactly the listed fields. Additional fields are rejected before database access. The relay validates the 22-character channel ID, canonical unpadded base64url payloads, safe integer cursors, and the following limits:

- 96 KiB maximum decoded payload;
- 128 KiB maximum encoded payload;
- 64 messages and 128 KiB aggregate encoded payload bytes per read.

The database schema stores only `channel`, `sequence`, and `payload`. The primary key is the pair `(channel, sequence)`. No table contains plaintext, identity, room, membership, or MLS metadata.

## Deployment

1. Create a Telegram Serverless project that can import the files in this directory.
2. Apply `schema.js` before exposing the endpoints.
3. Publish `endpoints/relayPut.js` and `endpoints/relayGet.js` as the two callable endpoints.
4. Configure the Mini App host and Telegram Bot integration separately.
5. Do not add request-body logging, analytics, or user-facing identifiers to the relay.

The relay does not inspect `ctx.initData`. It does not use Telegram identity data and does not need to validate it to provide the opaque transport contract. A deployment may add platform-level access controls, but those controls must not cause the relay to store plaintext or MLS structures.

## Contract tests

The tests use a small local SDK stub and do not require a deployed service:

```sh
node --experimental-loader ./tests/loader.mjs --test ./tests/relay.test.mjs
```

The test suite covers the exact endpoint set, concurrent sequence assignment, strict request validation, cursor ordering, and response limits.
