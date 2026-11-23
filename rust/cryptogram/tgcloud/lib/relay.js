import { db, EndpointError } from 'sdk';
import { and, asc, eq, gt } from 'sdk/db';

import { relayMessages } from '../schema.js';

export const CHANNEL_ID_LENGTH = 22;
export const CHANNEL_ID_BYTES = 16;
export const MAX_PAYLOAD_BYTES = 96 * 1024;
export const MAX_PAYLOAD_ENCODED_LENGTH = 128 * 1024;
export const MAX_GET_MESSAGES = 64;
export const MAX_GET_TOTAL_BYTES = 128 * 1024;
export const MAX_WRITE_ATTEMPTS = 8;

const BASE64URL_PATTERN = /^[A-Za-z0-9_-]+$/;
const INSERT_MESSAGE_SQL = `
  INSERT INTO relay_messages (channel, sequence, payload)
  SELECT :channel, COALESCE(MAX(sequence), 0) + 1, :payload
  FROM relay_messages
  WHERE channel = :channel
  RETURNING sequence
`;

const relayError = (code, description) => new EndpointError(description, { code });

const isSequenceCollision = (error) => {
  const code = String(error?.code ?? '');
  const message = String(error?.message ?? error ?? '');
  return code.includes('CONSTRAINT')
    || code === '1555'
    || /unique constraint|primary key|duplicate key|constraint failed/i.test(message);
};

const base64UrlValue = (character) => {
  const code = character.charCodeAt(0);
  if (code >= 65 && code <= 90) {
    return code - 65;
  }
  if (code >= 97 && code <= 122) {
    return code - 71;
  }
  if (code >= 48 && code <= 57) {
    return code + 4;
  }
  if (code === 45) {
    return 62;
  }
  if (code === 95) {
    return 63;
  }
  return -1;
};

const decodedBase64UrlLength = (value) => {
  const remainder = value.length % 4;
  if (remainder === 1) {
    return null;
  }
  return Math.floor((value.length * 3) / 4);
};

const isCanonicalBase64Url = (value) => {
  if (!BASE64URL_PATTERN.test(value)) {
    return false;
  }
  const remainder = value.length % 4;
  if (remainder === 1) {
    return false;
  }
  const lastValue = base64UrlValue(value.charAt(value.length - 1));
  const unusedBits = remainder === 2 ? 0x0f : 0x03;
  return (lastValue & unusedBits) === 0;
};

const validateChannel = (channel) => {
  if (
    typeof channel !== 'string'
    || channel.length !== CHANNEL_ID_LENGTH
    || !isCanonicalBase64Url(channel)
    || decodedBase64UrlLength(channel) !== CHANNEL_ID_BYTES
  ) {
    throw relayError('INVALID_CHANNEL', 'Channel ID is invalid');
  }
  return channel;
};

const isValidPayload = (payload) => {
  if (
    typeof payload !== 'string'
    || payload.length === 0
    || payload.length > MAX_PAYLOAD_ENCODED_LENGTH
    || !isCanonicalBase64Url(payload)
  ) {
    return false;
  }
  const byteLength = decodedBase64UrlLength(payload);
  return byteLength !== null && byteLength > 0 && byteLength <= MAX_PAYLOAD_BYTES;
};

const validatePayload = (payload) => {
  if (!isValidPayload(payload)) {
    throw relayError('INVALID_PAYLOAD', 'Payload is invalid');
  }
  return payload;
};

const validateSequence = (sequence) => {
  if (typeof sequence !== 'number' || !Number.isSafeInteger(sequence) || sequence < 0) {
    throw relayError('INVALID_SEQUENCE', 'Sequence is invalid');
  }
  return sequence === 0 ? 0 : sequence;
};

const hasExactKeys = (input, expectedKeys) => {
  if (input === null || typeof input !== 'object' || Array.isArray(input)) {
    return false;
  }
  const actualKeys = Object.keys(input);
  return actualKeys.length === expectedKeys.length
    && expectedKeys.every((key) => Object.prototype.hasOwnProperty.call(input, key));
};

export const parseRelayPutInput = (input) => {
  if (!hasExactKeys(input, ['channel', 'payload'])) {
    throw relayError('INVALID_REQUEST', 'Relay request is invalid');
  }
  return {
    channel: validateChannel(input.channel),
    payload: validatePayload(input.payload),
  };
};

export const parseRelayGetInput = (input) => {
  if (!hasExactKeys(input, ['channel', 'sequence'])) {
    throw relayError('INVALID_REQUEST', 'Relay request is invalid');
  }
  return {
    channel: validateChannel(input.channel),
    sequence: validateSequence(input.sequence),
  };
};

export const putRelayMessage = async (channel, payload) => {
  const validChannel = validateChannel(channel);
  const validPayload = validatePayload(payload);
  for (let attempt = 0; attempt < MAX_WRITE_ATTEMPTS; attempt += 1) {
    try {
      const result = await db.run(INSERT_MESSAGE_SQL, {
        ':channel': validChannel,
        ':payload': validPayload,
      });
      const sequence = result.rows?.[0]?.sequence;
      if (!Number.isSafeInteger(sequence) || sequence < 1) {
        throw new Error('Relay write did not return a sequence');
      }
      return { sequence };
    } catch (error) {
      if (!isSequenceCollision(error) || attempt + 1 === MAX_WRITE_ATTEMPTS) {
        throw relayError('RELAY_WRITE_ERROR', 'Relay write failed');
      }
    }
  }
  throw relayError('RELAY_WRITE_ERROR', 'Relay write failed');
};

export const getRelayMessages = async (channel, sequence) => {
  const validChannel = validateChannel(channel);
  const validSequence = validateSequence(sequence);
  let rows;
  try {
    rows = await db
      .select({ sequence: relayMessages.sequence, payload: relayMessages.payload })
      .from(relayMessages)
      .where(and(
        eq(relayMessages.channel, validChannel),
        gt(relayMessages.sequence, validSequence),
      ))
      .orderBy(asc(relayMessages.sequence))
      .limit(MAX_GET_MESSAGES)
      .all();
  } catch {
    throw relayError('RELAY_READ_ERROR', 'Relay read failed');
  }
  if (!Array.isArray(rows)) {
    throw relayError('RELAY_READ_ERROR', 'Relay read failed');
  }

  const messages = [];
  let totalBytes = 0;
  let previousSequence = validSequence;
  for (const row of rows.slice(0, MAX_GET_MESSAGES)) {
    if (
      !row
      || !Number.isSafeInteger(row.sequence)
      || row.sequence <= previousSequence
      || typeof row.payload !== 'string'
      || !isValidPayload(row.payload)
    ) {
      throw relayError('RELAY_READ_ERROR', 'Relay read failed');
    }
    if (row.payload.length > MAX_GET_TOTAL_BYTES - totalBytes) {
      break;
    }
    totalBytes += row.payload.length;
    previousSequence = row.sequence;
    messages.push({ sequence: row.sequence, payload: row.payload });
  }
  return { messages };
};
