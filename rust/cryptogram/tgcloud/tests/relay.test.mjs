import assert from 'node:assert/strict';
import { readdir } from 'node:fs/promises';
import test from 'node:test';

import { setDatabase } from 'sdk';

import relayGet from '../endpoints/relayGet.js';
import relayPut from '../endpoints/relayPut.js';
import {
  MAX_GET_MESSAGES,
  MAX_GET_TOTAL_BYTES,
  MAX_PAYLOAD_ENCODED_LENGTH,
} from '../lib/relay.js';

const CHANNEL = 'AAAAAAAAAAAAAAAAAAAAAA';
const NONCANONICAL_CHANNEL = 'AAAAAAAAAAAAAAAAAAAAAB';

const createDatabase = () => {
  const state = {
    rows: [],
    runCalls: [],
    collisionOnce: false,
    selectCalls: [],
    selectLimits: [],
  };
  let writeTail = Promise.resolve();
  const database = {
    run(sql, params) {
      state.runCalls.push({ sql, params });
      const operation = writeTail.then(async () => {
        await Promise.resolve();
        if (state.collisionOnce) {
          state.collisionOnce = false;
          throw new Error('UNIQUE constraint failed: relay_messages.channel, relay_messages.sequence');
        }
        const channel = params[':channel'];
        const payload = params[':payload'];
        const sequence = state.rows
          .filter((row) => row.channel === channel)
          .reduce((highest, row) => Math.max(highest, row.sequence), 0) + 1;
        state.rows.push({ channel, sequence, payload });
        return {
          rowsAffected: 1,
          lastInsertRowid: state.rows.length,
          rows: [{ sequence }],
        };
      });
      writeTail = operation.then(() => undefined, () => undefined);
      return operation;
    },
    select() {
      let condition = null;
      let limit = null;
      const builder = {
        from(table) {
          state.selectCalls.push({ table: table.__table.name });
          return builder;
        },
        where(nextCondition) {
          condition = nextCondition;
          return builder;
        },
        orderBy() {
          return builder;
        },
        limit(value) {
          limit = value;
          state.selectLimits.push(value);
          return builder;
        },
        async all() {
          const channelCondition = condition.conditions.find((item) => item.op === 'eq');
          const sequenceCondition = condition.conditions.find((item) => item.op === 'gt');
          const channel = channelCondition.right;
          const after = sequenceCondition.right;
          return state.rows
            .filter((row) => row.channel === channel && row.sequence > after)
            .sort((left, right) => left.sequence - right.sequence)
            .slice(0, limit)
            .map(({ sequence, payload }) => ({ sequence, payload }));
        },
      };
      return builder;
    },
  };
  return { database, state };
};

const expectCode = async (operation, code) => {
  await assert.rejects(operation, (error) => (
    error.name === 'EndpointError'
    && error.parameters?.code === code
  ));
};

test('exposes exactly the two relay endpoints', async () => {
  const files = await readdir(new URL('../endpoints/', import.meta.url));
  assert.deepEqual(files.filter((file) => file.endsWith('.js')).sort(), ['relayGet.js', 'relayPut.js']);
});

test('assigns unique monotonic sequences to concurrent puts', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  const responses = await Promise.all(
    Array.from({ length: 24 }, (_, index) => relayPut({
      channel: CHANNEL,
      payload: index % 2 === 0 ? 'AQ' : '_w',
    })),
  );
  const sequences = responses.map(({ sequence }) => sequence).sort((left, right) => left - right);
  assert.deepEqual(sequences, Array.from({ length: 24 }, (_, index) => index + 1));
  assert.equal(state.rows.length, 24);
  assert.deepEqual(Object.keys(state.rows[0]).sort(), ['channel', 'payload', 'sequence']);
});

test('retries a sequence collision without changing the relay contract', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  state.collisionOnce = true;
  const response = await relayPut({ channel: CHANNEL, payload: 'AQ' });
  assert.equal(response.sequence, 1);
  assert.equal(state.rows.length, 1);
  assert.equal(state.runCalls.length, 2);
});

test('rejects invalid and expanded requests before storage access', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  await expectCode(
    relayPut({ channel: CHANNEL, payload: 'AQ', sequence: 1 }),
    'INVALID_REQUEST',
  );
  await expectCode(relayPut({ channel: 'invalid', payload: 'AQ' }), 'INVALID_CHANNEL');
  await expectCode(relayPut({ channel: 'AAAAAAAAAAAAAAAAAAAAAB', payload: 'AQ' }), 'INVALID_CHANNEL');
  await expectCode(relayPut({ channel: NONCANONICAL_CHANNEL, payload: 'AQ' }), 'INVALID_CHANNEL');
  await expectCode(relayPut({ channel: CHANNEL, payload: 'not base64!' }), 'INVALID_PAYLOAD');
  await expectCode(relayPut({ channel: CHANNEL, payload: 'AB' }), 'INVALID_PAYLOAD');
  await expectCode(relayPut({ channel: CHANNEL, payload: 'AAAB' }), 'INVALID_PAYLOAD');
  await expectCode(
    relayPut({ channel: CHANNEL, payload: 'A'.repeat(MAX_PAYLOAD_ENCODED_LENGTH + 1) }),
    'INVALID_PAYLOAD',
  );
  await expectCode(relayGet({ channel: CHANNEL, sequence: -1 }), 'INVALID_SEQUENCE');
  assert.equal(state.runCalls.length, 0);
  assert.equal(state.selectCalls.length, 0);
});

test('returns only messages after the cursor in sequence order', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  await Promise.all(Array.from({ length: 8 }, (_, index) => relayPut({
    channel: CHANNEL,
    payload: index % 2 === 0 ? 'AQ' : '_w',
  })));
  const result = await relayGet({ channel: CHANNEL, sequence: 3 });
  assert.deepEqual(result.messages.map(({ sequence }) => sequence), [4, 5, 6, 7, 8]);
  assert.equal(state.selectCalls.at(-1).table, 'relay_messages');
});

test('rejects corrupted payload rows', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  state.rows.push({ channel: CHANNEL, sequence: 1, payload: 'AAAB' });
  await expectCode(relayGet({ channel: CHANNEL, sequence: 0 }), 'RELAY_READ_ERROR');
});

test('caps message count and aggregate payload bytes', async () => {
  const { database, state } = createDatabase();
  setDatabase(database);
  const payload = Buffer.alloc(70_000).toString('base64url');
  state.rows.push(
    { channel: CHANNEL, sequence: 1, payload },
    { channel: CHANNEL, sequence: 2, payload },
  );
  const result = await relayGet({ channel: CHANNEL, sequence: 0 });
  assert.equal(result.messages.length, 1);
  assert.ok(result.messages[0].payload.length <= MAX_GET_TOTAL_BYTES);
  assert.equal(state.selectCalls.length, 1);
  assert.equal(state.selectLimits.at(-1), MAX_GET_MESSAGES);
});
