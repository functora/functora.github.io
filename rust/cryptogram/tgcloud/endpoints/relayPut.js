import { parseRelayPutInput, putRelayMessage } from '../lib/relay.js';

export default async function relayPut(input) {
  const { channel, payload } = parseRelayPutInput(input);
  return putRelayMessage(channel, payload);
}
