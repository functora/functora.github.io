import { getRelayMessages, parseRelayGetInput } from '../lib/relay.js';

export default async function relayGet(input) {
  const { channel, sequence } = parseRelayGetInput(input);
  return getRelayMessages(channel, sequence);
}
