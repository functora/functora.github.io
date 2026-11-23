import { integer, primaryKey, table, text } from 'sdk/db';

export const relayMessages = table('relay_messages', {
  channel: text('channel').notNull(),
  sequence: integer('sequence').notNull(),
  payload: text('payload').notNull(),
}, (t) => ({
  channelSequence: primaryKey({ columns: [t.channel, t.sequence] }),
}));
