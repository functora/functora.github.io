import { readFile } from 'node:fs/promises';

const SDK_SOURCE = `
let database = {
  run: () => Promise.reject(new Error('database not configured')),
  select: () => Promise.reject(new Error('database not configured')),
};

export const db = {
  run: (...args) => database.run(...args),
  select: (...args) => database.select(...args),
};

export const setDatabase = (nextDatabase) => {
  database = nextDatabase;
};

export class EndpointError extends Error {
  constructor(description, parameters) {
    super(description);
    this.name = 'EndpointError';
    this.parameters = parameters;
  }
}
`;

const SDK_DB_SOURCE = `
const column = (name, type) => ({
  __column: { name, type },
  notNull() {
    return { ...this, __notNull: true };
  },
  default(value) {
    return { ...this, __default: value };
  },
  primaryKey() {
    return { ...this, __primaryKey: true };
  },
});

export const table = (name, columns, extras) => ({
  ...columns,
  __table: { name, columns },
  __extras: extras?.(columns) ?? {},
});

export const text = (name) => column(name, 'text');
export const integer = (name) => column(name, 'integer');
export const primaryKey = ({ columns }) => ({ __extra: true, columns });
export const and = (...conditions) => ({ op: 'and', conditions });
export const eq = (left, right) => ({ op: 'eq', left, right });
export const gt = (left, right) => ({ op: 'gt', left, right });
export const asc = (value) => ({ op: 'asc', value });
`;

const PROJECT_MARKER = '/cryptogram/tgcloud/';

export async function resolve(specifier, context, nextResolve) {
  if (specifier === 'sdk') {
    return { url: 'tgcloud:sdk', shortCircuit: true };
  }
  if (specifier === 'sdk/db') {
    return { url: 'tgcloud:sdk-db', shortCircuit: true };
  }
  return nextResolve(specifier, context);
}

export async function load(url, context, nextLoad) {
  if (url === 'tgcloud:sdk') {
    return { format: 'module', source: SDK_SOURCE, shortCircuit: true };
  }
  if (url === 'tgcloud:sdk-db') {
    return { format: 'module', source: SDK_DB_SOURCE, shortCircuit: true };
  }
  if (url.startsWith('file:') && url.includes(PROJECT_MARKER) && url.endsWith('.js')) {
    return {
      format: 'module',
      source: await readFile(new URL(url), 'utf8'),
      shortCircuit: true,
    };
  }
  return nextLoad(url, context);
}
