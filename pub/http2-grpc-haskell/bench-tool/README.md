bench-tool
==========

A tool to facilitate interoperability and performance investigations.  For
simplicity, we support all tested combinations of features in a single
mega-binary.

Although this tool can serve as an example of all combinations of moving parts,
please keep in mind that the use case is not representative of normal
applications (in particular the readability/performance tradeoff in this
package may be leaning towards the performance side so that the benchmarks are
representative of the networking stack).

# Usage

The idea is that the client-side does all the configuration logic in terms of
(queries to send, number of connections, threads, repetitions). When the server
ust chose to behave a certain way (e.g., returning multiple responses in the
streaming-server RPC) the behavior is part of the client-query.

At this time, the mega-binary only has a naive argument parser for both client
and server branches.

To start a server:

```bash
stack exec -- bench-tool-exe \
  --mode Server \
  --host localhost \
  --port 3030 \
  # all the following arguments are ignored but required due to the naive parser
  --program Unary \
  --tls WithTls \
  --protoLib ProtoLens \
  --compression NoCompression \
  --repetitions 0 \
  --concurrentClients 0 \
  --concurrentQueries 0
```

To start the client:

```bash
stack exec -- bench-tool-exe \
  --mode Client \
  --host localhost \
  --port 3030 \
  --program <Unary|ClientStreaming|ServerStreaming> \
  --tls <WithTls|NoTls> \
  --protoLib ProtoLens \
  --compression <NoCompression|GzipCompression> \
  --repetitions 100 \
  --concurrentClients 4 \
  --concurrentQueries 50 
```

The `program` argument determines the code to be exercised:

- Unary: one query / one reply
- ClientStreaming: multiple queries / one reply
- ServerStreaming: one query / multiple replies

The `concurrentClients` argument dictates how many concurrent gRPC sessions to start.

The `concurrentQueries` argument dictates the number of threads for each session.

The `repetitions`  argument dictates how many messages are sent over each thread.

In total, the number of requests (or replies) is `concurrentClients * concurrentQueries * repetitions`.

# Bench proto

See `protos/bench.proto` for the Protobuf definition of the benchmark.

We will soon settle on some better "benchmarking protocol" to test a variety of
messages types.
