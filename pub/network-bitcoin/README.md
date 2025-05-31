network-bitcoin
====

This library supports [Bitcoin Core][1] `v0.18` only.

See the [Hackage documentation][2].

Testing
----

The tests expect to run against a `bitcoind` node running in regtest mode.
Invoke `bitcoind` with:

```shell
$ bitcoind -regtest -rpcuser=bitcoinrpc -rpcpassword=bitcoinrpcpassword -rpcport=18444
```

[1]: https://github.com/bitcoin/bitcoin
[2]: http://hackage.haskell.org/package/network-bitcoin
