# fp

[![Package Version](https://img.shields.io/hexpm/v/fp)](https://hex.pm/packages/fp)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/fp/)

Haskell-style type classes and combinators.

## Example

Gleam provides monad-like expressions `use foo <- bar(buz)` out of the box, which are very powerful. However, they can lead to scope pollution with variables. By using applicatives, we can avoid creating extra variables in many cases and adopt a more eta-reduced style:

```gleam
import fp
import gleam/int
import gleam/base
import gleam/function

pub type Wallet {
  Wallet(balance: Int, seed: BitString, info: String)
}

pub fn applicative_web_form(
  balance: String,
  seed: String,
  info: String,
) -> Result(Wallet, Nil) {
  Wallet
  |> function.curry3
  |> fp.fmap_result(int.parse(balance))
  |> fp.ap_result(base.decode64(seed))
  |> fp.ap_result(fp.pure_result(info))
}
```

and then:

```gleam
applicative_web_form("100", "sRSOkra5qC0=", "Hello!")
// Ok(Wallet(100, <<177, 20, 142, 146, 182, 185, 168, 45>>, "Hello!"))

applicative_web_form("WORLD!", "sRSOkra5qC0=", "Hello!")
// Error(Nil)

applicative_web_form("100", "WORLD!", "Hello!")
// Error(Nil)

applicative_web_form("WORLD!", "WORLD!", "Hello!")
// Error(Nil)

applicative_web_form("WORLD!", "WORLD!", "WORLD!")
// Error(Nil)
```

## Expansibility

Combinators like `fp.ap_result` are just shortcuts around more generic `fp.ap` with hardcoded instances:

```gleam
pub fn ap_result(lhs: Result(fn(a) -> b, e), rhs: Result(a, e)) -> Result(b, e) {
  ap(lhs, rhs, result_applicative_instance)
}
```

You can implement your own instances for other types and utilize `fp.ap`, or a simple shortcut wrapper like `fp.ap_mytype` to use applicative notation for working with your own types.
