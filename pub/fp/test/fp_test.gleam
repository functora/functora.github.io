import gleeunit
import fp
import gleeunit/should
import gleam/option.{Some}
import gleam/string
import gleam/int
import gleam/base
import gleam/function

pub fn main() {
  gleeunit.main()
}

pub fn list_functor_test() {
  fp.fmap_list(fn(x) { string.inspect(x + 1) }, [1, 2, 3])
  |> should.equal(["2", "3", "4"])
}

pub fn option_functor_test() {
  fp.fmap_option(fn(x) { string.inspect(x + 1) }, Some(1))
  |> should.equal(Some("2"))
}

pub fn result_functor_test() {
  fp.fmap_result(fn(x) { string.inspect(x + 1) }, Ok(1))
  |> should.equal(Ok("2"))
}

pub fn function_functor_test() {
  fp.fmap_function(string.inspect, fn(x) { x + 1 })(1)
  |> should.equal("2")
}

pub type Bar {
  Bar(Int, String, Bool)
}

pub fn list_applicative_test() {
  Bar
  |> function.curry3
  |> fp.fmap_list([1, 2])
  |> fp.ap_list(["foo", "bar"])
  |> fp.ap_list([True, False])
  |> should.equal([
    Bar(1, "foo", True),
    Bar(1, "foo", False),
    Bar(1, "bar", True),
    Bar(1, "bar", False),
    Bar(2, "foo", True),
    Bar(2, "foo", False),
    Bar(2, "bar", True),
    Bar(2, "bar", False),
  ])
}

pub fn option_applicative_test() {
  Bar
  |> function.curry3
  |> fp.fmap_option(Some(1))
  |> fp.ap_option(Some("foo"))
  |> fp.ap_option(Some(True))
  |> should.equal(Some(Bar(1, "foo", True)))
}

pub fn result_applicative_test() {
  Bar
  |> function.curry3
  |> fp.fmap_result(Ok(1))
  |> fp.ap_result(Ok("foo"))
  |> fp.ap_result(Ok(True))
  |> should.equal(Ok(Bar(1, "foo", True)))
}

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

pub fn example_test() {
  applicative_web_form("100", "sRSOkra5qC0=", "Hello!")
  |> should.equal(
    Wallet(
      balance: 100,
      seed: <<177, 20, 142, 146, 182, 185, 168, 45>>,
      info: "Hello!",
    )
    |> Ok,
  )
  applicative_web_form("WORLD!", "sRSOkra5qC0=", "Hello!")
  |> should.equal(Error(Nil))
  applicative_web_form("100", "WORLD!", "Hello!")
  |> should.equal(Error(Nil))
  applicative_web_form("WORLD!", "WORLD!", "Hello!")
  |> should.equal(Error(Nil))
  applicative_web_form("WORLD!", "WORLD!", "WORLD!")
  |> should.equal(Error(Nil))
}
