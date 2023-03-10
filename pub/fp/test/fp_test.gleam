import gleeunit
import fp.{alst, aopt, ap, ares, ffun, flst, fmap, fopt, fres}
import gleeunit/should
import gleam/option.{Some}
import gleam/string
import gleam/function

pub fn main() {
  gleeunit.main()
}

pub fn list_functor_test() {
  fmap(fn(x) { string.inspect(x + 1) }, [1, 2, 3], flst)
  |> should.equal(["2", "3", "4"])
}

pub fn option_functor_test() {
  fmap(fn(x) { string.inspect(x + 1) }, Some(1), fopt)
  |> should.equal(Some("2"))
}

pub fn result_functor_test() {
  fmap(fn(x) { string.inspect(x + 1) }, Ok(1), fres)
  |> should.equal(Ok("2"))
}

pub fn function_functor_test() {
  fmap(string.inspect, fn(x) { x + 1 }, ffun)(1)
  |> should.equal("2")
}

pub type Bar {
  Bar(Int, String, Bool)
}

pub fn list_applicative_test() {
  Bar
  |> function.curry3
  |> fmap([1, 2], flst)
  |> ap(["foo", "bar"], alst)
  |> ap([True, False], alst)
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
  |> fmap(Some(1), fopt)
  |> ap(Some("foo"), aopt)
  |> ap(Some(True), aopt)
  |> should.equal(Some(Bar(1, "foo", True)))
}

pub fn result_applicative_test() {
  Bar
  |> function.curry3
  |> fmap(Ok(1), fres)
  |> ap(Ok("foo"), ares)
  |> ap(Ok(True), ares)
  |> should.equal(Ok(Bar(1, "foo", True)))
}
