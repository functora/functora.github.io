import gleeunit
import fp.{ffun, flst, fmap, fopt, fres}
import gleeunit/should
import gleam/option.{Some}
import gleam/string

pub fn main() {
  gleeunit.main()
}

pub type Foo {
  Foo(Int)
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
  Foo
  |> fmap(Some(1), fopt)
  |> should.equal(Some(Foo(1)))
}

pub fn function_functor_test() {
  fmap(string.inspect, fn(x) { x + 1 }, ffun)(1)
  |> should.equal("2")
}
