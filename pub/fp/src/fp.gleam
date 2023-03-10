import gleam/list
import gleam/result
import gleam/function
import gleam/option.{Option}

//
// Functors
//

pub opaque type FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: fn(fn(a) -> b, fa) -> fb)
}

pub fn fmap(
  f: fn(a) -> b,
  x: fa,
  instance: fn() -> FunctorInstance(a, b, fa, fb),
) -> fb {
  instance().fmap(f, x)
}

pub fn ffmap(
  x: fa,
  f: fn(a) -> b,
  instance: fn() -> FunctorInstance(a, b, fa, fb),
) -> fb {
  instance().fmap(f, x)
}

pub fn mkfunctor(
  fmapper: fn(fa, fn(a) -> b) -> fb,
) -> FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: fn(f, x) { fmapper(x, f) })
}

pub fn flst() -> FunctorInstance(a, b, List(a), List(b)) {
  mkfunctor(list.map)
}

pub fn fopt() -> FunctorInstance(a, b, Option(a), Option(b)) {
  mkfunctor(option.map)
}

pub fn fres() -> FunctorInstance(a, b, Result(a, e), Result(b, e)) {
  mkfunctor(result.map)
}

pub fn ffun() -> FunctorInstance(a, b, fn(r) -> a, fn(r) -> b) {
  mkfunctor(function.compose)
}
//
// Applicatives - TODO
//
