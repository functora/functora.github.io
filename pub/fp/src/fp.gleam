import gleam/list
import gleam/result
import gleam/function
import gleam/option.{Option, Some}

//
// Functors
//

pub opaque type FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: fn(fn(a) -> b, fa) -> fb)
}

pub fn fmap(
  lhs: fn(a) -> b,
  rhs: fa,
  instance: fn() -> FunctorInstance(a, b, fa, fb),
) -> fb {
  instance().fmap(lhs, rhs)
}

pub fn mkfunctor(
  fmapper: fn(fa, fn(a) -> b) -> fb,
) -> FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: function.flip(fmapper))
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
// Applicatives
//

pub opaque type ApplicativeInstance(a, b, fa, fb, fab) {
  ApplicativeInstance(
    functor: fn() -> FunctorInstance(a, b, fa, fb),
    pure: fn(a) -> fa,
    ap: fn(fab, fa) -> fb,
  )
}

pub fn ap(
  lhs: fab,
  rhs: fa,
  instance: fn() -> ApplicativeInstance(a, b, fa, fb, fab),
) -> fb {
  instance().ap(lhs, rhs)
}

pub fn alst() -> ApplicativeInstance(a, b, List(a), List(b), List(fn(a) -> b)) {
  ApplicativeInstance(
    functor: flst,
    pure: fn(x) { [x] },
    ap: fn(lhs, rhs) {
      lhs
      |> list.flat_map(fn(f) { fmap(f, rhs, flst) })
    },
  )
}

pub fn aopt() -> ApplicativeInstance(
  a,
  b,
  Option(a),
  Option(b),
  Option(fn(a) -> b),
) {
  ApplicativeInstance(
    functor: fopt,
    pure: Some,
    ap: fn(lhs, rhs) {
      lhs
      |> option.then(fn(f) { fmap(f, rhs, fopt) })
    },
  )
}

pub fn ares() -> ApplicativeInstance(
  a,
  b,
  Result(a, e),
  Result(b, e),
  Result(fn(a) -> b, e),
) {
  ApplicativeInstance(
    functor: fres,
    pure: Ok,
    ap: fn(lhs, rhs) {
      lhs
      |> result.then(fn(f) { fmap(f, rhs, fres) })
    },
  )
}
