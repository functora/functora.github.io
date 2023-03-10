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

pub fn fmapf(
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
// Applicatives
//

pub opaque type ApplicativeInstance(a, b, fa, fb, fab) {
  ApplicativeInstance(
    functor: FunctorInstance(a, b, fa, fb),
    ap: fn(fab, fa) -> fb,
  )
}

pub fn ap(
  f: fab,
  x: fa,
  instance: fn() -> ApplicativeInstance(a, b, fa, fb, fab),
) -> fb {
  instance().ap(f, x)
}

pub fn apf(
  x: fa,
  f: fab,
  instance: fn() -> ApplicativeInstance(a, b, fa, fb, fab),
) -> fb {
  instance().ap(f, x)
}

pub fn alst() -> ApplicativeInstance(a, b, List(a), List(b), List(fn(a) -> b)) {
  ApplicativeInstance(
    functor: flst(),
    ap: fn(fab, fa) {
      fab
      |> list.flat_map(fn(f) { fmap(f, fa, flst) })
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
    functor: fopt(),
    ap: fn(fab, fa) {
      fab
      |> option.then(fn(f) { fmap(f, fa, fopt) })
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
    functor: fres(),
    ap: fn(fab, fa) {
      fab
      |> result.then(fn(f) { fmap(f, fa, fres) })
    },
  )
}
