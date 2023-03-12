import gleam/list
import gleam/result
import gleam/function
import gleam/option.{Option, Some}

//
// Functors
//

pub type FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: fn(fn(a) -> b, fa) -> fb)
}

// Methods

pub fn fmap(
  lhs: fn(a) -> b,
  rhs: fa,
  instance: fn() -> FunctorInstance(a, b, fa, fb),
) -> fb {
  instance().fmap(lhs, rhs)
}

// Instances

pub fn new_functor_instance(
  fmapper: fn(fa, fn(a) -> b) -> fb,
) -> FunctorInstance(a, b, fa, fb) {
  FunctorInstance(fmap: function.flip(fmapper))
}

pub fn list_functor_instance() -> FunctorInstance(a, b, List(a), List(b)) {
  new_functor_instance(list.map)
}

pub fn option_functor_instance() -> FunctorInstance(a, b, Option(a), Option(b)) {
  new_functor_instance(option.map)
}

pub fn result_functor_instance() -> FunctorInstance(
  a,
  b,
  Result(a, e),
  Result(b, e),
) {
  new_functor_instance(result.map)
}

pub fn function_functor_instance() -> FunctorInstance(
  a,
  b,
  fn(r) -> a,
  fn(r) -> b,
) {
  new_functor_instance(function.compose)
}

// Shortcuts

pub fn fmap_list(lhs: fn(a) -> b, rhs: List(a)) -> List(b) {
  fmap(lhs, rhs, list_functor_instance)
}

pub fn fmap_option(lhs: fn(a) -> b, rhs: Option(a)) -> Option(b) {
  fmap(lhs, rhs, option_functor_instance)
}

pub fn fmap_result(lhs: fn(a) -> b, rhs: Result(a, e)) -> Result(b, e) {
  fmap(lhs, rhs, result_functor_instance)
}

pub fn fmap_function(lhs: fn(a) -> b, rhs: fn(r) -> a) -> fn(r) -> b {
  fmap(lhs, rhs, function_functor_instance)
}

//
// Applicatives
//

pub type ApplicativeInstance(a, b, fa, fb, fab) {
  ApplicativeInstance(
    functor_instance: fn() -> FunctorInstance(a, b, fa, fb),
    pure: fn(a) -> fa,
    ap: fn(fab, fa) -> fb,
  )
}

// Methods

pub fn pure(
  x: a,
  instance: fn() -> ApplicativeInstance(a, b, fa, fb, fab),
) -> fa {
  instance().pure(x)
}

pub fn ap(
  lhs: fab,
  rhs: fa,
  instance: fn() -> ApplicativeInstance(a, b, fa, fb, fab),
) -> fb {
  instance().ap(lhs, rhs)
}

// Instances

pub fn list_applicative_instance() -> ApplicativeInstance(
  a,
  b,
  List(a),
  List(b),
  List(fn(a) -> b),
) {
  ApplicativeInstance(
    functor_instance: list_functor_instance,
    pure: fn(x) { [x] },
    ap: fn(lhs, rhs) {
      lhs
      |> list.flat_map(fn(f) { fmap(f, rhs, list_functor_instance) })
    },
  )
}

pub fn option_applicative_instance() -> ApplicativeInstance(
  a,
  b,
  Option(a),
  Option(b),
  Option(fn(a) -> b),
) {
  ApplicativeInstance(
    functor_instance: option_functor_instance,
    pure: Some,
    ap: fn(lhs, rhs) {
      lhs
      |> option.then(fn(f) { fmap(f, rhs, option_functor_instance) })
    },
  )
}

pub fn result_applicative_instance() -> ApplicativeInstance(
  a,
  b,
  Result(a, e),
  Result(b, e),
  Result(fn(a) -> b, e),
) {
  ApplicativeInstance(
    functor_instance: result_functor_instance,
    pure: Ok,
    ap: fn(lhs, rhs) {
      lhs
      |> result.then(fn(f) { fmap(f, rhs, result_functor_instance) })
    },
  )
}

// Shortcuts

pub fn pure_list(x: a) -> List(a) {
  pure(x, list_applicative_instance)
}

pub fn pure_option(x: a) -> Option(a) {
  pure(x, option_applicative_instance)
}

pub fn pure_result(x: a) -> Result(a, e) {
  pure(x, result_applicative_instance)
}

pub fn ap_list(lhs: List(fn(a) -> b), rhs: List(a)) -> List(b) {
  ap(lhs, rhs, list_applicative_instance)
}

pub fn ap_option(lhs: Option(fn(a) -> b), rhs: Option(a)) -> Option(b) {
  ap(lhs, rhs, option_applicative_instance)
}

pub fn ap_result(lhs: Result(fn(a) -> b, e), rhs: Result(a, e)) -> Result(b, e) {
  ap(lhs, rhs, result_applicative_instance)
}
