# functora-tagged

Lightweight, macro-free newtypes with refinement and derived traits.

## Motivation

Newtypes are a fundamental pattern in Rust for enhancing type safety and expressing semantic meaning. By wrapping an existing type (the `Rep`resentation) in a new, distinct type, we prevent accidental misuse and clearly communicate intent. For example, distinguishing between a `UserId` and a `ProductId`, even if both are internally represented as `u64`, prevents bugs where one might be used in place of the other. This style makes code more self-documenting and less prone to logical errors.

While standard traits like `Eq`, `PartialEq`, `Ord`, `PartialOrd`, `Clone`, and `Debug` can often be derived automatically in Rust, many essential traits are not. These include parsing (`FromStr`), serialization (`serde::Serialize`/`Deserialize`), and database integration (`diesel::Queryable`, `ToSql`, `FromSql`, `AsExpression`). Implementing these traits for newtypes manually can lead to substantial boilerplate.

Rust has common macro-based solutions for the newtype traits derivation problem. Macros are often employed as a last resort when language expressiveness is insufficient. However, they introduce significant drawbacks:

- Syntax: Macro-based code is not just Rust code. It acts as a complex, "foreign" DSL that is hard to read, maintain, and extend.
- Boilerplate: Despite their intent, macros frequently require significant boilerplate, undermining their primary benefit.
- Complexity: Macros can obscure the underlying logic and make debugging difficult.

`functora-tagged` offers a superior, macro-free alternative. It provides a clean, idiomatic, and type-safe mechanism for creating newtypes. Through the `Refine` trait, you can define custom validation and transformation logic for your newtype. This logic is then automatically integrated into implementations for crucial, non-trivially derivable traits like `FromStr`, `serde`, and `diesel`, achieving true zero boilerplate for these complex scenarios without the downsides of macros.

## Tagged Struct

The primary newtype building block is the `Tagged<Rep, Tag>` struct.

- `Rep`: The underlying representation type (e.g., `String`, `i32`).
- `Tag`: A phantom type used at compile time to distinguish between different newtypes that share the same `Rep`. The `Tag` type itself implements the `Refine<Rep>` trait to define refinement logic.

This structure allows you to create distinct types that behave identically to their `Rep` type for many traits, unless explicitly customized.

```rust
use std::marker::PhantomData;

pub struct Tagged<Rep, Tag, F>(Rep, PhantomData<(Tag, F)>);
```

The `Tagged::new` constructor returns a `Result` that can be `unwrap`ped directly if the `Tag`'s `Refine` implementation returns `std::convert::Infallible`. The `InfallibleInto` trait and its `infallible()` method provide a convenient way to handle this.

## `ViaString` Struct

The `ViaString<Rep, Tag>` struct is a specialized newtype primarily intended for scenarios where the underlying representation (`Rep`) is closely tied to string manipulation or needs to be serialized/deserialized as a string. It differs from `Tagged` in its serialization and deserialization behavior:

- **Serialization**: `ViaString` serializes to its string representation (via `ToString` on `Rep`), whereas `Tagged` serializes the `Rep` directly.
- **Deserialization**: `ViaString` deserializes from a string, then attempts to parse it into `Rep` using `FromStr`. `Tagged` deserializes `Rep` directly.

It also implements `FromStr` and derives common traits, similar to `Tagged`, respecting the `Refine` trait for validation.

## Refine Trait

To enforce specific refinement rules for your newtypes, you implement the `Refine<Rep>` trait for the `Tag` type. This trait allows you to define custom logic for refining the newtype representation.

```rust
use functora_tagged::*;

pub enum NonEmptyTag {}
pub struct NonEmptyError;

impl Refine<String> for NonEmptyTag {
    type RefineError = NonEmptyError;
    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError> {
        if rep.is_empty() {
            Err(NonEmptyError)
        } else {
            Ok(rep)
        }
    }
}
```

Note that the `Refine` trait has a default implementation that simply returns the input `Rep` without modification. This allows you to create simple newtypes for type distinction without needing to implement the `refine` function, as demonstrated in the `NonNegTag` example.

## Derived Traits

`functora-tagged` provides blanket implementations for several important traits. These traits work seamlessly with your newtypes, respecting the underlying representation behavior and customizable refinement rules defined by the `Tag` type's implementation of `Refine<Rep>`.

### Direct Derive:

- `Eq`
- `PartialEq`
- `Ord`
- `PartialOrd`
- `Clone`
- `Debug`
- `serde::Serialize` (with `serde` feature)
- `diesel::serialize::ToSql` (with `diesel` feature)
- `diesel::expression::AsExpression` (with `diesel` feature)

### Refined Derive:

- `FromStr`: Implemented for `Tagged<Rep, Tag>` and `ViaString<Rep, Tag>`. Returns a `ParseError<Rep, Tag>`, which can be either a `Decode` error (from `Rep::from_str`) or a `Refine` error (from `Tag::refine`). For nested types, these errors can be further nested.
- `serde::Deserialize` (with `serde` feature)
- `diesel::Queryable` (with `diesel` feature)
- `diesel::deserialize::FromSql` (with `diesel` feature)

## Integrations

`functora-tagged` provides optional integrations for common Rust ecosystems:

- **`serde`**: For serialization and deserialization. Enable with the `serde` feature.
- **`diesel`**: For database interactions. Enable with the `diesel` feature.

These integrations respect the `Refine` rules defined for your types.

## Examples

You can promote `Rep` values into newtype values using `Tagged::new(rep)` applied directly to a `Rep` value. To demote a newtype value back to a `Rep` value, you can use the `.rep()` method to get a reference, or the `.untag()` method to consume the newtype and get the value. You can also use any serializer or deserializer for the newtype that is available for `Rep`.

### Default Newtype

When a `Tag` type has a default `Refine` implementation that doesn't add new constraints or transformations, `Tagged` can be used for simple type distinction.

```rust
use functora_tagged::*;
use std::convert::Infallible;

pub enum NonNegTag {}

impl Refine<usize> for NonNegTag {
    type RefineError = Infallible;
}

pub type NonNeg = Tagged<usize, NonNegTag, NonNegTag>;

let rep = 123;
let new = NonNeg::new(rep).infallible();

assert_eq!(*new.rep(), rep);
```

### Refined Newtype

This example demonstrates a simple refinement for numeric types to ensure they are positive, using `PositiveTag`.

```rust
use functora_tagged::*;

#[derive(PartialEq, Debug)]
pub enum PositiveTag {}
pub type Positive = Tagged<usize, PositiveTag, PositiveTag>;

#[derive(PartialEq, Debug)]
pub struct PositiveError;

impl Refine<usize> for PositiveTag {
    type RefineError = PositiveError;
    fn refine(
        rep: usize,
    ) -> Result<usize, Self::RefineError>
    {
        if rep > 0 {
            Ok(rep)
        } else {
            Err(PositiveError)
        }
    }
}

let rep = 100;
let new = Positive::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

let err = Positive::new(0).unwrap_err();
assert_eq!(err, PositiveError);
```

### Generic Newtype

This demonstrates a generic `Positive<Rep>` newtype that enforces positive values for any numeric type `Rep` that implements `Refine<PositiveTag>`.

```rust
use functora_tagged::*;
use num_traits::Zero;

#[derive(PartialEq, Debug)]
pub enum PositiveTag {}
pub type Positive<Rep> = Tagged<Rep, PositiveTag, PositiveTag>;

#[derive(PartialEq, Debug)]
pub struct PositiveError;

impl<Rep> Refine<Rep> for PositiveTag
where
    Rep: Zero + PartialOrd,
{
    type RefineError = PositiveError;
    fn refine(
        rep: Rep,
    ) -> Result<Rep, Self::RefineError>
    {
        if rep > Rep::zero() {
            Ok(rep)
        } else {
            Err(PositiveError)
        }
    }
}

let rep = 100;
let new = Positive::<i32>::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

let rep = 10.5;
let new = Positive::<f64>::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

let err = Positive::<i32>::new(-5).unwrap_err();
assert_eq!(err, PositiveError);

let err = Positive::<f64>::new(0.0).unwrap_err();
assert_eq!(err, PositiveError);
```

### Nested Newtype

This example demonstrates nesting newtypes: `UserId<Rep>` generic newtype is built on top of the other `NonEmpty<Rep>` generic newtype and adds its own refinement logic.

```rust
use functora_tagged::*;

#[derive(PartialEq, Debug)]
pub enum NonEmptyTag {}
pub type NonEmpty<Rep> = Tagged<Rep, NonEmptyTag, NonEmptyTag>;

#[derive(PartialEq, Debug)]
pub struct NonEmptyError;

impl Refine<String> for NonEmptyTag {
    type RefineError = NonEmptyError;
    fn refine(
        rep: String,
    ) -> Result<String, Self::RefineError>
    {
        if rep.is_empty() {
            Err(NonEmptyError)
        } else {
            Ok(rep)
        }
    }
}

impl Refine<isize> for NonEmptyTag {
    type RefineError = NonEmptyError;
    fn refine(
        rep: isize,
    ) -> Result<isize, Self::RefineError>
    {
        if rep == 0 {
            Err(NonEmptyError)
        } else {
            Ok(rep)
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum UserIdTag {}
pub type UserId<Rep> =
    Tagged<NonEmpty<Rep>, UserIdTag, UserIdTag>;

#[derive(PartialEq, Debug)]
pub struct UserIdError;

impl Refine<NonEmpty<String>> for UserIdTag {
    type RefineError = UserIdError;
    fn refine(
        rep: NonEmpty<String>,
    ) -> Result<NonEmpty<String>, Self::RefineError>
    {
        if rep.rep().starts_with("user_")
            && rep.rep().len() > 5
        {
            Ok(rep)
        } else {
            Err(UserIdError)
        }
    }
}

impl Refine<NonEmpty<isize>> for UserIdTag {
    type RefineError = UserIdError;
    fn refine(
        rep: NonEmpty<isize>,
    ) -> Result<NonEmpty<isize>, Self::RefineError>
    {
        if *rep.rep() > 0 {
            Ok(rep)
        } else {
            Err(UserIdError)
        }
    }
}

let rep = "user_123";
let new = rep.parse::<UserId<String>>().unwrap();
assert_eq!(new.rep().rep(), rep);

let err = "".parse::<UserId<String>>().unwrap_err();
assert_eq!(
    err,
    ParseError::Decode(ParseError::Refine(
        NonEmptyError,
        PhantomData
    ), PhantomData)
);

let err = "post_123"
    .parse::<UserId<String>>()
    .unwrap_err();
assert_eq!(err, ParseError::Refine(UserIdError, PhantomData));

let rep: isize = 123;
let new = rep
    .to_string()
    .parse::<UserId<isize>>()
    .unwrap();
assert_eq!(*new.rep().rep(), rep);

let err = "0".parse::<UserId<isize>>().unwrap_err();
assert_eq!(
    err,
    ParseError::Decode(ParseError::Refine(
        NonEmptyError,
        PhantomData
    ), PhantomData)
);

let err =
    "-1".parse::<UserId<isize>>().unwrap_err();
assert_eq!(err, ParseError::Refine(UserIdError, PhantomData));
```

## Dimensional Math

`functora-tagged` includes a `num` module that enables type-safe dimensional analysis and arithmetic. It prevents accidental mixing of units (e.g., adding meters to seconds) and ensures that operations produce correctly typed results (e.g., dividing meters by seconds yields velocity).

The system is built on four core algebraic types that carry unit information in their `PhantomData`:

- **`Identity<I, F>`**: Represents a neutral element or a base scalar (like a dimensionless number).
- **`Atomic<A, F>`**: Represents a fundamental unit (e.g., Meter, Second, Kg).
- **`Times<L, R, F>`**: Represents the product of two units (e.g., Meter \* Meter = Area).
- **`Per<L, R, F>`**: Represents the quotient of two units (e.g., Meter / Second = Velocity).

All these types accept a refinement generic `F` (e.g., `DNonNeg`, `DPos`, etc.) to enforce constraints like non-negativity on the underlying values.

### Example

This example demonstrates how to define physical units and calculate Kinetic Energy (i.e. **Ek = kg \* (m/s)^2**) safely.

```rust
use functora_tagged::num::*;
use functora_tagged::*;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

//
// 1. Dimensionless unit (Identity)
//

#[derive(Debug)]
pub enum INum {}
type DNum = Identity<INum, DAny>;
type Num = Tagged<Decimal, DNum, DAny>;

//
// 2. Fundamental units (Atomic)
//

#[derive(Debug)]
pub enum AMeter {}
#[derive(Debug)]
pub enum ASecond {}
#[derive(Debug)]
pub enum AKg {}

// The "Dimension" types with non-negative refinement
type DMeter = Atomic<AMeter, DNonNeg>;
type DSecond = Atomic<ASecond, DNonNeg>;
type DKg = Atomic<AKg, DNonNeg>;

// The concrete types with Decimal rep
type Meter = Tagged<Decimal, DMeter, DNonNeg>;
type Second = Tagged<Decimal, DSecond, DNonNeg>;
type Kg = Tagged<Decimal, DKg, DNonNeg>;

//
// 3. Non-fundamental units (Per, Times)
//

// Velocity = Meter / Second
type DVelocity = Per<DMeter, DSecond, DNonNeg>;
type Velocity = Tagged<Decimal, DVelocity, DNonNeg>;

// Joule = Kg * Velocity^2
type DJoule = Times<
    DKg,
    Times<DVelocity, DVelocity, DNonNeg>,
    DNonNeg,
>;
type Joule = Tagged<Decimal, DJoule, DNonNeg>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let distance = Meter::new(dec!(50))?; // 50 meters
    let time = Second::new(dec!(5))?; // 5 seconds
    let mass = Kg::new(dec!(100))?; // 100 kg

    // Calculate velocity: 50m / 5s = 10 m/s
    let velocity: Velocity = distance.tdiv(&time)?;
    assert_eq!(velocity.rep(), &dec!(10));

    // Calculate Energy: 100kg * (10m/s)^2 = 10000J
    let energy: Joule =
        mass.tmul(&velocity.tmul(&velocity)?)?;
    assert_eq!(energy.rep(), &dec!(10000));

    // Scaling by dimensionless 0.5 doesn't change units
    let half = Num::new(dec!(0.5))?;
    let half_energy: Joule = energy.tmul(&half)?;
    assert_eq!(half_energy.rep(), &dec!(5000));

    Ok(())
}
```

<hr>

© 2025 [Functora](https://functora.github.io/). All rights reserved.
