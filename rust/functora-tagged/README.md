# functora-tagged

Lightweight, macro-free newtypes with refinement and derived traits.

## Motivation

Newtypes are a fundamental pattern in Rust for enhancing type safety and expressing semantic meaning. By wrapping an existing type (the representation) in a new, distinct type, we prevent accidental misuse and clearly communicate intent. For example, distinguishing between a `UserId` and a `ProductId`, even if both are internally represented as `u64`, prevents bugs where one might be used in place of the other. This style makes code more self-documenting and less prone to logical errors.

While standard traits like `Eq`, `PartialEq`, `Ord`, `PartialOrd`, `Clone`, and `Debug` can often be derived automatically in Rust, many essential traits are not. These include parsing (`FromStr`), serialization (`serde::Serialize`/`Deserialize`), and database integration (`diesel::Queryable`, `ToSql`, `FromSql`, `AsExpression`). Implementing these traits for newtypes manually can lead to substantial boilerplate.

Rust has common macro-based solutions for the newtype traits derivation problem. Macros are often employed as a last resort when language expressiveness is insufficient. However, they introduce significant drawbacks:

- Syntax: Macro-based code is not just Rust code. It acts as a complex, "foreign" DSL that is hard to read, maintain, and extend.
- Boilerplate: Despite their intent, macros frequently require significant boilerplate, undermining their primary benefit.
- Complexity: Macros can obscure the underlying logic and make debugging difficult.

`functora-tagged` offers a superior, macro-free alternative. It provides a clean, idiomatic, and type-safe mechanism for creating newtypes. Through the `Refine` trait, you can define custom validation and transformation logic for your newtype. This logic is then automatically integrated into implementations for crucial, non-trivially derivable traits like `FromStr`, `serde`, and `diesel`, achieving true zero boilerplate for these complex scenarios without the downsides of macros.

## `Tagged` Struct

The primary newtype building block is the `Tagged<T, D, F>` struct.

- `T`: The underlying representation type (e.g., `String`, `i32`).
- `D`: The "Dimension". A phantom type used at compile time to distinguish between different newtypes that share the same `T`.
- `F`: The "Refinery". A phantom type that implements the `Refine<T>` trait to define validation and transformation logic.

This separation of `D` and `F` allows you to have the same semantic type (e.g., `Meter`) with different refinements (e.g., `NonNegative` vs `Positive`) without changing the type's identity for dimension-checking purposes.

```rust
use std::marker::PhantomData;

pub struct Tagged<T, D, F>(T, PhantomData<(D, F)>);
```

The `Tagged::new` constructor returns a `Result` that can be unwrapped directly if the `F`'s `Refine` implementation returns `std::convert::Infallible`. The `InfallibleInto` trait and its `infallible()` method provide a convenient way to handle this.

## `ViaString` Struct

The `ViaString<T, D, F>` struct is a specialized newtype primarily intended for scenarios where the underlying representation (`T`) is closely tied to string manipulation or needs to be serialized/deserialized as a string. It differs from `Tagged` in its serialization and deserialization behavior:

- **Serialization**: `ViaString` serializes to its string representation (via `ToString` on `T`), whereas `Tagged` serializes the `T` directly.
- **Deserialization**: `ViaString` deserializes from a string, then attempts to parse it into `T` using `FromStr`. `Tagged` deserializes `T` directly.

It also implements `FromStr` and derives common traits, similar to `Tagged`, respecting the `Refine` trait for validation.

## Refinement

To enforce specific refinement rules for your newtypes, you implement the `Refine<T>` trait for the `F` type (the Refinery). This trait allows you to define custom validation logic.

### Example: Even Number Refinement

Here is a complete example of defining a Dimension `D`, a Refinery `F`, and implementing `Refine` to ensure a number is even.

```rust
use functora_tagged::*;

// 1. Define the Dimension (D)
#[derive(Debug)]
pub enum DCount {}

// 2. Define the Refinery (F)
#[derive(Debug)]
pub enum FEven {}

// 3. Define the Error Type
#[derive(Debug, PartialEq)]
pub struct NotEvenError;

// 4. Implement Refine for the Refinery
impl Refine<i32> for FEven {
    type RefineError = NotEvenError;

    fn refine(rep: i32) -> Result<i32, Self::RefineError> {
        if rep % 2 == 0 {
            Ok(rep)
        } else {
            Err(NotEvenError)
        }
    }
}

// 5. Define the Newtype
pub type EvenCount = Tagged<i32, DCount, FEven>;

// Usage
let val = EvenCount::new(42);
assert!(val.is_ok());
assert_eq!(*val.unwrap().rep(), 42);

let err = EvenCount::new(43);
assert_eq!(err.unwrap_err(), NotEvenError);
```

## Common Refineries

`functora-tagged` provides a set of common, ready-to-use refineries in the `common` module (`src/common.rs`). You can use these to quickly create refined newtypes without writing boilerplate.

-   **`FCrude`**: No-op refinement. Used when you only need a distinct type without validation. `RefineError` is `Infallible`.
-   **`FPositive`**: Ensures the value is strictly greater than zero (`> 0`).
-   **`FNonNeg`**: Ensures the value is non-negative (`>= 0`).
-   **`FNonEmpty`**: Ensures a collection (iterable) is not empty.

You can import them via `use functora_tagged::common::*;`.

## Derived Traits

`functora-tagged` provides blanket implementations for several important traits. These traits work seamlessly with your newtypes, respecting the underlying representation behavior and customizable refinement rules defined by the `F` type's implementation of `Refine<T>`.

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

- `FromStr`: Implemented for `Tagged<T, D, F>` and `ViaString<T, D, F>`. Returns a `ParseError<T, D, F>`, which can be either a `Decode` error (from `T::from_str`) or a `Refine` error (from `F::refine`). For nested types, these errors can be further nested.
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

### Default Newtype (using `FCrude`)

When you don't need validation, use `FCrude` from the `common` module.

```rust
use functora_tagged::*;
use functora_tagged::common::*;

pub enum DUserId {}

pub type UserId = Tagged<u64, DUserId, FCrude>;

let id = UserId::new(12345).infallible();
assert_eq!(*id.rep(), 12345);
```

### Refined Newtype (using `FPositive`)

This example demonstrates ensuring numeric types are positive using `FPositive`.

```rust
use functora_tagged::*;
use functora_tagged::common::*;

#[derive(PartialEq, Debug)]
pub enum DCount {} // The Dimension

// Use common FPositive refinery
pub type PositiveCount = Tagged<usize, DCount, FPositive>;

let rep = 100;
let new = PositiveCount::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

// FPositive returns PositiveError on failure
let err = PositiveCount::new(0).unwrap_err();
assert!(matches!(err, PositiveError(0)));
```

### Generic Newtype (using `FPositive`)

This demonstrates a generic `PositiveAmount<T>` newtype that enforces positive values for any numeric type `T` that satisfies `FPositive`.

```rust
use functora_tagged::*;
use functora_tagged::common::*;

#[derive(Debug)]
pub enum DAmount {} // Dimension

pub type PositiveAmount<T> = Tagged<T, DAmount, FPositive>;

// Works with i32
let rep = 100;
let new = PositiveAmount::<i32>::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

// Works with f64
let rep = 10.5;
let new = PositiveAmount::<f64>::new(rep).unwrap();
assert_eq!(*new.rep(), rep);

// Validation fails
let err = PositiveAmount::<i32>::new(-5).unwrap_err();
assert!(matches!(err, PositiveError(-5)));

let err = PositiveAmount::<f64>::new(0.0).unwrap_err();
assert!(matches!(err, PositiveError(0.0)));
```

### Composite Refinement

This example demonstrates how to combine multiple refinement rules (e.g., `NonEmpty` and `UserId` format) into a single, flat `Tagged` type using a composite refinery. This avoids the complexity of nesting `Tagged` types.

```rust
use functora_tagged::*;

#[derive(PartialEq, Debug)]
pub enum DUserId {} // Dimension
#[derive(Debug)]
pub enum FUserId {} // Refinery

// Flat structure: one Tagged wrapper
pub type UserId = Tagged<String, DUserId, FUserId>;

#[derive(PartialEq, Debug)]
pub enum UserIdError {
    Empty,
    InvalidFormat,
}

impl Refine<String> for FUserId {
    type RefineError = UserIdError;

    fn refine(rep: String) -> Result<String, Self::RefineError> {
        if rep.is_empty() {
            Err(UserIdError::Empty)
        } else if rep.starts_with("user_") && rep.len() > 5 {
            Ok(rep)
        } else {
            Err(UserIdError::InvalidFormat)
        }
    }
}

let rep = "user_123";
let new = rep.parse::<UserId>().unwrap();
assert_eq!(*new.rep(), rep);

let err = "".parse::<UserId>().unwrap_err();
assert_eq!(
    err,
    ParseError::Refine(UserIdError::Empty, PhantomData)
);

let err = "post_123".parse::<UserId>().unwrap_err();
assert_eq!(
    err,
    ParseError::Refine(UserIdError::InvalidFormat, PhantomData)
);
```

## Dimensional Math

`functora-tagged` includes a `num` module that enables type-safe dimensional analysis and arithmetic. It prevents accidental mixing of units (e.g., adding meters to seconds) and ensures that operations produce correctly typed results (e.g., dividing meters by seconds yields velocity).

The system is built on four core algebraic types that carry unit information in their `PhantomData`:

- **`Identity<I, F>`**: Represents a neutral element or a base scalar (like a dimensionless number).
- **`Atomic<A, F>`**: Represents a fundamental unit (e.g., Meter, Second, Kg).
- **`Times<L, R, F>`**: Represents the product of two units (e.g., Meter \* Meter = Area).
- **`Per<L, R, F>`**: Represents the quotient of two units (e.g., Meter / Second = Velocity).

All these types accept a refinement generic `F` (e.g., `FPositive`, `FNonNeg`, `FNonEmpty` etc.) to enforce constraints like non-negativity on the underlying values.

### Example

This example demonstrates how to define physical units and calculate Kinetic Energy (i.e. **Ek = kg \* (m/s)^2**) safely.

```rust
use functora_tagged::num::*;
use functora_tagged::*;
use functora_tagged::common::*;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

//
// 1. Dimensionless unit (Identity)
//

#[derive(Debug)]
pub enum INum {}
type DNum = Identity<INum, FCrude>;
type Num = Tagged<Decimal, DNum, FCrude>;

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
type DMeter = Atomic<AMeter, FNonNeg>;
type DSecond = Atomic<ASecond, FNonNeg>;
type DKg = Atomic<AKg, FNonNeg>;

// The concrete types with Decimal rep
type Meter = Tagged<Decimal, DMeter, FNonNeg>;
type Second = Tagged<Decimal, DSecond, FNonNeg>;
type Kg = Tagged<Decimal, DKg, FNonNeg>;

//
// 3. Non-fundamental units (Per, Times)
//

// Velocity = Meter / Second
type DVelocity = Per<DMeter, DSecond, FNonNeg>;
type Velocity = Tagged<Decimal, DVelocity, FNonNeg>;

// Joule = Kg * Velocity^2
type DJoule = Times<
    DKg,
    Times<DVelocity, DVelocity, FNonNeg>,
    FNonNeg,
>;
type Joule = Tagged<Decimal, DJoule, FNonNeg>;

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
