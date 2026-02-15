# functora-tagged

Lightweight, macro-free newtypes with refinement and derived traits.

## Motivation

Newtypes are a fundamental pattern in Rust for enhancing type safety and expressing semantic meaning. By wrapping an existing type (the representation) in a distinct new type, we prevent accidental misuse and clearly communicate intent. For example, distinguishing between a `UserId` and a `ProductId`, even if both are internally represented as `u64`, prevents bugs where one might be used in place of the other. This pattern makes code more self-documenting and less prone to logical errors.

While standard traits like `Eq`, `PartialEq`, `Ord`, `PartialOrd`, `Clone`, and `Debug` can often be derived automatically, many essential traits are not. These include parsing (`FromStr`), serialization (`serde::Serialize`/`Deserialize`), and database integration (`diesel::Queryable`, `ToSql`, `FromSql`, `AsExpression`). Implementing these traits for newtypes manually can lead to substantial boilerplate.

Rust has common macro-based solutions for deriving newtype traits. Macros are often employed as a last resort when language expressiveness is insufficient. However, they introduce significant drawbacks:

- **Syntax**: Macro-based code is not conventional Rust code. It acts as a complex "foreign" DSL that is hard to read, maintain, and extend.
- **Boilerplate**: Despite their intent, macros frequently require significant boilerplate, undermining their primary benefit.
- **Complexity**: Macros can obscure the underlying logic and make debugging difficult.

`functora-tagged` offers a superior, macro-free alternative. It provides a clean, idiomatic, and type-safe mechanism for creating newtypes. Through the `Refine` trait, you can define custom verification and transformation logic for your newtype. This logic is then automatically integrated into implementations for crucial, non-trivially derivable traits like `FromStr`, `serde`, and `diesel`, achieving zero boilerplate for these complex scenarios without the downsides of macros.

## Tagged

The primary newtype building block is the `Tagged<T, D, F>` struct.

- `T`: The underlying representation type (e.g., `String`, `i32`).
- `D`: The "Dimension". A phantom type used at compile time to distinguish between different newtypes that share the same `T` and `F`.
- `F`: The "Refinery". A phantom type that implements the `Refine<T>` trait to define refinement logic.

This separation of `D` and `F` allows you to have the same semantic type (e.g., `Meter`) with different refinements (e.g., `NonNegative` vs `Positive`) without changing the type's identity for dimension-checking purposes.

```rust
use std::marker::PhantomData;

pub struct Tagged<T, D, F>(T, PhantomData<(D, F)>);
```

The `Tagged::new` constructor returns a `Result` that you can unwrap directly if the `Refine` implementation for `F` returns `Infallible`. The `InfallibleInto` trait and its `infallible()` method provide a convenient way to handle this.

### `ViaString`

The `ViaString<T, D, F>` struct is a specialized newtype for scenarios where the underlying representation (`T`) is closely tied to string manipulation or requires string-based serialization or deserialization. It differs from `Tagged` in its serialization behavior:

- **Serialization**: `ViaString` serializes to its string representation (via the `ToString` implementation of `T`), whereas `Tagged` serializes the `T` directly.
- **Deserialization**: `ViaString` deserializes from a string, then attempts to parse it into `T` using `FromStr`. `Tagged` deserializes `T` directly.

It also implements `FromStr` and derives common traits, similar to `Tagged`, respecting the `Refine` trait.

## Refinement

To enforce specific refinement rules for your newtypes, implement the `Refine<T>` trait for the `F` type (the Refinery). This trait allows you to define custom refinement logic.

### Custom Refinery

Here is a complete example of defining a Dimension `D`, a Refinery `F`, and implementing `Refine`.

```rust
use functora_tagged::*;

// 1. Define the Dimension (D)
#[derive(Debug)]
pub enum DCurrencyCode {}

// 2. Define the Refinery (F)
#[derive(Debug)]
pub enum FCurrencyCode {}

// 3. Define the Error Type
#[derive(Debug, PartialEq)]
pub struct CurrencyCodeError;

// 4. Implement Refine for the Refinery
impl Refine<String> for FCurrencyCode {
    type RefineError = CurrencyCodeError;

    fn refine(rep: String) -> Result<String, Self::RefineError> {
        let trimmed = rep.trim();
        if trimmed.len() == 3 && trimmed.chars().all(|c| c.is_ascii_alphabetic()) {
             Ok(trimmed.to_uppercase())
        } else {
            Err(CurrencyCodeError)
        }
    }
}

// 5. Define the Newtype
pub type CurrencyCode = Tagged<String, DCurrencyCode, FCurrencyCode>;

// Usage
let usd = CurrencyCode::new("USD".to_string());
assert!(usd.is_ok());
assert_eq!(*usd.unwrap(), "USD");

let eur = CurrencyCode::new("  eur  ".to_string()); // Whitespace stripped, uppercased
assert!(eur.is_ok());
assert_eq!(*eur.unwrap(), "EUR");

let err = CurrencyCode::new("us".to_string()); // Too short
assert_eq!(err.unwrap_err(), CurrencyCodeError);

let err = CurrencyCode::new("123".to_string()); // Not letters
assert_eq!(err.unwrap_err(), CurrencyCodeError);
```

## Common Refineries

`functora-tagged` provides a set of common, ready-to-use refineries in the `common` module (`src/common.rs`). These allow you to quickly create refined newtypes with zero boilerplate.

-   **`FCrude`**: No-op refinery. Used when you only need a distinct type without refinement. `RefineError` is `Infallible`.
-   **`FPositive`**: Ensures the value is strictly greater than zero (`> 0`).
-   **`FNonNeg`**: Ensures the value is non-negative (`>= 0`).
-   **`FNonEmpty`**: Ensures the value is not empty, i.e. the length is `> 0`.

## Derives

`functora-tagged` provides blanket implementations for several essential traits. These traits work seamlessly with your newtypes, respecting the behavior of the underlying representation and the refinement rules defined by the `F` type's implementation of `Refine<T>`.

### Direct

- `Eq`
- `PartialEq`
- `Ord`
- `PartialOrd`
- `Clone`
- `Debug`
- `serde::Serialize` (with `serde` feature)
- `diesel::serialize::ToSql` (with `diesel` feature)
- `diesel::expression::AsExpression` (with `diesel` feature)

### Refined

- `FromStr`: Implemented for `Tagged<T, D, F>` and `ViaString<T, D, F>`. Returns a `ParseError<T, D, F>`, which can be an upstream `Decode` error (from `T::from_str`) or a `Refine` error (from `F::refine`).
- `serde::Deserialize` (with `serde` feature)
- `diesel::Queryable` (with `diesel` feature)
- `diesel::deserialize::FromSql` (with `diesel` feature)

## Integrations

`functora-tagged` provides optional integrations for common Rust ecosystems:

- **`serde`**: For serialization and deserialization. Enable with the `serde` feature.
- **`diesel`**: For database interactions. Enable with the `diesel` feature.

These integrations respect the `Refine` rules defined for your types.

## Recipes

You can promote `rep` values into newtypes using `Tagged::new(rep)`. To demote a newtype back to its representation, use the `.rep()` method to get a reference, or `.untag()` to consume it. You can also use the `Deref` trait (via the `*` operator) to access the underlying value if it implements `Copy`. Standard serializers and deserializers available for `Rep` work directly with the newtype as well.

### Simple Newtype

When you don't need refinement, use `FCrude` from the `common` module.

```rust
use functora_tagged::*;

pub enum DUserId {}

pub type UserId = Tagged<u64, DUserId, FCrude>;

let id = UserId::new(12345).infallible();
assert_eq!(*id, 12345);
```

### Refined Newtype

This example demonstrates ensuring numeric types are positive using `FPositive`.

```rust
use functora_tagged::*;

// The Dimension
#[derive(PartialEq, Debug)]
pub enum DCount {}

// Use common FPositive refinery
pub type PositiveCount = Tagged<usize, DCount, FPositive>;

let rep = 100;
let new = PositiveCount::new(rep).unwrap();
assert_eq!(*new, rep);

// FPositive returns PositiveError on failure
let err = PositiveCount::new(0).unwrap_err();
assert_eq!(err, PositiveError(0));
```

### Generic Newtype

This demonstrates a generic `PositiveAmount<T>` newtype that enforces positive values for any numeric type `T` that satisfies `FPositive`.

```rust
use functora_tagged::*;

#[derive(Debug)]
pub enum DAmount {} // Dimension

pub type PositiveAmount<T> = Tagged<T, DAmount, FPositive>;

// Works with i32
let rep = 100;
let new = PositiveAmount::<i32>::new(rep).unwrap();
assert_eq!(*new, rep);

// Works with f64
let rep = 10.5;
let new = PositiveAmount::<f64>::new(rep).unwrap();
assert_eq!(*new, rep);

// Refinement fails
let err = PositiveAmount::<i32>::new(-5).unwrap_err();
assert_eq!(err, PositiveError(-5));

let err = PositiveAmount::<f64>::new(0.0).unwrap_err();
assert_eq!(err, PositiveError(0.0));
```

### Composite Newtype

This example demonstrates how to combine multiple refinement rules (e.g., `NonNeg` and `Max 100`) into a single, flat `Tagged` type using a composite refinery. This avoids the complexity of nesting `Tagged` types.

```rust
use functora_tagged::*;

#[derive(Debug)]
pub enum DScore {} // Dimension
#[derive(Debug)]
pub enum FScore {} // Refinery

// Flat structure: one Tagged wrapper
pub type Score = Tagged<i32, DScore, FScore>;

#[derive(Debug, PartialEq)]
pub enum ScoreError {
    NonNeg(NonNegError<i32>),
    TooHigh(i32),
}

impl Refine<i32> for FScore {
    type RefineError = ScoreError;

    fn refine(rep: i32) -> Result<i32, Self::RefineError> {
        // Reuse FNonNeg logic first (DRY)
        let val = FNonNeg::refine(rep).map_err(ScoreError::NonNeg)?;

        // Then apply custom max check
        if val <= 100 {
            Ok(val)
        } else {
            Err(ScoreError::TooHigh(val))
        }
    }
}

let val = 85;
let score = Score::new(val).unwrap();
assert_eq!(*score, val);

let err = Score::new(-10).unwrap_err();
assert_eq!(err, ScoreError::NonNeg(NonNegError(-10)));

let err = Score::new(101).unwrap_err();
assert_eq!(err, ScoreError::TooHigh(101));
```

## Dimensional

`functora-tagged` includes a `num` module that enables type-safe dimensional analysis and arithmetic. It prevents accidental unit mixing (e.g., adding meters to seconds) and ensures that operations produce correctly typed results (e.g., dividing meters by seconds yields velocity).

The system is built on four core algebraic types that carry unit information in their `PhantomData`:

- **`Identity<I, F>`**: Represents a neutral element or a base scalar (like a dimensionless number).
- **`Atomic<A, F>`**: Represents a fundamental unit (e.g., Meter, Second, Kg).
- **`Times<L, R, F>`**: Represents the product of two units (e.g., Meter \* Meter = Area).
- **`Per<L, R, F>`**: Represents the quotient of two units (e.g., Meter / Second = Velocity).

All these types accept a refinement generic `F` (e.g., `FPositive`, `FNonNeg` etc.) to enforce constraints like non-negativity on the underlying values.

### Physics

Large dimensional systems can become verbose if the refinery `F` is repeated for every type. To solve this, `functora-tagged` provides the `Raffinate` trait. Each dimensional type (`Identity`, `Atomic`, `Times`, `Per`) implements `Raffinate`, which carries its own refinery through a `Refinery` associated type.

This allows the dimensional types to automatically derive the correct refinery. By defining a simple `Dim<D>` type alias, you can eliminate the need to repeat refinery `F` in your definitions:

```rust
use functora_tagged::*;
use rust_decimal::Decimal;

type Dim<D> =
    Tagged<Decimal, D, <D as Raffinate>::Refinery>;
```

This example demonstrates how to define physical units and calculate Kinetic Energy (**Ek = kg * (m/s)^2**) safely and concisely.

```rust
use functora_tagged::*;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

//
// 1. Generic dimensional type alias
//

type Dim<D> =
    Tagged<Decimal, D, <D as Raffinate>::Refinery>;

//
// 2. Dimensionless unit (Identity)
//

#[derive(Debug)]
pub enum INum {}
type DNum = Identity<INum, FCrude>;
type Num = Dim<DNum>;

//
// 3. Fundamental units (Atomic)
//

#[derive(Debug)]
pub enum AMeter {}
#[derive(Debug)]
pub enum ASecond {}
#[derive(Debug)]
pub enum AKg {}

type DMeter = Atomic<AMeter, FNonNeg>;
type DSecond = Atomic<ASecond, FNonNeg>;
type DKg = Atomic<AKg, FNonNeg>;

type Meter = Dim<DMeter>;
type Second = Dim<DSecond>;
type Kg = Dim<DKg>;

//
// 4. Composite units (Per, Times)
//

// Velocity = Meter / Second
type DVelocity = Per<DMeter, DSecond, FNonNeg>;
type Velocity = Dim<DVelocity>;

// Joule = Kg * Velocity^2
type DJoule = Times<
    DKg,
    Times<DVelocity, DVelocity, FNonNeg>,
    FNonNeg,
>;
type Joule = Dim<DJoule>;

//
// 5. Type-safe calculation
//

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let distance = Meter::new(dec!(50))?; // 50 meters
    let time = Second::new(dec!(5))?; // 5 seconds
    let mass = Kg::new(dec!(100))?; // 100 kg

    // Calculate velocity: 50m / 5s = 10 m/s
    let velocity: Velocity = distance.tdiv(&time)?;
    assert_eq!(*velocity, dec!(10));

    // Calculate Energy: 100kg * (10m/s)^2 = 10000J
    let energy: Joule =
        mass.tmul(&velocity.tmul(&velocity)?)?;
    assert_eq!(*energy, dec!(10000));

    // Scaling by a dimensionless 0.5 doesn't change the units
    let half = Num::new(dec!(0.5))?;
    let half_energy: Joule = energy.tmul(&half)?;
    assert_eq!(*half_energy, dec!(5000));

    Ok(())
}
```

<hr>

© 2025 [Functora](https://functora.github.io/). All rights reserved.
