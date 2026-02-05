use functora::*;
use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::convert::Infallible;
use std::fmt::Debug;

type Test = Result<(), Box<dyn std::error::Error>>;

#[derive(Debug)]
pub enum DNum {}
#[derive(Debug)]
pub enum DUsd {}
#[derive(Debug)]
pub enum DEur {}
#[derive(Debug)]
pub enum DMeter {}
#[derive(Debug)]
pub enum DSecond {}

type Num = Tagged<Decimal, Identity<DNum>>;
impl Refine<Decimal> for DNum {
    type RefineError = Infallible;
}

type Usd = Tagged<Decimal, Prim<DUsd>>;
impl Refine<Decimal> for DUsd {
    type RefineError = NonNegError<Decimal>;
    fn refine(
        rep: Decimal,
    ) -> Result<Decimal, Self::RefineError> {
        NonNeg::refine(rep)
    }
}

type Eur = Tagged<Decimal, Prim<DEur>>;
impl Refine<Decimal> for DEur {
    type RefineError = NonNegError<Decimal>;
    fn refine(
        rep: Decimal,
    ) -> Result<Decimal, Self::RefineError> {
        NonNeg::refine(rep)
    }
}

type EurPerUsd =
    Tagged<Decimal, Per<Prim<DEur>, Prim<DUsd>, Pos>>;

type Meter = Tagged<Decimal, Prim<DMeter>>;
impl Refine<Decimal> for DMeter {
    type RefineError = NonNegError<Decimal>;
    fn refine(
        rep: Decimal,
    ) -> Result<Decimal, Self::RefineError> {
        NonNeg::refine(rep)
    }
}

type Second = Tagged<Decimal, Prim<DSecond>>;
impl Refine<Decimal> for DSecond {
    type RefineError = NonNegError<Decimal>;
    fn refine(
        rep: Decimal,
    ) -> Result<Decimal, Self::RefineError> {
        NonNeg::refine(rep)
    }
}

type Hertz = Tagged<
    Decimal,
    Per<Identity<DNum>, Prim<DSecond>, Pos>,
>;

type MeterPerSecond = Tagged<
    Decimal,
    Per<Prim<DMeter>, Prim<DSecond>, NonNeg>,
>;

type MeterTimesMeter = Tagged<
    Decimal,
    Times<Prim<DMeter>, Prim<DMeter>, NonNeg>,
>;

#[test]
fn area() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a: MeterTimesMeter = l.tmul(&w)?;
    assert_eq!(a.rep(), &dec!(50));
    assert_eq!(a.tdiv(&l)?, w);
    assert_eq!(a.tdiv(&w)?, l);
    ok()
}

#[test]
fn velocity() -> Test {
    let d = Meter::new(dec!(100))?;
    let t = Second::new(dec!(10))?;
    let v: MeterPerSecond = d.tdiv(&t)?;
    let h: Hertz = v.tdiv(&d)?;
    assert_eq!(v.rep(), &dec!(10));
    assert_eq!(v.tmul(&t)?, d);
    assert_eq!(t.tmul(&v)?, d);
    assert_eq!(h.rep(), &dec!(0.1));
    ok()
}

#[test]
fn velocity_cancel() -> Test {
    let v = MeterPerSecond::new(dec!(10))?;
    let t = Second::new(dec!(2))?;
    let d: Meter = v.tmul(&t)?;
    assert_eq!(d.rep(), &dec!(20));
    ok()
}

#[test]
fn exchange_rate() -> Test {
    let eur = Eur::new(dec!(100))?;
    let usd = Usd::new(dec!(1.2))?;
    let rate: EurPerUsd = eur.tdiv(&usd)?;
    assert_eq!(rate.rep(), &(dec!(100) / dec!(1.2)));
    ok()
}

#[test]
fn currency_convert() -> Test {
    let usd = Usd::new(dec!(400))?;
    let rate = EurPerUsd::new(dec!(0.9))?;
    let eur: Eur = usd.tmul(&rate)?;
    assert_eq!(eur.rep(), &dec!(360));
    ok()
}

#[test]
fn test_commutative() -> Test {
    let x = Meter::new(dec!(10))?;
    let y = Meter::new(dec!(5))?;
    // x + y = y + x
    assert_eq!(x.tadd(&y)?, y.tadd(&x)?);
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a1: MeterTimesMeter = l.tmul(&w)?;
    let a2: MeterTimesMeter = w.tmul(&l)?;
    // l * w = w * l
    assert_eq!(a1.rep(), a2.rep());
    ok()
}

#[test]
fn test_associative() -> Test {
    let a = Meter::new(dec!(10))?;
    let b = Meter::new(dec!(5))?;
    let c = Meter::new(dec!(2))?;
    // (a + b) + c = a + (b + c)
    assert_eq!(
        a.tadd(&b)?.tadd(&c)?,
        a.tadd(&b.tadd(&c)?)?
    );
    ok()
}

#[test]
fn test_distributive() -> Test {
    let a = Meter::new(dec!(10))?;
    let b = Meter::new(dec!(5))?;
    let c = Meter::new(dec!(2))?;
    let res1: MeterTimesMeter = a.tmul(&b.tadd(&c)?)?;
    let res2: MeterTimesMeter =
        a.tmul(&b)?.tadd(&a.tmul(&c)?)?;
    // a * (b + c) = a * b + a * c
    assert_eq!(res1, res2);
    ok()
}

#[test]
fn test_identity() -> Test {
    let x = Meter::new(dec!(10))?;
    let zero = Meter::new(dec!(0))?;
    // x + 0 = x
    assert_eq!(x.tadd(&zero)?, x);
    // 0 + x = x
    assert_eq!(zero.tadd(&x)?, x);
    let y = Meter::new(dec!(10))?;
    let one = Num::new(dec!(1))?;
    // y * 1 = y
    assert_eq!(y.tmul(&one)?, y);
    // 1 * y = y
    assert_eq!(one.tmul(&y)?, y);
    ok()
}

#[test]
fn test_cancellation() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let area: MeterTimesMeter = l.tmul(&w)?;
    // (L ✖ R) / L = R
    assert_eq!(w, area.tdiv(&l)?);
    // (L ✖ R) / R = L
    assert_eq!(l, area.tdiv(&w)?);
    ok()
}

#[test]
fn test_identity_division() -> Test {
    let m = Meter::new(dec!(10))?;
    let one = Num::new(dec!(1))?;
    // T / 1 = T
    assert_eq!(m, m.tdiv(&one)?);
    // 1 / T = 1 ÷ T
    let h: Hertz = one.tdiv(&Second::new(dec!(2))?)?;
    assert_eq!(h.rep(), &dec!(0.5));
    ok()
}
