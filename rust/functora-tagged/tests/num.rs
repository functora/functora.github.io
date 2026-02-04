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
pub enum DEurPerUsd {}
#[derive(Debug)]
pub enum DMeter {}
#[derive(Debug)]
pub enum DSecond {}
#[derive(Debug)]
pub enum DFreePerSecond {}
#[derive(Debug)]
pub enum DMeterPerSecond {}
#[derive(Debug)]
pub enum DMeterTimesMeter {}

type Num = Tagged<Decimal, Identity<DNum>>;
impl Refine<Decimal> for DNum {
    type RefineError = Infallible;
}

type Usd = Tagged<Decimal, Prim<DUsd>>;
impl Refine<Decimal> for DUsd {
    type RefineError = Infallible;
}

type Eur = Tagged<Decimal, Prim<DEur>>;
impl Refine<Decimal> for DEur {
    type RefineError = Infallible;
}

type EurPerUsd = Tagged<
    Decimal,
    Per<Prim<DEur>, Prim<DUsd>, DEurPerUsd>,
>;
impl Refine<Decimal> for DEurPerUsd {
    type RefineError = Infallible;
}

type Meter = Tagged<Decimal, Prim<DMeter>>;
impl Refine<Decimal> for DMeter {
    type RefineError = Infallible;
}

type Second = Tagged<Decimal, Prim<DSecond>>;
impl Refine<Decimal> for DSecond {
    type RefineError = Infallible;
}

type Hertz = Tagged<
    Decimal,
    Per<Identity<DNum>, Prim<DSecond>, DFreePerSecond>,
>;
impl Refine<Decimal> for DFreePerSecond {
    type RefineError = Infallible;
}

type MeterPerSecond = Tagged<
    Decimal,
    Per<Prim<DMeter>, Prim<DSecond>, DMeterPerSecond>,
>;
impl Refine<Decimal> for DMeterPerSecond {
    type RefineError = Infallible;
}

type MeterTimesMeter = Tagged<
    Decimal,
    Times<Prim<DMeter>, Prim<DMeter>, DMeterTimesMeter>,
>;
impl Refine<Decimal> for DMeterTimesMeter {
    type RefineError = Infallible;
}

#[test]
fn area() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a: MeterTimesMeter = l.fmul(&w)?;
    assert_eq!(a.rep(), &dec!(50));
    assert_eq!(a.fdiv(&l)?, w);
    assert_eq!(a.fdiv(&w)?, l);
    ok()
}

#[test]
fn velocity() -> Test {
    let d = Meter::new(dec!(100))?;
    let t = Second::new(dec!(10))?;
    let v: MeterPerSecond = d.fdiv(&t)?;
    let h: Hertz = v.fdiv(&d)?;
    assert_eq!(v.rep(), &dec!(10));
    assert_eq!(v.fmul(&t)?, d);
    assert_eq!(t.fmul(&v)?, d);
    assert_eq!(h.rep(), &dec!(0.1));
    ok()
}

#[test]
fn velocity_cancel() -> Test {
    let v = MeterPerSecond::new(dec!(10))?;
    let t = Second::new(dec!(2))?;
    let d: Meter = v.fmul(&t)?;
    assert_eq!(d.rep(), &dec!(20));
    ok()
}

#[test]
fn exchange_rate() -> Test {
    let eur = Eur::new(dec!(100))?;
    let usd = Usd::new(dec!(1.2))?;
    let rate: EurPerUsd = eur.fdiv(&usd)?;
    assert_eq!(rate.rep(), &(dec!(100) / dec!(1.2)));
    ok()
}

#[test]
fn currency_convert() -> Test {
    let usd = Usd::new(dec!(400))?;
    let rate = EurPerUsd::new(dec!(0.9))?;
    let eur: Eur = usd.fmul(&rate)?;
    assert_eq!(eur.rep(), &dec!(360));
    ok()
}

#[test]
fn test_commutative() -> Test {
    let x = Meter::new(dec!(10))?;
    let y = Meter::new(dec!(5))?;
    assert_eq!(x.fadd(&y)?, y.fadd(&x)?);
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a1: MeterTimesMeter = l.fmul(&w)?;
    let a2: MeterTimesMeter = w.fmul(&l)?;
    assert_eq!(a1.rep(), a2.rep());
    ok()
}

#[test]
fn test_associative() -> Test {
    let a = Meter::new(dec!(10))?;
    let b = Meter::new(dec!(5))?;
    let c = Meter::new(dec!(2))?;
    assert_eq!(
        a.fadd(&b)?.fadd(&c)?,
        a.fadd(&b.fadd(&c)?)?
    );
    ok()
}

#[test]
fn test_distributive() -> Test {
    let a = Meter::new(dec!(10))?;
    let b = Meter::new(dec!(5))?;
    let c = Meter::new(dec!(2))?;
    // a * (b + c)
    let res1: MeterTimesMeter = a.fmul(&b.fadd(&c)?)?;
    // a * b + a * c
    let res2: MeterTimesMeter =
        a.fmul(&b)?.fadd(&a.fmul(&c)?)?;
    assert_eq!(res1, res2);
    ok()
}

#[test]
fn test_identity() -> Test {
    // x + 0 = x
    let x = Meter::new(dec!(10))?;
    let zero = Meter::new(dec!(0))?;
    assert_eq!(x.fadd(&zero)?, x);
    assert_eq!(zero.fadd(&x)?, x);
    // y * 1 = y
    let y = Meter::new(dec!(10))?;
    let one = Num::new(dec!(1))?;
    assert_eq!(y.fmul(&one)?, y);
    assert_eq!(one.fmul(&y)?, y);
    ok()
}

#[test]
fn test_cancellation() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let area: MeterTimesMeter = l.fmul(&w)?;

    // Times / L = R
    let w_back: Meter = area.fdiv(&l)?;
    assert_eq!(w, w_back);

    // Times / R = L
    let l_back: Meter = area.fdiv(&w)?;
    assert_eq!(l, l_back);

    ok()
}

#[test]
fn test_identity_division() -> Test {
    let m = Meter::new(dec!(10))?;
    let one = Num::new(dec!(1))?;

    // Prim / Identity = Prim
    let m_back: Meter = m.fdiv(&one)?;
    assert_eq!(m, m_back);

    // Identity / Prim = Per<Identity, Prim>
    let h: Hertz = one.fdiv(&Second::new(dec!(2))?)?;
    assert_eq!(h.rep(), &dec!(0.5));

    ok()
}
