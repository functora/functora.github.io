use functora::*;
use functora_tagged::num::*;
use functora_tagged::tagged::Tagged;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;

type Dim<D> = Tagged<Decimal, D>;

#[derive(Debug)]
pub enum INum {}
type DNum = Identity<INum, DAny>;
type Num = Dim<DNum>;

#[derive(Debug)]
pub enum AKg {}
type DKg = Atomic<AKg, DNonNeg>;
type Kg = Dim<DKg>;

#[derive(Debug)]
pub enum AMeter {}
type DMeter = Atomic<AMeter, DNonNeg>;
type Meter = Dim<DMeter>;

#[derive(Debug)]
pub enum ASecond {}
type DSecond = Atomic<ASecond, DNonNeg>;
type Second = Dim<DSecond>;

type DVelocity = Per<DMeter, DSecond, DNonNeg>;
type Velocity = Dim<DVelocity>;

type DArea = Times<DMeter, DMeter, DNonNeg>;
type Area = Dim<DArea>;

type DHertz = Per<DNum, DSecond, DNonNeg>;
type Hertz = Dim<DHertz>;

type DJoule = Times<
    DKg,
    Times<DVelocity, DVelocity, DNonNeg>,
    DNonNeg,
>;
type Joule = Dim<DJoule>;

#[derive(Debug)]
pub enum AUsd {}
type DUsd = Atomic<AUsd, DNonNeg>;
type Usd = Dim<DUsd>;

#[derive(Debug)]
pub enum AEur {}
type DEur = Atomic<AEur, DNonNeg>;
type Eur = Dim<DEur>;

type EurPerUsd = Dim<Per<DEur, DUsd, DPos>>;

type Test = Result<(), Box<dyn std::error::Error>>;

#[test]
fn area() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a: Area = l.tmul(&w)?;
    assert_eq!(a.rep(), &dec!(50));
    assert_eq!(a.tdiv(&l)?, w);
    assert_eq!(a.tdiv(&w)?, l);
    ok()
}

#[test]
fn velocity() -> Test {
    let d = Meter::new(dec!(100))?;
    let t = Second::new(dec!(10))?;
    let v: Velocity = d.tdiv(&t)?;
    let h: Hertz = v.tdiv(&d)?;
    assert_eq!(v.rep(), &dec!(10));
    assert_eq!(v.tmul(&t)?, d);
    assert_eq!(t.tmul(&v)?, d);
    assert_eq!(h.rep(), &dec!(0.1));
    ok()
}

#[test]
fn kinetic_energy() -> Test {
    let m: Kg = Kg::new(dec!(2))?;
    let d: Meter = Meter::new(dec!(10))?;
    let t: Second = Second::new(dec!(5))?;
    let v: Velocity = d.tdiv(&t)?;
    let e: Joule = m.tmul(&v.tmul(&v)?)?;
    assert_eq!(e.rep(), &dec!(8));
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
fn commutative() -> Test {
    let x = Meter::new(dec!(10))?;
    let y = Meter::new(dec!(5))?;
    // x + y = y + x
    assert_eq!(x.tadd(&y)?, y.tadd(&x)?);
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a1: Area = l.tmul(&w)?;
    let a2: Area = w.tmul(&l)?;
    // l * w = w * l
    assert_eq!(a1.rep(), a2.rep());
    ok()
}

#[test]
fn associative() -> Test {
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
fn distributive() -> Test {
    let a = Meter::new(dec!(10))?;
    let b = Meter::new(dec!(5))?;
    let c = Meter::new(dec!(2))?;
    let res1: Area = a.tmul(&b.tadd(&c)?)?;
    let res2: Area = a.tmul(&b)?.tadd(&a.tmul(&c)?)?;
    // a * (b + c) = a * b + a * c
    assert_eq!(res1, res2);
    ok()
}

#[test]
fn identity() -> Test {
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
fn cancellation() -> Test {
    let l = Meter::new(dec!(10))?;
    let w = Meter::new(dec!(5))?;
    let a: Area = l.tmul(&w)?;
    // (L ✖ R) / L = R
    assert_eq!(w, a.tdiv(&l)?);
    // (L ✖ R) / R = L
    assert_eq!(l, a.tdiv(&w)?);
    ok()
}

#[test]
fn identity_division() -> Test {
    let m = Meter::new(dec!(10))?;
    let one = Num::new(dec!(1))?;
    // T / 1 = T
    assert_eq!(m, m.tdiv(&one)?);
    // 1 / T = 1 ÷ T
    let h: Hertz = one.tdiv(&Second::new(dec!(2))?)?;
    assert_eq!(h.rep(), &dec!(0.5));
    ok()
}
