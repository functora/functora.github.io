use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;

#[derive(Debug)]
pub enum DFree {}
#[derive(Debug)]
pub enum DUsd {}
#[derive(Debug)]
pub enum DEur {}
#[derive(Debug)]
pub enum DEurPerDUsd {}
#[derive(Debug)]
pub enum DMeter {}
#[derive(Debug)]
pub enum DSecond {}
#[derive(Debug)]
pub enum DFreePerDSecond {}
#[derive(Debug)]
pub enum DMeterPerDSecond {}
#[derive(Debug)]
pub enum DMeterTimesDMeter {}

type Usd = Tagged<Decimal, DUsd>;
impl Refine<Decimal> for DUsd {
    type RefineError = std::convert::Infallible;
}

type Eur = Tagged<Decimal, DEur>;
impl Refine<Decimal> for DEur {
    type RefineError = std::convert::Infallible;
}

type EurPerUsd =
    Tagged<Decimal, Per<DEur, DUsd, DEurPerDUsd>>;
impl Refine<Decimal> for DEurPerDUsd {
    type RefineError = std::convert::Infallible;
}

type Meter = Tagged<Decimal, DMeter>;
impl Refine<Decimal> for DMeter {
    type RefineError = std::convert::Infallible;
}

type Second = Tagged<Decimal, DSecond>;
impl Refine<Decimal> for DSecond {
    type RefineError = std::convert::Infallible;
}

type Hertz =
    Tagged<Decimal, Per<DFree, DSecond, DFreePerDSecond>>;
impl Refine<Decimal> for DFreePerDSecond {
    type RefineError = std::convert::Infallible;
}

type MeterPerSecond =
    Tagged<Decimal, Per<DMeter, DSecond, DMeterPerDSecond>>;
impl Refine<Decimal> for DMeterPerDSecond {
    type RefineError = std::convert::Infallible;
}

type MeterTimesMeter = Tagged<
    Decimal,
    Times<DMeter, DMeter, DMeterTimesDMeter>,
>;
impl Refine<Decimal> for DMeterTimesDMeter {
    type RefineError = std::convert::Infallible;
}

impl IsScalar for DFree {}
impl IsScalar for DUsd {}
impl IsScalar for DEur {}
impl IsScalar for DMeter {}
impl IsScalar for DSecond {}

impl IsTimes for DMeterTimesDMeter {
    type L = DMeter;
    type R = DMeter;
}

impl IsPer for DMeterPerDSecond {
    type L = DMeter;
    type R = DSecond;
}

impl IsPer for DEurPerDUsd {
    type L = DEur;
    type R = DUsd;
}

impl IsPer for DFreePerDSecond {
    type L = DFree;
    type R = DSecond;
}

#[test]
fn area() {
    let l = Meter::new(dec!(10)).unwrap();
    let w = Meter::new(dec!(5)).unwrap();
    let a: MeterTimesMeter = l.fmul(&w).unwrap();
    assert_eq!(a.rep(), &dec!(50));
    assert_eq!(a.fdiv(&l).unwrap(), w);
    assert_eq!(a.fdiv(&w).unwrap(), l);
}

#[test]
fn velocity() {
    let d = Meter::new(dec!(100)).unwrap();
    let t = Second::new(dec!(10)).unwrap();
    let v: MeterPerSecond = d.fdiv(&t).unwrap();
    let h: Hertz = v.fdiv(&d).unwrap();
    assert_eq!(v.rep(), &dec!(10));
    assert_eq!(v.fmul(&t).unwrap(), d);
    assert_eq!(t.fmul(&v).unwrap(), d);
    assert_eq!(h.rep(), &dec!(0.1));
}

#[test]
fn velocity_cancel() {
    let v = MeterPerSecond::new(dec!(10)).unwrap();
    let t = Second::new(dec!(2)).unwrap();
    let d: Meter = v.fmul(&t).unwrap();
    assert_eq!(d.rep(), &dec!(20));
}

#[test]
fn exchange_rate() {
    let eur = Eur::new(dec!(100)).unwrap();
    let usd = Usd::new(dec!(1.2)).unwrap();
    let rate: EurPerUsd = eur.fdiv(&usd).unwrap();
    assert_eq!(rate.rep(), &(dec!(100) / dec!(1.2)));
}

#[test]
fn currency_convert() {
    let usd = Usd::new(dec!(400)).unwrap();
    let rate = EurPerUsd::new(dec!(0.9)).unwrap();
    let eur: Eur = usd.fmul(&rate).unwrap();
    assert_eq!(eur.rep(), &dec!(360));
}
