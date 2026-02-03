use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;

#[derive(Debug)]
pub enum USD {}
#[derive(Debug)]
pub enum EUR {}
#[derive(Debug)]
pub enum Meter {}
#[derive(Debug)]
pub enum Second {}

impl Refine<Decimal> for USD {
    type RefineError = std::convert::Infallible;
}
impl Refine<Decimal> for EUR {
    type RefineError = std::convert::Infallible;
}
impl Refine<Decimal> for Meter {
    type RefineError = std::convert::Infallible;
}
impl Refine<Decimal> for Second {
    type RefineError = std::convert::Infallible;
}

#[derive(Debug)]
pub enum MeterPerSecond {}

impl Refine<Decimal> for MeterPerSecond {
    type RefineError = std::convert::Infallible;
}

impl TagMul<Meter, Times<Meter, Meter>> for Meter {}

impl TagDiv<Second, Per<Meter, Second>> for Meter {}

impl TagDiv<Second, MeterPerSecond> for Meter {}

impl TagMul<Second, Meter> for Per<Meter, Second> {}

impl TagMul<Second, Meter> for MeterPerSecond {}

impl TagDiv<USD, Per<EUR, USD>> for EUR {}

impl TagMul<Per<EUR, USD>, EUR> for USD {}

fn tag<U: Refine<Decimal>>(v: Decimal) -> Tagged<Decimal, U>
where
    U::RefineError: Debug,
{
    Tagged::new(v).unwrap()
}

#[test]
fn area() {
    let l = tag::<Meter>(dec!(10));
    let w = tag::<Meter>(dec!(5));

    let a: Tagged<Decimal, Times<Meter, Meter>> =
        l.fmul(&w).unwrap();

    assert_eq!(a.rep(), &dec!(50));
}

#[test]
fn velocity() {
    let d = tag::<Meter>(dec!(100));
    let t = tag::<Second>(dec!(10));

    let v: Tagged<Decimal, Per<Meter, Second>> =
        d.fdiv(&t).unwrap();

    assert_eq!(v.rep(), &dec!(10));
}

#[test]
fn velocity_cancel() {
    let v = tag::<Per<Meter, Second>>(dec!(10));
    let t = tag::<Second>(dec!(2));

    let d: Tagged<Decimal, Meter> = v.fmul(&t).unwrap();

    assert_eq!(d.rep(), &dec!(20));
}

#[test]
fn velocity_isomorphic() {
    let d = tag::<Meter>(dec!(100));
    let t = tag::<Second>(dec!(10));

    let v: Tagged<Decimal, MeterPerSecond> =
        d.fdiv(&t).unwrap();

    assert_eq!(v.rep(), &dec!(10));

    let t2 = tag::<Second>(dec!(5));
    let d2: Tagged<Decimal, Meter> = v.fmul(&t2).unwrap();

    assert_eq!(d2.rep(), &dec!(50));
}

#[test]
fn exchange_rate() {
    let eur = tag::<EUR>(dec!(100));
    let usd = tag::<USD>(dec!(1.2));

    let r: Tagged<Decimal, Per<EUR, USD>> =
        eur.fdiv(&usd).unwrap();

    assert_eq!(r.rep(), &(dec!(100) / dec!(1.2)));
}

#[test]
fn currency_convert() {
    let profit = tag::<USD>(dec!(400));
    let rate = tag::<Per<EUR, USD>>(dec!(0.9));

    let profit_eur: Tagged<Decimal, EUR> =
        profit.fmul(&rate).unwrap();

    assert_eq!(profit_eur.rep(), &dec!(360));
}
