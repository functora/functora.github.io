use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use num_traits::CheckedDiv;
use std::fmt::Debug;
use std::ops::Div;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Number(pub f64);

impl Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Number(self.0 / rhs.0)
    }
}

impl CheckedDiv for Number {
    fn checked_div(&self, v: &Self) -> Option<Self> {
        if v.0 == 0.0 {
            None
        } else {
            Some(Number(self.0 / v.0))
        }
    }
}

#[derive(Debug)]
pub enum USD {}

#[derive(Debug)]
pub enum EUR {}

#[derive(Debug)]
pub enum Meter {}

#[derive(Debug)]
pub enum Second {}

impl Refine<Number> for USD {
    type RefineError = ();
}

impl Refine<Number> for EUR {
    type RefineError = ();
}

impl Refine<Number> for Meter {
    type RefineError = ();
}

impl Refine<Number> for Second {
    type RefineError = ();
}

impl Refine<Number> for Per<Meter, Second> {
    type RefineError = ();
}

impl Refine<Number> for Per<EUR, USD> {
    type RefineError = ();
}

#[test]
fn test_fdiv_number() {
    let lhs = Number(10.0);
    let rhs = Number(2.0);
    let res: Number = lhs.fdiv(&rhs).unwrap();
    assert_eq!(res, Number(5.0));
}

#[test]
fn test_currency_ratio() {
    let lhs =
        Tagged::<Number, USD>::new(Number(100.0)).unwrap();
    let rhs =
        Tagged::<Number, USD>::new(Number(2.0)).unwrap();
    let res: Number = lhs.fdiv(&rhs).unwrap();
    assert_eq!(res, Number(50.0));
}

#[test]
fn test_exchange_rate() {
    let lhs =
        Tagged::<Number, EUR>::new(Number(100.0)).unwrap();
    let rhs =
        Tagged::<Number, USD>::new(Number(1.2)).unwrap();
    let res: Tagged<Number, Per<EUR, USD>> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(100.0 / 1.2));
}

#[test]
fn test_velocity() {
    let lhs = Tagged::<Number, Meter>::new(Number(100.0))
        .unwrap();
    let rhs = Tagged::<Number, Second>::new(Number(10.0))
        .unwrap();
    let res: Tagged<Number, Per<Meter, Second>> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(10.0));
}
