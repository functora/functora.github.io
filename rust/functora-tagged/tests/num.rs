use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use num_traits::{
    CheckedAdd, CheckedDiv, CheckedMul, CheckedSub,
};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;
use std::ops::{Add, Div, Mul, Sub};

#[derive(
    Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd,
)]
pub struct Number(pub Decimal);

impl Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Number(self.0 + rhs.0)
    }
}

impl Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Number(self.0 - rhs.0)
    }
}

impl Mul for Number {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Number(self.0 * rhs.0)
    }
}

impl Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Number(self.0 / rhs.0)
    }
}

impl CheckedAdd for Number {
    fn checked_add(&self, v: &Self) -> Option<Self> {
        Some(Number(self.0 + v.0))
    }
}

impl CheckedSub for Number {
    fn checked_sub(&self, v: &Self) -> Option<Self> {
        Some(Number(self.0 - v.0))
    }
}

impl CheckedMul for Number {
    fn checked_mul(&self, v: &Self) -> Option<Self> {
        Some(Number(self.0 * v.0))
    }
}

impl CheckedDiv for Number {
    fn checked_div(&self, v: &Self) -> Option<Self> {
        if v.0.is_zero() {
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

impl Refine<Number> for Per<USD, EUR> {
    type RefineError = ();
}

impl Refine<Number> for Per<Number, Second> {
    type RefineError = ();
}

impl Refine<Number> for Times<Meter, Meter> {
    type RefineError = ();
}

impl Refine<Number> for Per<Number, Meter> {
    type RefineError = ();
}

impl Refine<Number> for Times<Meter, Second> {
    type RefineError = ();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FailNumber;

impl Add for FailNumber {
    type Output = Self;
    fn add(self, _: Self) -> Self::Output {
        FailNumber
    }
}

impl Sub for FailNumber {
    type Output = Self;
    fn sub(self, _: Self) -> Self::Output {
        FailNumber
    }
}

impl Mul for FailNumber {
    type Output = Self;
    fn mul(self, _: Self) -> Self::Output {
        FailNumber
    }
}

impl Div for FailNumber {
    type Output = Self;
    fn div(self, _: Self) -> Self::Output {
        FailNumber
    }
}

impl CheckedAdd for FailNumber {
    fn checked_add(&self, _: &Self) -> Option<Self> {
        None
    }
}

impl CheckedSub for FailNumber {
    fn checked_sub(&self, _: &Self) -> Option<Self> {
        None
    }
}

impl CheckedMul for FailNumber {
    fn checked_mul(&self, _: &Self) -> Option<Self> {
        None
    }
}

impl CheckedDiv for FailNumber {
    fn checked_div(&self, _: &Self) -> Option<Self> {
        None
    }
}

#[derive(Debug)]
pub enum LimitTag {}

impl Refine<Number> for LimitTag {
    type RefineError = ();
    fn refine(rep: Number) -> Result<Number, ()> {
        if rep.0 > dec!(100.0) {
            Err(())
        } else {
            Ok(rep)
        }
    }
}

impl Refine<Number> for Times<LimitTag, LimitTag> {
    type RefineError = ();
    fn refine(rep: Number) -> Result<Number, ()> {
        if rep.0 > dec!(100.0) {
            Err(())
        } else {
            Ok(rep)
        }
    }
}

impl Refine<Number> for Per<LimitTag, LimitTag> {
    type RefineError = ();
    fn refine(rep: Number) -> Result<Number, ()> {
        if rep.0 > dec!(100.0) {
            Err(())
        } else {
            Ok(rep)
        }
    }
}

impl Refine<FailNumber> for USD {
    type RefineError = ();
}

impl Refine<FailNumber> for Times<USD, USD> {
    type RefineError = ();
}

impl Refine<FailNumber> for Per<USD, USD> {
    type RefineError = ();
}

#[test]
fn test_partitioning() {
    // Dividing 10 items into 2 equal groups
    let items = Number(dec!(10.0));
    let groups = Number(dec!(2.0));
    let items_per_group: Number =
        items.fdiv(&groups).unwrap();
    assert_eq!(items_per_group, Number(dec!(5.0)));
}

#[test]
fn test_currency_ratio() {
    let lhs =
        Tagged::<Number, USD>::new(Number(dec!(100.0)))
            .unwrap();
    let rhs = Tagged::<Number, USD>::new(Number(dec!(2.0)))
        .unwrap();
    let res: Number = lhs.fdiv(&rhs).unwrap();
    assert_eq!(res, Number(dec!(50.0)));
}

#[test]
fn test_exchange_rate() {
    let lhs =
        Tagged::<Number, EUR>::new(Number(dec!(100.0)))
            .unwrap();
    let rhs = Tagged::<Number, USD>::new(Number(dec!(1.2)))
        .unwrap();
    let res: Tagged<Number, Per<EUR, USD>> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(100.0) / dec!(1.2)));
}

#[test]
fn test_velocity() {
    let lhs =
        Tagged::<Number, Meter>::new(Number(dec!(100.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, Second>::new(Number(dec!(10.0)))
            .unwrap();
    let res: Tagged<Number, Per<Meter, Second>> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(10.0)));
}

#[test]
fn test_stock_count_aggregation() {
    // Adding 10 items from one shelf and 2 from another
    let shelf1 = Number(dec!(10.0));
    let shelf2 = Number(dec!(2.0));
    let total_stock = shelf1.fadd(&shelf2).unwrap();
    assert_eq!(total_stock, Number(dec!(12.0)));
}

#[test]
fn test_fadd_tagged() {
    let lhs =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, Meter>::new(Number(dec!(2.0)))
            .unwrap();
    let res: Tagged<Number, Meter> =
        lhs.fadd(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(12.0)));
}

#[test]
fn test_inventory_reduction() {
    // Subtracting 2 sold items from initial stock of 10
    let initial_stock = Number(dec!(10.0));
    let sold_items = Number(dec!(2.0));
    let remaining_stock =
        initial_stock.fsub(&sold_items).unwrap();
    assert_eq!(remaining_stock, Number(dec!(8.0)));
}

#[test]
fn test_fsub_tagged() {
    let lhs =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, Meter>::new(Number(dec!(2.0)))
            .unwrap();
    let res: Tagged<Number, Meter> =
        lhs.fsub(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(8.0)));
}

#[test]
fn test_total_scaling() {
    // 10 boxes each containing 2 items
    let boxes = Number(dec!(10.0));
    let items_per_box = Number(dec!(2.0));
    let total_items = boxes.fmul(&items_per_box).unwrap();
    assert_eq!(total_items, Number(dec!(20.0)));
}

#[test]
fn test_fmul_tagged() {
    let lhs =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, Meter>::new(Number(dec!(2.0)))
            .unwrap();
    let res: Tagged<Number, Times<Meter, Meter>> =
        lhs.fmul(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(20.0)));
}

#[test]
fn test_fmul_per_cancel_2() {
    // (Meter / Second) * Second = Meter
    let lhs = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(10.0)),
    )
    .unwrap();
    let rhs =
        Tagged::<Number, Second>::new(Number(dec!(2.0)))
            .unwrap();
    let res: Tagged<Number, Meter> =
        lhs.fmul(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(20.0)));
}

#[test]
fn test_fdiv_cancel_1() {
    // Meter / (Meter / Second) = Second
    let lhs =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let rhs = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(2.0)),
    )
    .unwrap();
    let res: Tagged<Number, Second> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(5.0)));
}

#[test]
fn test_fdiv_cancel_2() {
    // (Meter / Second) / Meter = 1/Second (Per<Number, Second>)
    let lhs = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(10.0)),
    )
    .unwrap();
    let rhs =
        Tagged::<Number, Meter>::new(Number(dec!(2.0)))
            .unwrap();
    let res: Tagged<Number, Per<Number, Second>> =
        lhs.fdiv(&rhs).unwrap();
    assert_eq!(res.rep(), &Number(dec!(5.0)));
}

#[test]
fn test_physical_scaling() {
    // Scaling a 10-meter measurement by factor of 2
    let length =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let factor = Number(dec!(2.0));

    // Tag * Rep
    let scaled_up: Tagged<Number, Meter> =
        length.fmul(&factor).unwrap();
    assert_eq!(scaled_up.rep(), &Number(dec!(20.0)));

    // Rep * Tag
    let scaled_up_reverse: Tagged<Number, Meter> =
        factor.fmul(&length).unwrap();
    assert_eq!(
        scaled_up_reverse.rep(),
        &Number(dec!(20.0))
    );

    // Tag / Rep
    let scaled_down: Tagged<Number, Meter> =
        length.fdiv(&factor).unwrap();
    assert_eq!(scaled_down.rep(), &Number(dec!(5.0)));

    // Rep / Tag = 1/Tag (Per<Rep, Tag>)
    // Scaling a unit value: 2 / 10m = 0.2m^-1
    let inv_length: Tagged<Number, Per<Number, Meter>> =
        factor.fdiv(&length).unwrap();
    assert_eq!(inv_length.rep(), &Number(dec!(0.2)));
}

#[test]
fn test_division_by_zero_logic() {
    // Distributing 10 items among 0 people - should error
    let items = Number(dec!(10.0));
    let people = Number(dec!(0.0));
    let err = items.fdiv(&people).unwrap_err();
    match err {
        FNumError::Div(i, p) => {
            assert_eq!(i, Number(dec!(10.0)));
            assert_eq!(p, Number(dec!(0.0)));
        }
        _ => panic!("Expected FNumError::Div"),
    }
}

#[test]
fn test_fail_number_scenarios() {
    // Representing operations with faulty results
    let val = FailNumber;
    assert!(matches!(
        val.fadd(&val),
        Err(FNumError::Add(FailNumber, FailNumber))
    ));
    assert!(matches!(
        val.fsub(&val),
        Err(FNumError::Sub(FailNumber, FailNumber))
    ));
    assert!(matches!(
        val.fmul(&val),
        Err(FNumError::Mul(FailNumber, FailNumber))
    ));
    assert!(matches!(
        val.fdiv(&val),
        Err(FNumError::Div(FailNumber, FailNumber))
    ));
}

#[test]
fn test_incompatible_unit_scenarios() {
    // Preventing logical errors in unit-less operations with faulty types
    let shelf1 =
        Tagged::<FailNumber, USD>::new(FailNumber).unwrap();
    let shelf2 =
        Tagged::<FailNumber, USD>::new(FailNumber).unwrap();

    assert!(
        matches!(shelf1.fadd(&shelf2), Err(FNumError::Add(l, r)) if l == shelf1 && r == shelf2)
    );
    assert!(
        matches!(shelf1.fsub(&shelf2), Err(FNumError::Sub(l, r)) if l == shelf1 && r == shelf2)
    );

    assert!(
        matches!(shelf1.fmul(&shelf2), Err(FNumError::Mul(l, r)) if l == shelf1 && r == shelf2)
    );

    let res: Result<Tagged<FailNumber, Per<USD, USD>>, _> =
        shelf1.fdiv(&shelf2);
    assert!(
        matches!(res, Err(FNumError::Div(l, r)) if l == shelf1 && r == shelf2)
    );
}

#[test]
fn test_op_fail_tagged_refine() {
    let lhs =
        Tagged::<Number, LimitTag>::new(Number(dec!(60.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, LimitTag>::new(Number(dec!(60.0)))
            .unwrap();

    // Add: 60+60=120 > 100 -> Fail
    let err = lhs.fadd(&rhs).unwrap_err();
    match err {
        FNumError::Add(l, r) => {
            assert_eq!(l.rep(), &Number(dec!(60.0)));
            assert_eq!(r.rep(), &Number(dec!(60.0)));
        }
        _ => panic!("Expected FNumError::Add"),
    }

    // Sub: 150-40=110 > 100 -> Fail. Need lhs 150 (valid for LimitTag? No 150>100).
    // LimitTag prevents construction > 100.
    // So we can't test FSub *refine* failure with LimitTag unless we construct invalid Tagged.
    // Can we construct invalid Tagged? No, new() checks.
    // Unless we use unsafe or transmute, or a Refine that depends on external state.
    // OR: Refine allows 150 initially, but operation changes the logic? No.
    // Actually, FSub returns Self. If Rep - Rep is valid, and result is valid, it works.
    // If result invalid, it fails.
    // Example: LimitTag allows <= 100.
    // 50 - (-60) = 110. Fail.
    // Number wraps Decimal. -60 is valid.
    let lhs_sub =
        Tagged::<Number, LimitTag>::new(Number(dec!(50.0)))
            .unwrap();
    let rhs_sub = Tagged::<Number, LimitTag>::new(Number(
        dec!(-60.0),
    ))
    .unwrap();
    let err_sub = lhs_sub.fsub(&rhs_sub).unwrap_err();
    match err_sub {
        FNumError::Sub(l, r) => {
            assert_eq!(l.rep(), &Number(dec!(50.0)));
            assert_eq!(r.rep(), &Number(dec!(-60.0)));
        }
        _ => panic!("Expected FNumError::Sub"),
    }
}

#[test]
fn test_fmul_fail_tagged_refine() {
    let lhs =
        Tagged::<Number, LimitTag>::new(Number(dec!(10.0)))
            .unwrap();
    let rhs =
        Tagged::<Number, LimitTag>::new(Number(dec!(11.0)))
            .unwrap();
    // 10*11 = 110 > 100.
    let err = lhs.fmul(&rhs).unwrap_err();
    match err {
        FNumError::Mul(l, r) => {
            assert_eq!(l.rep(), &Number(dec!(10.0)));
            assert_eq!(r.rep(), &Number(dec!(11.0)));
        }
        _ => panic!("Expected FNumError::Mul"),
    }
}

#[test]
fn test_fdiv_fail_tagged_refine() {
    assert!(
        Tagged::<Number, LimitTag>::new(Number(dec!(
            220.0
        )))
        .is_err()
    );
    // Wait, can't create 220.
    // 100 / dec!(0.5) = 200 > 100.
    let lhs = Tagged::<Number, LimitTag>::new(Number(
        dec!(100.0),
    ))
    .unwrap();
    let rhs =
        Tagged::<Number, LimitTag>::new(Number(dec!(0.5)))
            .unwrap();
    let res: Result<
        Tagged<Number, Per<LimitTag, LimitTag>>,
        _,
    > = lhs.fdiv(&rhs);
    let err = res.unwrap_err();
    match err {
        FNumError::Div(l, r) => {
            assert_eq!(l.rep(), &Number(dec!(100.0)));
            assert_eq!(r.rep(), &Number(dec!(0.5)));
        }
        _ => panic!("Expected FNumError::Div"),
    }
}

#[test]
fn test_real_world_physics_kinematics() {
    let v0 = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(10.0)),
    )
    .unwrap();
    let v1 = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(30.0)),
    )
    .unwrap();
    let t =
        Tagged::<Number, Second>::new(Number(dec!(4.0)))
            .unwrap();

    // velocity change = v1 - v0
    let dv: Tagged<Number, Per<Meter, Second>> =
        v1.fsub(&v0).unwrap();
    assert_eq!(*dv.rep(), Number(dec!(20.0)));

    // acceleration gap (absolute difference) using fgap
    let v_gap: Tagged<Number, Per<Meter, Second>> =
        v1.fgap(&v0).unwrap();
    assert_eq!(*v_gap.rep(), Number(dec!(20.0)));

    // distance = velocity * time
    let d = v0.fmul(&t).unwrap();
    assert_eq!(*d.rep(), Number(dec!(40.0)));
}

impl Refine<Number> for Per<Per<Meter, Second>, Second> {
    type RefineError = ();
}

#[test]
fn test_real_world_physics_acceleration() {
    let v0 = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(10.0)),
    )
    .unwrap();
    let v1 = Tagged::<Number, Per<Meter, Second>>::new(
        Number(dec!(30.0)),
    )
    .unwrap();
    let dt =
        Tagged::<Number, Second>::new(Number(dec!(4.0)))
            .unwrap();
    let dv = v1.fsub(&v0).unwrap();

    // a = dv / dt -> Tagged<Number, Per<Per<Meter, Second>, Second>>
    let a: Tagged<Number, Per<Per<Meter, Second>, Second>> =
        dv.fdiv(&dt).unwrap();
    assert_eq!(*a.rep(), Number(dec!(5.0)));

    // v = a * dt -> Tagged<Number, Per<Meter, Second>>
    let v: Tagged<Number, Per<Meter, Second>> =
        a.fmul(&dt).unwrap();
    assert_eq!(*v.rep(), Number(dec!(20.0)));
}

#[test]
fn test_real_world_financial_accounting() {
    let rev =
        Tagged::<Number, USD>::new(Number(dec!(1000.0)))
            .unwrap();
    let exp =
        Tagged::<Number, USD>::new(Number(dec!(600.0)))
            .unwrap();

    // profit = rev - exp
    let profit: Tagged<Number, USD> =
        rev.fsub(&exp).unwrap();
    assert_eq!(*profit.rep(), Number(dec!(400.0)));

    // gap between rev and exp
    let gap: Tagged<Number, USD> = rev.fgap(&exp).unwrap();
    assert_eq!(*gap.rep(), Number(dec!(400.0)));

    // margin = profit / rev
    let margin: Number = profit.fdiv(&rev).unwrap();
    assert_eq!(margin, Number(dec!(0.4)));

    // conversion to EUR
    let rate = Tagged::<Number, Per<EUR, USD>>::new(
        Number(dec!(0.9)),
    )
    .unwrap();
    let profit_eur: Tagged<Number, EUR> =
        profit.fmul(&rate).unwrap();
    assert_eq!(*profit_eur.rep(), Number(dec!(360.0)));

    // conversion back to USD
    let profit_usd: Tagged<Number, USD> =
        profit_eur.fdiv(&rate).unwrap();
    assert_eq!(*profit_usd.rep(), Number(dec!(400.0)));
}

#[test]
fn test_real_world_composite_units() {
    let length =
        Tagged::<Number, Meter>::new(Number(dec!(10.0)))
            .unwrap();
    let width =
        Tagged::<Number, Meter>::new(Number(dec!(5.0)))
            .unwrap();

    // Area = length * width -> Tagged<Number, Times<Meter, Meter>>
    let area: Tagged<Number, Times<Meter, Meter>> =
        length.fmul(&width).unwrap();
    assert_eq!(*area.rep(), Number(dec!(50.0)));

    // Verification of Rep / Tag = Per<Rep, Tag>
    let scalar = Number(dec!(1.0));
    let inv_length: Tagged<Number, Per<Number, Meter>> =
        scalar.fdiv(&length).unwrap();
    assert_eq!(*inv_length.rep(), Number(dec!(0.1)));

    // Verification of Per<LTag, RTag> / LTag = Per<Rep, RTag>
    // e.g. (Meter / Second) / Meter = 1 / Second
    let velocity =
        Tagged::<Number, Per<Meter, Second>>::new(Number(
            dec!(10.0),
        ))
        .unwrap();
    let inv_time: Tagged<Number, Per<Number, Second>> =
        velocity.fdiv(&length).unwrap();
    assert_eq!(*inv_time.rep(), Number(dec!(1.0)));
}
