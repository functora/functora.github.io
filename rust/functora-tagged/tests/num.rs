use functora::*;
use functora_tagged::*;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;

type Dim<D> =
    Tagged<Decimal, D, <D as Raffinate>::Refinery>;

#[derive(Debug)]
pub enum INum {}
type DNum = Identity<INum, FCrude>;
type Num = Dim<DNum>;

#[derive(Debug)]
pub enum AKg {}
type DKg = Atomic<AKg, FNonNeg>;
type Kg = Dim<DKg>;

#[derive(Debug)]
pub enum AMeter {}
type DMeter = Atomic<AMeter, FNonNeg>;
type Meter = Dim<DMeter>;

#[derive(Debug)]
pub enum ASecond {}
type DSecond = Atomic<ASecond, FNonNeg>;
type Second = Dim<DSecond>;

type DVelocity = Per<DMeter, DSecond, FNonNeg>;
type Velocity = Dim<DVelocity>;

type DArea = Times<DMeter, DMeter, FNonNeg>;
type Area = Dim<DArea>;

type DHertz = Per<DNum, DSecond, FNonNeg>;
type Hertz = Dim<DHertz>;

type DJoule = Times<
    DKg,
    Times<DVelocity, DVelocity, FNonNeg>,
    FNonNeg,
>;
type Joule = Dim<DJoule>;

#[derive(Debug)]
pub enum AUsd {}
type DUsd = Atomic<AUsd, FNonNeg>;
type Usd = Dim<DUsd>;

#[derive(Debug)]
pub enum AEur {}
type DEur = Atomic<AEur, FNonNeg>;
type Eur = Dim<DEur>;

type EurPerUsd = Dim<Per<DEur, DUsd, FPositive>>;

type DVolume = Times<DArea, DMeter, FNonNeg>;
type Volume = Dim<DVolume>;

type DAcceleration = Per<DVelocity, DSecond, FNonNeg>;
type Acceleration = Dim<DAcceleration>;

type DForce = Times<DKg, DAcceleration, FNonNeg>;
type Force = Dim<DForce>;

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

#[test]
fn mul_permutations() -> Test {
    let one = Num::new(dec!(1))?;
    let m = Meter::new(dec!(2))?;
    let s = Second::new(dec!(2))?;
    let kg = Kg::new(dec!(5))?;
    let area = Area::new(dec!(6))?; // m^2
    let vel = Velocity::new(dec!(4))?; // m/s

    // Identity * Times
    let vol: Volume = one.tmul(&area.tmul(&m)?)?;
    assert_eq!(vol.rep(), &dec!(12));

    // Identity * Per
    let acc: Acceleration = one.tmul(&vel.tdiv(&s)?)?;
    assert_eq!(acc.rep(), &(dec!(4) / dec!(2)));

    // Atomic * Times
    let force: Force = kg.tmul(&acc)?;
    assert_eq!(
        force.rep(),
        &((dec!(5) * dec!(4)) / dec!(2))
    );

    // Atomic * Per
    let momentum: Tagged<
        Decimal,
        Times<DKg, DVelocity, FNonNeg>,
        FNonNeg,
    > = kg.tmul(&vel)?;
    assert_eq!(momentum.rep(), &dec!(20));

    // Times * Atomic
    let force2: Tagged<
        Decimal,
        Times<DAcceleration, DKg, FNonNeg>,
        FNonNeg,
    > = acc.tmul(&kg)?; // Commutative check roughly
    assert_eq!(force2.rep(), force.rep());

    // Per * Atomic (Non-cancelling checked in velocity test)
    // Per * Per
    let acc2: Tagged<
        Decimal,
        Times<DVelocity, DHertz, FNonNeg>,
        FNonNeg,
    > = vel.tmul(&Hertz::new(dec!(1))?)?;
    assert_eq!(acc2.rep(), &dec!(4));

    ok()
}

#[test]
fn div_permutations() -> Test {
    let one = Num::new(dec!(1))?;
    let m = Meter::new(dec!(10))?;
    let s = Second::new(dec!(2))?;
    let area = Area::new(dec!(100))?;
    let vel = Velocity::new(dec!(5))?;

    // Identity / Times
    let per_area: Tagged<
        Decimal,
        Per<DNum, DArea, FNonNeg>,
        FNonNeg,
    > = one.tdiv(&area)?;
    assert_eq!(per_area.rep(), &dec!(0.01));

    // Identity / Per
    let s_per_m: Tagged<
        Decimal,
        Per<DNum, DVelocity, FNonNeg>,
        FNonNeg,
    > = one.tdiv(&vel)?;
    assert_eq!(s_per_m.rep(), &dec!(0.2));

    // Atomic / Times
    let per_m: Tagged<
        Decimal,
        Per<DNum, DMeter, FNonNeg>,
        FNonNeg,
    > = m.tdiv(&area)?;
    assert_eq!(per_m.rep(), &dec!(0.1));

    // Times / Times
    let ratio: Tagged<
        Decimal,
        Per<DArea, DArea, FNonNeg>,
        FNonNeg,
    > = area.tdiv(&area)?;
    assert_eq!(ratio.rep(), &dec!(1));

    // Times / Per
    let acc_time: Tagged<
        Decimal,
        Per<DVelocity, DSecond, FNonNeg>,
        FNonNeg,
    > = vel.tdiv(&s)?;
    assert_eq!(acc_time.rep(), &dec!(2.5));

    ok()
}

#[test]
fn cancellation_edge_cases() -> Test {
    let m = Meter::new(dec!(10))?;
    let s = Second::new(dec!(2))?;
    let vel = Velocity::new(dec!(5))?;

    // L / (L * R)
    let ms: Tagged<
        Decimal,
        Times<DMeter, DSecond, FNonNeg>,
        FNonNeg,
    > = m.tmul(&s)?;
    let per_s_val: Tagged<
        Decimal,
        Per<DNum, DSecond, FPositive>,
        FPositive,
    > = m.tdiv(&ms)?;
    assert_eq!(per_s_val.rep(), &dec!(0.5));

    // L / (L / R) = R
    let s2: Second = m.tdiv(&vel)?; // m / (m/s) = s
    assert_eq!(s2.rep(), &dec!(2));

    // (L / R) / L = 1/R
    let inv_s: Hertz = vel.tdiv(&m)?;
    assert_eq!(inv_s.rep(), &dec!(0.5));

    ok()
}

#[test]
fn test_polymorphism() -> Test {
    let m1 = Meter::new(dec!(10))?;
    let m2 = Meter::new(dec!(5))?;

    // TAdd
    let _: Meter = m1.tadd(m2)?;
    let _: Meter = m1.tadd(&m2)?;
    let _: Meter = (&m1).tadd(m2)?;
    let _: Meter = (&m1).tadd(&m2)?;

    // TSub
    let _: Meter = m1.tsub(m2)?;
    let _: Meter = m1.tsub(&m2)?;
    let _: Meter = (&m1).tsub(m2)?;
    let _: Meter = (&m1).tsub(&m2)?;

    // TGap
    let _: Meter = m1.tgap(m2)?;
    let _: Meter = m1.tgap(&m2)?;
    let _: Meter = (&m1).tgap(m2)?;
    let _: Meter = (&m1).tgap(&m2)?;

    let one = Num::new(dec!(1))?;

    // TMul
    let _: Meter = m1.tmul(one)?;
    let _: Meter = m1.tmul(&one)?;
    let _: Meter = (&m1).tmul(one)?;
    let _: Meter = (&m1).tmul(&one)?;

    // TDiv
    let _: Meter = m1.tdiv(one)?;
    let _: Meter = m1.tdiv(&one)?;
    let _: Meter = (&m1).tdiv(one)?;
    let _: Meter = (&m1).tdiv(&one)?;

    ok()
}

#[test]
fn test_primitive_polymorphism() -> Test {
    let x = dec!(10);
    let y = dec!(5);

    // TAdd
    let _: Decimal = x.tadd(y)?;
    let _: Decimal = x.tadd(&y)?;
    let _: Decimal = (&x).tadd(y)?;
    let _: Decimal = (&x).tadd(&y)?;

    // TSub
    let _: Decimal = x.tsub(y)?;
    let _: Decimal = x.tsub(&y)?;
    let _: Decimal = (&x).tsub(y)?;
    let _: Decimal = (&x).tsub(&y)?;

    // TGap
    let _: Decimal = x.tgap(y)?;
    let _: Decimal = x.tgap(&y)?;
    let _: Decimal = (&x).tgap(y)?;
    let _: Decimal = (&x).tgap(&y)?;

    // TMul
    let _: Decimal = x.tmul(y)?;
    let _: Decimal = x.tmul(&y)?;
    let _: Decimal = (&x).tmul(y)?;
    let _: Decimal = (&x).tmul(&y)?;

    // TDiv
    let _: Decimal = x.tdiv(y)?;
    let _: Decimal = x.tdiv(&y)?;
    let _: Decimal = (&x).tdiv(y)?;
    let _: Decimal = (&x).tdiv(&y)?;

    ok()
}
