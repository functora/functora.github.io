use functora_tagged::num::*;
use functora_tagged::refine::Refine;
use functora_tagged::tagged::Tagged;
use std::error::Error;

//
// NumError
//

#[test]
fn test_num_error_debug_and_display() {
    let e: NumError<NonNeg<i32>, i32> =
        NumError::Underflow(-1);
    assert!(format!("{:?}", e).contains("Underflow"));
    assert!(e.to_string().contains("Underflow"));
}

#[test]
fn test_num_error_impl_error() {
    fn assert_error<E: Error>() {}
    assert_error::<NumError<NonNeg<i32>, i32>>();
}

#[test]
fn test_num_error_variants_construct() {
    let _: NumError<NonNeg<i32>, i32> =
        NumError::Underflow(-5);
    let _: NumError<NonNeg<i32>, i32> = NumError::Add(
        NonNeg::new(10).unwrap(),
        NonNeg::new(20).unwrap(),
    );
    let _: NumError<NonNeg<i32>, i32> = NumError::Sub(
        NonNeg::new(10).unwrap(),
        NonNeg::new(20).unwrap(),
    );
    let _: NumError<NonNeg<i32>, i32> = NumError::Div(
        NonNeg::new(10).unwrap(),
        NonNeg::new(20).unwrap(),
    );
    let _: NumError<NonNeg<i32>, i32> =
        NumError::MulRep(NonNeg::new(10).unwrap(), 2);
    let _: NumError<NonNeg<i32>, i32> =
        NumError::DivRep(NonNeg::new(10).unwrap(), 2);
}

//
// NonNegTag / NonNeg<Rep>
//

#[test]
fn test_non_neg_refine_accepts_zero() {
    let r = NonNeg::new(0i32);
    assert!(r.is_ok());
    assert_eq!(r.unwrap().rep(), &0);
}

#[test]
fn test_non_neg_refine_accepts_positive() {
    let r = NonNeg::new(42i32);
    assert!(r.is_ok());
    assert_eq!(r.unwrap().rep(), &42);
}

#[test]
fn test_non_neg_refine_rejects_negative() {
    let r = NonNeg::new(-1i32);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, -1),
        _ => panic!("expected Underflow"),
    }
}

#[test]
fn test_non_neg_refine_rejects_negative_large() {
    let r = NonNeg::new(i32::MIN);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => {
            assert_eq!(rep, i32::MIN)
        }
        _ => panic!("expected Underflow"),
    }
}

//
// PosTag / Pos<Rep>
//

#[test]
fn test_pos_refine_rejects_zero() {
    let r = Pos::new(0i32);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, 0),
        _ => panic!("expected Underflow"),
    }
}

#[test]
fn test_pos_refine_rejects_negative() {
    let r = Pos::new(-1i32);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, -1),
        _ => panic!("expected Underflow"),
    }
}

#[test]
fn test_pos_refine_accepts_positive() {
    let r = Pos::new(1i32);
    assert!(r.is_ok());
    assert_eq!(r.unwrap().rep(), &1);
    let r = Pos::new(100i32);
    assert!(r.is_ok());
    assert_eq!(r.unwrap().rep(), &100);
}

//
// zero() and one()
//

#[test]
fn test_non_neg_zero() {
    let z: Result<NonNeg<i32>, _> = NonNeg::zero();
    assert!(z.is_ok());
    assert_eq!(z.unwrap().rep(), &0);
}

#[test]
fn test_non_neg_one() {
    let o: Result<NonNeg<i32>, _> = NonNeg::one();
    assert!(o.is_ok());
    assert_eq!(o.unwrap().rep(), &1);
}

#[test]
fn test_pos_zero_fails() {
    let z: Result<Pos<i32>, _> = Pos::zero();
    assert!(z.is_err());
    match z.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, 0),
        _ => panic!("expected Underflow(0)"),
    }
}

#[test]
fn test_pos_one() {
    let o: Result<Pos<i32>, _> = Pos::one();
    assert!(o.is_ok());
    assert_eq!(o.unwrap().rep(), &1);
}

//
// add()
//

#[test]
fn test_non_neg_add_ok() {
    let a = NonNeg::new(10).unwrap();
    let b = NonNeg::new(20).unwrap();
    let sum = a.add(&b).unwrap();
    assert_eq!(sum.rep(), &30);
}

#[test]
fn test_pos_add_ok() {
    let a = Pos::new(3).unwrap();
    let b = Pos::new(7).unwrap();
    let sum = a.add(&b).unwrap();
    assert_eq!(sum.rep(), &10);
}

#[test]
fn test_non_neg_add_overflow() {
    let a = NonNeg::new(i8::MAX).unwrap();
    let b = NonNeg::new(1i8).unwrap();
    let r = a.add(&b);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Add(x, y) => {
            assert_eq!(x.rep(), &i8::MAX);
            assert_eq!(y.rep(), &1);
        }
        _ => panic!("expected Add error"),
    }
}

//
// sub()
//

#[test]
fn test_non_neg_sub_ok() {
    let a = NonNeg::new(20).unwrap();
    let b = NonNeg::new(7).unwrap();
    let diff = a.sub(&b).unwrap();
    assert_eq!(diff.rep(), &13);
}

#[test]
fn test_non_neg_sub_equal_gives_zero() {
    let a = NonNeg::new(10).unwrap();
    let b = NonNeg::new(10).unwrap();
    let diff = a.sub(&b).unwrap();
    assert_eq!(diff.rep(), &0);
}

/// When lhs < rhs, checked_sub returns Some(negative); refinement then fails with Underflow.
#[test]
fn test_non_neg_sub_result_negative_refines_to_underflow() {
    let a = NonNeg::new(5).unwrap();
    let b = NonNeg::new(10).unwrap();
    let r = a.sub(&b);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, -5),
        e => panic!("expected Underflow, got {:?}", e),
    }
}

/// When lhs < rhs, same: result is negative, refinement fails with Underflow.
#[test]
fn test_pos_sub_result_negative_refines_to_underflow() {
    let a = Pos::new(1).unwrap();
    let b = Pos::new(2).unwrap();
    let r = a.sub(&b);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Underflow(rep) => assert_eq!(rep, -1),
        e => panic!("expected Underflow, got {:?}", e),
    }
}

/// NumError::Sub occurs when checked_sub returns None (signed overflow).
/// Use a tag that accepts full i8 range so we can have i8::MIN and trigger overflow.
#[test]
fn test_sub_overflow_returns_sub_error() {
    #[derive(Debug)]
    struct AllI8Tag;
    impl Refine<i8> for AllI8Tag {
        type RefineError =
            NumError<Tagged<i8, AllI8Tag>, i8>;
        fn refine(
            rep: i8,
        ) -> Result<i8, Self::RefineError> {
            Ok(rep)
        }
    }
    type AllI8 = Tagged<i8, AllI8Tag>;
    let a = AllI8::new(i8::MIN).unwrap();
    let b = AllI8::new(1).unwrap();
    let r = a.sub(&b);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Sub(x, y) => {
            assert_eq!(x.rep(), &i8::MIN);
            assert_eq!(y.rep(), &1);
        }
        e => panic!("expected Sub (overflow), got {:?}", e),
    }
}

//
// gap()
//

#[test]
fn test_non_neg_gap_positive() {
    let a = NonNeg::new(10).unwrap();
    let b = NonNeg::new(25).unwrap();
    let g = a.gap(&b).unwrap();
    assert_eq!(g.rep(), &15);
}

#[test]
fn test_non_neg_gap_reversed() {
    let a = NonNeg::new(25).unwrap();
    let b = NonNeg::new(10).unwrap();
    let g = a.gap(&b).unwrap();
    assert_eq!(g.rep(), &15);
}

#[test]
fn test_non_neg_gap_same() {
    let a = NonNeg::new(7).unwrap();
    let b = NonNeg::new(7).unwrap();
    let g = a.gap(&b).unwrap();
    assert_eq!(g.rep(), &0);
}

//
// div()
//

#[test]
fn test_non_neg_div_ok() {
    let a = NonNeg::new(20).unwrap();
    let b = NonNeg::new(4).unwrap();
    let q = a.div(&b).unwrap();
    assert_eq!(q, 5);
}

#[test]
fn test_pos_div_ok() {
    let a = Pos::new(15).unwrap();
    let b = Pos::new(3).unwrap();
    let q = a.div(&b).unwrap();
    assert_eq!(q, 5);
}

#[test]
fn test_non_neg_div_by_zero() {
    let a = NonNeg::new(10).unwrap();
    let b = NonNeg::new(0).unwrap();
    let r = a.div(&b);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::Div(x, y) => {
            assert_eq!(x.rep(), &10);
            assert_eq!(y.rep(), &0);
        }
        _ => panic!("expected Div error"),
    }
}

#[test]
fn test_div_returns_rep_not_tagged() {
    let a = NonNeg::new(10).unwrap();
    let b = NonNeg::new(2).unwrap();
    let q: i32 = a.div(&b).unwrap();
    assert_eq!(q, 5);
}

//
// mul_rep()
//

#[test]
fn test_non_neg_mul_rep_ok() {
    let a = NonNeg::new(6).unwrap();
    let p = a.mul_rep(&7).unwrap();
    assert_eq!(p.rep(), &42);
}

#[test]
fn test_pos_mul_rep_ok() {
    let a = Pos::new(3).unwrap();
    let p = a.mul_rep(&4).unwrap();
    assert_eq!(p.rep(), &12);
}

#[test]
fn test_non_neg_mul_rep_overflow() {
    let a = NonNeg::new(i8::MAX).unwrap();
    let r = a.mul_rep(&2);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::MulRep(t, rep) => {
            assert_eq!(t.rep(), &i8::MAX);
            assert_eq!(rep, 2);
        }
        _ => panic!("expected MulRep error"),
    }
}

//
// div_rep()
//

#[test]
fn test_non_neg_div_rep_ok() {
    let a = NonNeg::new(20).unwrap();
    let q = a.div_rep(&4).unwrap();
    assert_eq!(q.rep(), &5);
}

#[test]
fn test_pos_div_rep_ok() {
    let a = Pos::new(21).unwrap();
    let q = a.div_rep(&7).unwrap();
    assert_eq!(q.rep(), &3);
}

#[test]
fn test_non_neg_div_rep_by_zero() {
    let a = NonNeg::new(10).unwrap();
    let zero = 0i32;
    let r = a.div_rep(&zero);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::DivRep(t, rep) => {
            assert_eq!(t.rep(), &10);
            assert_eq!(rep, 0);
        }
        _ => panic!("expected DivRep error"),
    }
}

#[test]
fn test_pos_div_rep_by_zero() {
    let a = Pos::new(5).unwrap();
    let r = a.div_rep(&0);
    assert!(r.is_err());
    match r.unwrap_err() {
        NumError::DivRep(..) => {}
        _ => panic!("expected DivRep error"),
    }
}
