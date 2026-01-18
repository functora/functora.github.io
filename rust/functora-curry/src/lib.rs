use std::marker::PhantomData;

pub trait Pi<'input> {
    type Output<'output>
    where
        'input: 'output;
}
pub struct PiVal<T>(PhantomData<T>);
impl<'input, T: 'input> Pi<'input> for PiVal<T> {
    type Output<'output>
        = T
    where
        'input: 'output;
}
pub struct PiRef<T: ?Sized>(PhantomData<T>);
impl<'input, T: 'input + ?Sized> Pi<'input> for PiRef<T> {
    type Output<'output>
        = &'output T
    where
        'input: 'output;
}
pub struct PiMut<T: ?Sized>(PhantomData<T>);
impl<'input, T: 'input + ?Sized> Pi<'input> for PiMut<T> {
    type Output<'output>
        = &'output mut T
    where
        'input: 'output;
}

pub trait CurryImpl<'input, A, B, C>
where
    B: Pi<'input>,
{
    fn curry_impl(
        self,
        a: A,
    ) -> impl for<'output> FnOnce(B::Output<'output>) -> C;
}

impl<'input, A, B, C, F> CurryImpl<'input, A, B, C> for F
where
    B: Pi<'input>,
    F: for<'output> FnOnce(A, B::Output<'output>) -> C,
{
    fn curry_impl(
        self,
        a: A,
    ) -> impl for<'output> FnOnce(B::Output<'output>) -> C
    {
        move |b| self(a, b)
    }
}

pub trait Curry<A, C> {
    fn curry<'input, B: Pi<'input>>(
        self,
        a: A,
    ) -> impl for<'output> FnOnce(B::Output<'output>) -> C
    where
        Self: CurryImpl<'input, A, B, C>;
}

impl<A, C, F> Curry<A, C> for F {
    fn curry<'input, B: Pi<'input>>(
        self,
        a: A,
    ) -> impl for<'output> FnOnce(B::Output<'output>) -> C
    where
        Self: CurryImpl<'input, A, B, C>,
    {
        self.curry_impl(a)
    }
}
