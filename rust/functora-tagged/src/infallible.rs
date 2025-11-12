use std::convert::Infallible;

pub trait InfallibleInto<T> {
    fn infallible(self) -> T;
}

impl<T> InfallibleInto<T> for Result<T, Infallible> {
    fn infallible(self) -> T {
        self.unwrap()
    }
}
