pub trait Refine<Rep>: Sized {
    type RefineError;

    fn refine(rep: Rep) -> Result<Rep, Self::RefineError> {
        Ok(rep)
    }
}
