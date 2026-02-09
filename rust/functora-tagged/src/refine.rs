pub trait Refine<T>: Sized {
    type RefineError;

    fn refine(rep: T) -> Result<T, Self::RefineError> {
        Ok(rep)
    }
}
