pub trait NavCtx: Clone + 'static {
    fn push_route(&mut self, href: String);
    fn can_go_back(&self) -> bool;
    fn go_back(&mut self);
}
