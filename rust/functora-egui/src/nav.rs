#[derive(Debug, Clone)]
pub struct NavStack<R> {
    stack: Vec<R>,
    idx: u32,
}

impl<R> NavStack<R>
where
    R: Default + PartialEq + Clone,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            stack: vec![R::default()],
            idx: 0,
        }
    }

    /// # Panics
    /// Panics if the stack is empty, which cannot happen for a correctly constructed `NavStack`.
    #[must_use]
    pub fn current(&self) -> &R {
        self.stack.last().unwrap_or_else(|| {
            panic!("NavStack empty");
        })
    }

    pub fn push(&mut self, route: R) {
        if route == R::default() {
            self.reset();
        } else {
            self.idx = self.idx.saturating_add(1);
            self.stack.push(route);
        }
    }

    pub fn push_route(&mut self, href: &str)
    where
        R: std::str::FromStr,
    {
        if let Ok(route) = href.parse::<R>() {
            self.push(route);
        }
    }

    pub fn go_back(&mut self) -> bool {
        if self.stack.len() > 1 {
            _ = self.stack.pop();
            self.idx = self.idx.saturating_sub(1);
            true
        } else {
            self.idx = 0;
            false
        }
    }

    #[must_use]
    pub fn can_go_back(&self) -> bool {
        self.stack.len() > 1
    }

    #[must_use]
    pub fn has_navigated(&self) -> bool {
        self.idx > 0
    }

    pub fn reset(&mut self) {
        self.stack.truncate(1);
        self.stack[0] = R::default();
        self.idx = 0;
    }

    pub fn increment(&mut self) {
        self.idx = self.idx.saturating_add(1);
    }

    pub fn decrement(&mut self) {
        self.idx = self.idx.saturating_sub(1);
    }

    #[must_use]
    pub fn stack(&self) -> &[R] {
        &self.stack
    }

    #[must_use]
    pub fn idx(&self) -> u32 {
        self.idx
    }
}

impl<R> Default for NavStack<R>
where
    R: Default + PartialEq + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}
