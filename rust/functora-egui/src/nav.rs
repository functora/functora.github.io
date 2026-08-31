use crate::route::Routable;

#[derive(Debug, Clone)]
pub struct NavHistory<R: Routable> {
    stack: Vec<R>,
    pos: usize,
}

impl<R: Routable> NavHistory<R> {
    #[must_use]
    pub fn new(initial: R) -> Self {
        Self {
            stack: vec![initial],
            pos: 0,
        }
    }

    #[must_use]
    pub fn current(&self) -> &R {
        &self.stack[self.pos]
    }

    pub fn push(&mut self, route: R) {
        if self.stack[self.pos] == route {
            return;
        }
        self.stack.truncate(self.pos + 1);
        self.stack.push(route);
        self.pos = self.stack.len() - 1;
    }

    pub fn replace(&mut self, route: R) {
        self.stack[self.pos] = route;
    }

    pub fn go_back(&mut self) -> Option<&R> {
        if self.pos > 0 {
            self.pos -= 1;
            Some(self.current())
        } else {
            None
        }
    }

    pub fn go_forward(&mut self) -> Option<&R> {
        if self.pos + 1 < self.stack.len() {
            self.pos += 1;
            Some(self.current())
        } else {
            None
        }
    }

    #[must_use]
    pub fn can_go_back(&self) -> bool {
        self.pos > 0
    }

    #[must_use]
    pub fn can_go_forward(&self) -> bool {
        self.pos + 1 < self.stack.len()
    }

    pub fn truncate_forward(&mut self) {
        self.stack.truncate(self.pos + 1);
    }

    pub fn sync(&mut self, route: &R) {
        if let Some(pos) = self.stack.iter().position(|r| r == route) {
            self.pos = pos;
        } else {
            self.push(route.clone());
        }
    }

    #[must_use]
    pub fn stack(&self) -> &[R] {
        &self.stack
    }

    #[must_use]
    pub fn pos(&self) -> usize {
        self.pos
    }
}
