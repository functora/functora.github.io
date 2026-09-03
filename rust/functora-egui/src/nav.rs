use crate::route::Routable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HistoryPos(pub usize);

impl HistoryPos {
    #[must_use]
    pub fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct NavHistory<R: Routable> {
    stack: Vec<R>,
    pos: HistoryPos,
}

impl<R: Routable> NavHistory<R> {
    #[must_use]
    pub fn new(initial: R) -> Self {
        Self {
            stack: vec![initial],
            pos: HistoryPos(0),
        }
    }

    #[must_use]
    pub fn current(&self) -> &R {
        &self.stack[self.pos.0]
    }

    pub fn push(&mut self, route: R) {
        if self.stack[self.pos.0] == route {
            return;
        }
        self.stack.truncate(self.pos.0 + 1);
        self.stack.push(route);
        self.pos = HistoryPos(self.stack.len() - 1);
    }

    pub fn replace(&mut self, route: R) {
        self.stack[self.pos.0] = route;
    }

    pub fn go_back(&mut self) -> Option<&R> {
        if self.pos.0 > 0 {
            self.pos.0 -= 1;
            Some(self.current())
        } else {
            None
        }
    }

    pub fn go_forward(&mut self) -> Option<&R> {
        if self.pos.0 + 1 < self.stack.len() {
            self.pos.0 += 1;
            Some(self.current())
        } else {
            None
        }
    }

    #[must_use]
    pub fn can_go_back(&self) -> bool {
        self.pos.0 > 0
    }

    #[must_use]
    pub fn can_go_forward(&self) -> bool {
        self.pos.0 + 1 < self.stack.len()
    }

    pub fn truncate_forward(&mut self) {
        self.stack.truncate(self.pos.0 + 1);
    }

    pub fn reset(&mut self, route: R) {
        self.stack = vec![route];
        self.pos = HistoryPos(0);
    }

    pub fn sync(&mut self, route: &R) {
        if let Some(pos) = self.stack.iter().position(|r| r == route) {
            self.pos = HistoryPos(pos);
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
        self.pos.0
    }

    #[must_use]
    pub fn history_pos(&self) -> HistoryPos {
        self.pos
    }
}
