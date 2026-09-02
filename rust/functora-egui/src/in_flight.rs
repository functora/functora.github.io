use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

#[derive(Clone, Debug)]
pub struct InFlight {
    flag: Arc<AtomicBool>,
}

impl Default for InFlight {
    fn default() -> Self {
        Self {
            flag: Arc::new(AtomicBool::new(false)),
        }
    }
}

impl InFlight {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn claim(&self) -> Option<InFlightGuard> {
        self.flag
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .ok()
            .map(|_| InFlightGuard {
                flag: Arc::clone(&self.flag),
            })
    }

    #[must_use]
    pub fn is_in_flight(&self) -> bool {
        self.flag.load(Ordering::Acquire)
    }
}

#[derive(Debug)]
pub struct InFlightGuard {
    flag: Arc<AtomicBool>,
}

impl Drop for InFlightGuard {
    fn drop(&mut self) {
        self.flag.store(false, Ordering::Release);
    }
}
