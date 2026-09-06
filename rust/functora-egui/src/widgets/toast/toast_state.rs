//! Toast notification state manager.

/// Manages active toast notifications.
#[derive(Clone, Default)]
pub struct ToastState {
    pub(crate) toasts: Vec<super::toast_entry::ToastEntry>,
    next_id: u64,
}

impl ToastState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn next_id(&self) -> u64 {
        self.next_id
    }

    /// Adds a toast notification. Uses context time for creation timestamp.
    pub fn add(
        &mut self,
        title: impl Into<String>,
        variant: crate::tokens::toast_variant::ToastVariant,
        time: f64,
    ) {
        let id = self.next_id;
        self.next_id += 1;
        self.toasts.push(super::toast_entry::ToastEntry {
            id,
            title: title.into(),
            description: None,
            variant,
            created_at: time,
            duration_secs: 4.0,
        });
    }

    /// Adds a toast with description.
    pub fn add_with_description(
        &mut self,
        title: impl Into<String>,
        description: impl Into<String>,
        variant: crate::tokens::toast_variant::ToastVariant,
        time: f64,
    ) {
        let id = self.next_id;
        self.next_id += 1;
        self.toasts.push(super::toast_entry::ToastEntry {
            id,
            title: title.into(),
            description: Some(description.into()),
            variant,
            created_at: time,
            duration_secs: 4.0,
        });
    }

    /// Removes expired toasts.
    pub fn cleanup(&mut self, current_time: f64) {
        self.toasts
            .retain(|t| current_time - t.created_at < t.duration_secs);
    }

    #[must_use]
    pub fn next_y_offset(&self) -> f32 {
        let mut y_offset = -16.0;
        for toast in &self.toasts {
            let h = if toast.description.is_some() {
                72.0
            } else {
                52.0
            };
            y_offset -= h + 8.0;
        }
        y_offset
    }
}
