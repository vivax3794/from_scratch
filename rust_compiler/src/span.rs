//! Spans

/// A span
#[derive(Debug, Clone, Copy)]
pub struct Span {
    /// The start of the span
    pub from: usize,
    /// The end of the span
    pub to: usize,
}

/// A spanned value
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    /// The value
    pub value: T,
    /// The span
    pub span: Span,
}

impl Span {
    /// Create a new span
    pub const fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }
    ///
    /// Combine two spanned values
    pub const fn combine(&self, other: Span) -> Span {
        Span::new(self.from, other.to)
    }

    /// Map the value of the spanned value
    pub const fn with_value<R>(&self, value: R) -> Spanned<R> {
        Spanned::new(value, self.from, self.to)
    }
}

impl<T> Spanned<T> {
    /// Create a new spanned value
    pub const fn new(value: T, from: usize, to: usize) -> Self {
        Self {
            value,
            span: Span { from, to },
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        Self::new(value.from.into(), value.to - value.from)
    }
}
impl From<&Span> for miette::SourceSpan {
    fn from(value: &Span) -> Self {
        Self::new(value.from.into(), value.to - value.from)
    }
}
