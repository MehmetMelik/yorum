use serde::{Deserialize, Serialize};
use std::fmt;

/// Source location span — tracks exact position in source for
/// every AST node, enabling lossless source ↔ AST round-tripping.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    /// Byte offset of the start of this span in the source text.
    pub start: usize,
    /// Byte offset of the end (exclusive) of this span.
    pub end: usize,
    /// 1-based line number of the start.
    pub line: u32,
    /// 1-based column number of the start.
    pub col: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32, col: u32) -> Self {
        Self {
            start,
            end,
            line,
            col,
        }
    }

    /// A zero-width span used during compiler-generated nodes.
    pub fn synthetic() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            col: 0,
        }
    }

    /// Merge two spans into one that covers both.
    pub fn merge(self, other: Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        let (line, col) = if self.start <= other.start {
            (self.line, self.col)
        } else {
            (other.line, other.col)
        };
        Span {
            start,
            end,
            line,
            col,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
