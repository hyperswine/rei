# Common types and traits
# Re-exported from core

// pub, internal keywords

pub use core::types::*

pub trait Range {
    fn index(start: UInt, end: UInt=())
}
