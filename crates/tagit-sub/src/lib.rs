//! Re-exports [`tagit_sub_command`] and [`tagit_sub_impl`], while guaranteeing they're compatible
//! with each other.

pub use tagit_sub_command::SubtreeCommand;
#[cfg(feature = "impl")]
pub use tagit_sub_impl::sub;
