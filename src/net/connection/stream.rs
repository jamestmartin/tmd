#[cfg(feature = "encryption")]
pub mod encrypted;

use tokio::io::{AsyncRead, AsyncWrite};

pub trait Stream: AsyncRead + AsyncWrite + Send + Unpin { }
impl<S: AsyncRead + AsyncWrite + Send + Unpin> Stream for S { }
