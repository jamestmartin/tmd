pub mod chat;
pub mod listener;
pub mod packet_stream;
pub mod protocol;
pub mod serialize;

use tokio::io::{AsyncRead, AsyncWrite};

pub trait Stream: AsyncRead + AsyncWrite + Send + Unpin {}
impl<S: AsyncRead + AsyncWrite + Send + Unpin> Stream for S {}
