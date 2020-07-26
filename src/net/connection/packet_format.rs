#[cfg(feature = "compression")]
pub mod compressed;
pub mod default;

use async_trait::async_trait;
use std::io;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite};

pub type Reader = dyn AsyncRead + Unpin + Send;
pub type Writer = dyn AsyncWrite + Unpin + Send;

#[async_trait]
pub trait PacketFormat: Send + Sync {
    async fn recieve(&self, src: &mut Reader) -> io::Result<Box<[u8]>>;
    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()>;
}

/// A completely arbitrary limitation on the maximum size of a recieved packet.
pub const MAX_PACKET_SIZE: usize = 35565;

pub async fn read_varint(src: &mut Reader) -> io::Result<(usize, i32)> {
    let mut num_read: usize = 0;
    let mut acc = 0;
    while num_read < 5 {
        let byte = src.read_u8().await?;
        acc |= ((byte & 0b01111111) as i32) << num_read * 7;

        num_read += 1;

        if byte & 0b10000000 == 0 {
            return Ok((num_read, acc));
        }
    }

    Err(io::Error::new(io::ErrorKind::Other, "VarInt was too long.".to_string()))
}
