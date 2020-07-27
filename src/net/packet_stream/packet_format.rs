#[cfg(feature = "compression")]
mod compressed;
mod default;

use crate::net::packet_stream::packet_format::default::DefaultPacketFormat;
use crate::net::packet_stream::CompressionThreshold;
use async_trait::async_trait;
use std::io;
use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite};

pub type Reader = dyn AsyncRead + Unpin + Send;
pub type Writer = dyn AsyncWrite + Unpin + Send;

/// A packet format describes how to read a packet header and retrieve its data.
#[async_trait]
pub trait PacketFormat: Send + Sync {
    /// Receive the bytes of a packet's body (its id and header) from the provided stream.
    /// This involves reading the packet header and performing decompression if necessary.
    async fn receive(&self, src: &mut Reader) -> io::Result<Box<[u8]>>;

    /// Send the bytes of a packet's body (its id and header) through the provided stream.
    /// This involves writing the packet header and performing compression if necessary.
    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()>;
}

pub struct AutoPacketFormat(pub Option<CompressionThreshold>);

#[async_trait]
impl PacketFormat for AutoPacketFormat {
    async fn receive(&self, src: &mut Reader) -> io::Result<Box<[u8]>> {
        match self.0 {
            #[cfg(not(feature = "compression"))]
            Some(x) => x,
            #[cfg(feature = "compression")]
            Some(threshold) => {
                use crate::net::packet_stream::packet_format::compressed::CompressedPacketFormat;
                CompressedPacketFormat(threshold).receive(src).await
            },
            None => DefaultPacketFormat.receive(src).await,
        }
    }

    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()> {
        match self.0 {
            #[cfg(not(feature = "compression"))]
            Some(x) => x,
            #[cfg(feature = "compression")]
            Some(threshold) => {
                use crate::net::packet_stream::packet_format::compressed::CompressedPacketFormat;
                CompressedPacketFormat(threshold).send(dest, data).await
            },
            None => DefaultPacketFormat.send(dest, data).await,
        }
    }
}

/// A completely arbitrary limitation on the maximum size of a received packet.
pub const MAX_PACKET_SIZE: usize = 35565;

async fn read_varint(src: &mut Reader) -> io::Result<(usize, i32)> {
    let mut num_read: usize = 0;
    let mut acc = 0;
    while num_read < 5 {
        let byte = src.read_u8().await?;
        acc |= ((byte & 0b01111111) as i32) << (num_read * 7);

        num_read += 1;

        if byte & 0b10000000 == 0 {
            return Ok((num_read, acc));
        }
    }

    Err(io::Error::new(io::ErrorKind::Other, "VarInt was too long.".to_string()))
}
