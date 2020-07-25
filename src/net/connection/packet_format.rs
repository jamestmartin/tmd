pub mod compressed;
pub mod default;

use async_trait::async_trait;
use crate::net::{Reader, Writer};
use std::io;
use tokio::io::AsyncReadExt;

#[async_trait]
pub trait PacketFormat: Send + Sync {
    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()>;
    async fn recieve(&self, src: &mut Reader) -> io::Result<Box<[u8]>>;
}

pub const MAX_CLIENT_PACKET_SIZE: usize = 32767;

pub async fn read_varint(src: &mut Reader) -> io::Result<(usize, i32)> {
    let mut length = 1;
    let mut acc = 0;
    while length <= 5 {
        let byte = src.read_u8().await?;
        acc |= (byte & 0b01111111) as i32;

        if byte & 0b10000000 == 0 {
            return Ok((length, acc));
        }

        acc = acc << 7;
        length += 1;
    }

    Err(io::Error::new(io::ErrorKind::Other, "VarInt was too long.".to_string()))
}
