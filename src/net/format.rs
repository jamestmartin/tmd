use async_trait::async_trait;
use crate::net::{Reader, Writer};
use std::boxed::Box;
use std::io;
use tokio::io::AsyncReadExt;

#[async_trait]
pub trait PacketFormat {
    async fn send<'wr, 'w>(&self, dest: &'wr mut Writer<'w>, packet_id: i32, data: &[u8]) -> io::Result<()>;
    async fn recieve<'rr, 'r>(&self, src: &'rr mut Reader<'r>) -> io::Result<(i32, Box<[u8]>)>;
}

pub const MAX_CLIENT_PACKET_SIZE: usize = 32767;

pub struct DefaultPacketFormat;

async fn read_varint<'rr, 'r>(src: &'rr mut Reader<'r>) -> io::Result<(usize, i32)> {
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

    Err(io::Error::new(io::ErrorKind::Other, "VarInt was too long."))
}

#[async_trait]
impl PacketFormat for DefaultPacketFormat {
    async fn send<'wr, 'w>(&self, dest: &'wr mut Writer<'w>, packet_id: i32, data: &[u8]) -> io::Result<()> {
        use crate::net::serialize::{PacketSerializer, VarInt};

        let mut packet_id_buf = Vec::with_capacity(5);
        packet_id_buf.write(VarInt(packet_id));

        let packet_length = packet_id_buf.len() + data.len();
        let mut packet_length_buf = Vec::with_capacity(5);
        packet_length_buf.write(VarInt(packet_length as i32));

        {
            use tokio::io::AsyncWriteExt;

            dest.write(packet_length_buf.as_slice()).await?;
            dest.write(packet_id_buf.as_slice()).await?;
            dest.write(data).await?;
            dest.flush().await?;
        }

        Ok(())
    }

    async fn recieve<'rr, 'r>(&self, src: &'rr mut Reader<'r>) -> io::Result<(i32, Box<[u8]>)> {
        let (_, length) = read_varint(src).await?;
        if length > MAX_CLIENT_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet was too long."));
        }
        let length = length as usize;

        let (id_length, id) = read_varint(src).await?;

        let mut buf = Vec::with_capacity(length - id_length);
        buf.resize(length - id_length, 0);
        src.read_exact(buf.as_mut_slice()).await?;

        Ok((id, buf.into_boxed_slice()))
    }
}
