use crate::net::packet_stream::packet_format::{read_varint, PacketFormat, Reader, Writer, MAX_PACKET_SIZE};
use async_trait::async_trait;
use std::boxed::Box;
use std::io;

pub struct DefaultPacketFormat;

#[async_trait]
impl PacketFormat for DefaultPacketFormat {
    async fn receive(&self, src: &mut Reader) -> io::Result<Box<[u8]>> {
        use tokio::io::AsyncReadExt;

        let (_, length) = read_varint(src).await?;
        if length > MAX_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet was too long.".to_string()));
        }

        let mut buf = vec![0; length as usize];
        src.read_exact(buf.as_mut_slice()).await?;

        Ok(buf.into_boxed_slice())
    }

    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()> {
        use crate::net::serialize::{PacketSerializer, VarInt};

        let mut packet_length_buf = Vec::with_capacity(5);
        packet_length_buf.write(VarInt(data.len() as i32));

        {
            use tokio::io::AsyncWriteExt;

            dest.write(packet_length_buf.as_slice()).await?;
            dest.write(data).await?;
            dest.flush().await?;
        }

        Ok(())
    }
}
