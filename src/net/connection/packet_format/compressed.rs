use async_trait::async_trait;
use crate::net::{Reader, Writer};
use crate::net::connection::packet_format::{PacketFormat, MAX_CLIENT_PACKET_SIZE, read_varint};
use std::boxed::Box;
use std::io;
use tokio::io::AsyncReadExt;

pub struct CompressedPacketFormat {
    pub threshold: usize,
}

#[async_trait]
impl PacketFormat for CompressedPacketFormat {
    async fn send(&self, dest: &mut Writer, uncompressed_data: &[u8]) -> io::Result<()> {
        use crate::net::serialize::{PacketSerializer, VarInt};

        // If the length of the uncompressed packet is less than the threshold,
        // then we do not compress the packet and set the data_length field to 0.
        // Otherwise, data_length is set to the length of the uncompressed packet.
        let will_compress = uncompressed_data.len() >= self.threshold;

        let data_length = if will_compress { uncompressed_data.len() } else { 0 };
        let mut data_length_buf = Vec::with_capacity(5);
        data_length_buf.write(VarInt(data_length as i32));

        let mut compression_buf;
        let data = if will_compress {
            use flate2::{Compress, Compression, FlushCompress};

            // 1024 is just an arbitrary amount of extra space reserved
            // in case the output data ends up larger than the input data
            // (e.g. due to the zlib header).
            // FIXME: Further research to figure out the exact maximum capacity necessary.
            //   Perhaps you only need space for the header and the data itself can't get bigger?
            //   And what is the limit to how much bigger the data will get?
            //   Currently I don't actually know for a fact that this won't ever drop data.
            compression_buf = Vec::with_capacity(1024 + uncompressed_data.len());
            Compress::new(Compression::best(), true)
                .compress_vec(uncompressed_data, &mut compression_buf, FlushCompress::Finish)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            compression_buf.as_slice()
        } else {
            uncompressed_data
        };

        let mut packet_length_buf = Vec::with_capacity(5);
        packet_length_buf.write(VarInt((data_length_buf.len() + data.len()) as i32));

        {
            use tokio::io::AsyncWriteExt;

            dest.write(packet_length_buf.as_slice()).await?;
            dest.write(data_length_buf.as_slice()).await?;
            dest.write(data).await?;
            dest.flush().await?;
        }

        Ok(())
    }

    async fn recieve(&self, src: &mut Reader) -> io::Result<Box<[u8]>> {
        let (_, packet_length) = read_varint(src).await?;
        if packet_length < 0 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet length was negative."));
        }
        if packet_length > MAX_CLIENT_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet was too long."));
        }
        let packet_length = packet_length as usize;

        let (data_length_size, data_length) = read_varint(src).await?;
        if data_length < 0 {
            return Err(io::Error::new(io::ErrorKind::Other, "Data length was negative."));
        }
        if data_length > MAX_CLIENT_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Data was too long."));
        }
        let data_length = data_length as usize;

        let mut buf = Vec::with_capacity(packet_length);
        buf.resize(packet_length, 0);
        src.read_exact(buf.as_mut_slice()).await?;

        let decompressed_buf = if data_length != 0 {
            use flate2::{Decompress, FlushDecompress};

            let mut decompressed_buf = Vec::with_capacity(data_length);
            decompressed_buf.resize(data_length, 0);
            Decompress::new(true)
                .decompress(&buf, decompressed_buf.as_mut_slice(), FlushDecompress::Finish)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;
            decompressed_buf
        } else {
            let mut decompressed_buf = Vec::with_capacity(packet_length - data_length_size);
            decompressed_buf.copy_from_slice(&buf[data_length_size..]);
            decompressed_buf
        };

        Ok(decompressed_buf.into_boxed_slice())
    }
}
