use async_trait::async_trait;
use crate::net::connection::packet_format::
    {PacketFormat, Reader, Writer, MAX_PACKET_SIZE, read_varint};
use std::boxed::Box;
use std::io;

pub struct CompressedPacketFormat {
    threshold: usize,
}

impl CompressedPacketFormat {
    pub fn new(threshold: usize) -> Self {
        Self {
            threshold: threshold,
        }
    }
}

// A compressed header is in this format:
//
//     packet_length: VarInt
//     uncompressed_length: VarInt
//     data: [u8]
//
// The packet length is the size of the entire packet in bytes,
// including the uncompressed length.
// The uncompressed length is the size of the uncompressed data in bytes,
// or if it is zero, indicates that the data is not compressed.
// This is followed by the data, either compressed or uncompressed.

#[async_trait]
impl PacketFormat for CompressedPacketFormat {
    async fn recieve(&self, src: &mut Reader) -> io::Result<Box<[u8]>> {
        use tokio::io::AsyncReadExt;

        // First we read in the packet and uncompressed data lengths.
        let (_, packet_length) = read_varint(src).await?;
        if packet_length < 0 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet length was negative."));
        }
        if packet_length > MAX_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Packet was too long."));
        }
        let packet_length = packet_length as usize;

        let (data_length_size, data_length) = read_varint(src).await?;
        if data_length < 0 {
            return Err(io::Error::new(io::ErrorKind::Other, "Data length was negative."));
        }
        if data_length > MAX_PACKET_SIZE as i32 {
            return Err(io::Error::new(io::ErrorKind::Other, "Data was too long."));
        }
        let data_length = data_length as usize;

        // Now we recieve the remainder of the packet's data.
        let mut data = Vec::with_capacity(packet_length - data_length_size);
        data.resize(packet_length, 0);
        src.read_exact(data.as_mut_slice()).await?;

        // If the data was not compressed, we simply return it.
        if data_length == 0 {
            return Ok(data.into_boxed_slice());
        }

        // Otherwise, we decompress it.
        let mut decompressed = Vec::new();
        decompressed.resize(data_length, 0);

        use flate2::{Decompress, FlushDecompress};
        Decompress::new(true)
            .decompress(&data, decompressed.as_mut_slice(), FlushDecompress::Finish)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        Ok(decompressed.into_boxed_slice())
    }

    async fn send(&self, dest: &mut Writer, data: &[u8]) -> io::Result<()> {
        use crate::net::serialize::{PacketSerializer, VarInt};

        // If the length of the uncompressed data exceeds the threshold,
        // then we will compress this packet.
        if data.len() >= self.threshold {
            // Now we compress the data.
            use flate2::{Compress, FlushCompress};

            // 1024 is just an arbitrary amount of extra space reserved
            // in case the output data ends up larger than the input data
            // (e.g. due to the zlib header).
            // FIXME: Further research to figure out the exact maximum capacity necessary.
            //   Perhaps you only need space for the header and the data itself can't get bigger?
            //   And what is the limit to how much bigger the data will get?
            //   Currently I don't actually know for a fact that this won't ever drop data.
            let mut compressed = Vec::with_capacity(1024 + data.len());
            Compress::new(flate2::Compression::best(), true)
                .compress_vec(data, &mut compressed, FlushCompress::Finish)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

            // Since the packet is compressed,
            // data_length will be the length of the uncompressed data.
            let mut data_length_buf = Vec::with_capacity(5);
            data_length_buf.write(VarInt(data.len() as i32));

            let mut packet_length_buf = Vec::with_capacity(5);
            packet_length_buf.write(VarInt((data_length_buf.len() + compressed.len()) as i32));

            {
                // I have to keep this import in a block so that
                // it won't conflict with PacketSerialize::write.
                use tokio::io::AsyncWriteExt;
                dest.write(packet_length_buf.as_slice()).await?;
                dest.write(data_length_buf.as_slice()).await?;
                dest.write(compressed.as_slice()).await?;
                dest.flush().await?;
                Ok(())
            }
        } else {
            // Since the packet is uncompressed,
            // the packet length is just the length of the data plus the data_length,
            // which will just be 0x00 (1 byte long) because the data isn't compressed.
            let mut packet_length_buf = Vec::with_capacity(5);
            packet_length_buf.write(VarInt(data.len() as i32 + 1));

            {
                use tokio::io::AsyncWriteExt;

                dest.write(packet_length_buf.as_slice()).await?;
                dest.write_u8(0x00).await?;
                dest.write(data).await?;
                dest.flush().await?;
                Ok(())
            }
        }
    }
}
