use tokio::io::AsyncReadExt;
use tokio::net::tcp::ReadHalf;

const MAX_CLIENT_PACKET_SIZE: usize = 32767;

pub struct PacketSource<'a> {
    source: &'a mut ReadHalf<'a>,
    buf: [u8; MAX_CLIENT_PACKET_SIZE],
    index: usize,
    used: usize,
}

#[derive(Debug)]
pub enum PacketError {
    IoError(std::io::Error),
    PktError(String),
}

use PacketError::*;

pub type Result<A> = std::result::Result<A, PacketError>;

impl PacketSource<'_> {
    pub fn new<'a>(source: &'a mut ReadHalf<'a>) -> PacketSource<'a> {
        PacketSource {
            source: source,
            buf: [0; MAX_CLIENT_PACKET_SIZE],
            index: 0,
            used: 0,
        }
    }
    
    // TODO: Come up with a more efficient way of buffering.
    pub async fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
        let mut index = 0;
        while buf.len() > index {
            let bytes_needed = buf.len() - index;
            // If we've already read as many bytes as we need, return.
            if bytes_needed == 0 {
                break;
            }
            
            let bytes_remaining = self.used - self.index;
            // If we're out of bytes to read, read some more.
            if bytes_remaining == 0 {
                // If we've already used up the entire buffer, restart from the beginning.
                let space_remaining = self.buf.len() - self.used;
                if space_remaining < 1 {
                    self.used = 0;
                    self.index = 0;
                }
                
                let len = self.buf.len();
                let read = match self.source.read(&mut self.buf[self.index..len]).await {
                    Ok(read) => read,
                    Err(err) => return Err(IoError(err))
                };
                
                self.used += read;
                continue;
            }
            
            let bytes_to_copy = bytes_remaining.min(bytes_needed);
            
            buf[index..index + bytes_to_copy]
                .copy_from_slice(&self.buf[self.index..self.index + bytes_to_copy]);
            index += bytes_to_copy;
            self.index += bytes_to_copy;
        }
        
        Ok(())
    }
    
    pub async fn read_u8(&mut self) -> Result<u8> {
        let mut buf = [0; 1];
        self.read_exact(&mut buf).await?;
        Ok(buf[0])
    }
    
    pub async fn read_u16(&mut self) -> Result<u16> {
        let mut buf = [0; 2];
        self.read_exact(&mut buf).await?;
        Ok(u16::from_le_bytes(buf))
    }
    
    pub async fn read_i64(&mut self) -> Result<i64> {
        let mut buf = [0; 8];
        self.read_exact(&mut buf).await?;
        Ok(i64::from_le_bytes(buf))
    }
    
    pub async fn read_varint(&mut self) -> Result<i32> {
        let mut length = 1;
        let mut acc = 0;
        // VarInts must not be longer than 5 bytes.
        while length <= 5 {
            // If the highest bit is set, there are further bytes to be read from this VarInt;
            // the rest of the bits are the actual data in the VarInt.
            let read = self.read_u8().await?;
            acc |= (read & 0b01111111) as i32;
            
            // There are no mo
            if (read & 0b10000000) == 0 {
                return Ok(acc);
            }
            
            // Make space for the rest of the bits.
            acc = acc << 7;
            length += 1;
        }
        
        // The VarInt was too long!
        Err(PktError("VarInt was more than 5 bytes.".to_string()))
    }
    
    pub async fn read_string(&mut self) -> Result<String> {
        let length = self.read_varint().await?;
        if length < 0 {
            return Err(PktError("String length cannot be negative.".to_string()));
        }
        
        let length = length as usize;
        if length > MAX_CLIENT_PACKET_SIZE {
            return Err(PktError("String was too long.".to_string()));
        }
        
        let mut buf = Vec::new();
        buf.resize(length, 0);
        self.read_exact(buf.as_mut_slice()).await?;
        String::from_utf8(buf).map_err(|_| PktError("String was invalid UTF-8.".to_string()))
    }
}