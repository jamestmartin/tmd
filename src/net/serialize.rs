use crate::net::format::MAX_CLIENT_PACKET_SIZE;
use serde::Serialize;

pub trait PacketSerializer {
    /// Write a slice of bytes directly, without a length prefix.
    fn write(&mut self, value: &[u8]);

    fn write_u8(&mut self, value: u8) {
        self.write(&value.to_le_bytes());
    }

    /// https://wiki.vg/Protocol#VarInt_and_VarLong
    fn write_varint(&mut self, mut value: i32) {
        loop {
            let mut temp = (value & 0b01111111) as u8;
            value = value >> 7;
            if value != 0 {
                temp |= 0b10000000;
            }
            self.write_u8(temp);

            if value == 0 {
                break;
            }
        }
    }

    /// Write a varint-length-prefixed byte slice.
    fn write_slice(&mut self, value: &[u8]) {
        self.write_varint(value.len() as i32);
        self.write(value);
    }

    /// Write a varint-length-prefixed string.
    fn write_str(&mut self, value: &str) {
        self.write_slice(value.as_bytes());
    }

    fn write_json(&mut self, value: &impl Serialize) {
        self.write_slice(serde_json::to_vec(value).unwrap().as_slice())
    }
}

impl PacketSerializer for Vec<u8> {
    fn write(&mut self, value: &[u8]) {
        self.extend_from_slice(value);
    }

    fn write_u8(&mut self, value: u8) {
        self.push(value);
    }
}

pub trait PacketDeserializer {
    fn read(&mut self, buf: &mut [u8]) -> Result<(), String>;
    fn read_eof(&mut self) -> Result<(), String>;

    fn read_u8(&mut self) -> Result<u8, String> {
        let mut buf = [0; 1];
        self.read(&mut buf)?;
        Ok(u8::from_le_bytes(buf))
    }

    fn read_u16(&mut self) -> Result<u16, String> {
        let mut buf = [0; 2];
        self.read(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    fn read_varint(&mut self) -> Result<i32, String> {
        let mut length = 1;
        let mut acc = 0;
        // VarInts must not be longer than 5 bytes.
        while length <= 5 {
            // If the highest bit is set, there are further bytes to be read from this VarInt;
            // the rest of the bits are the actual data in the VarInt.
            let read = self.read_u8()?;
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
        Err("VarInt was more than 5 bytes.".to_string())
    }

    fn read_string(&mut self) -> Result<String, String> {
        let length = self.read_varint()?;
        if length < 0 {
            return Err("String length cannot be negative.".to_string());
        }

        let length = length as usize;
        if length > MAX_CLIENT_PACKET_SIZE {
            return Err("String was too long.".to_string());
        }

        let mut buf = Vec::with_capacity(length);
        buf.resize(length, 0);
        self.read(buf.as_mut_slice())?;
        String::from_utf8(buf).map_err(|_| "String was invalid UTF-8.".to_string())
    }
}

pub struct VecPacketDeserializer<'a> {
    data: &'a [u8],
    index: usize,
}

impl VecPacketDeserializer<'_> {
    pub fn new<'a>(data: &'a [u8]) -> VecPacketDeserializer<'a> {
        VecPacketDeserializer {
            data: data,
            index: 0,
        }
    }
}

impl PacketDeserializer for VecPacketDeserializer<'_> {
    fn read(&mut self, buf: &mut [u8]) -> Result<(), String> {
        if self.index + buf.len() > self.data.len() {
            return Err("Tried to read past length of packet.".to_string());
        }

        let len = buf.len();
        buf[..].copy_from_slice(&self.data[self.index..self.index + len]);
        self.index += buf.len();

        Ok(())
    }

    fn read_eof(&mut self) -> Result<(), String> {
        if self.index != self.data.len() {
            return Err("Packet contained more data than necessary.".to_string());
        }

        Ok(())
    }
}
