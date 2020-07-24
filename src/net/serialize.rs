use crate::net::format::MAX_CLIENT_PACKET_SIZE;
use serde::Serialize;
use serde::de::DeserializeOwned;
use std::convert::{From, Into};

pub trait PacketData: Sized {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String>
        where Self: std::marker::Sized;
    fn write(&self, ser: &mut impl PacketSerializer);
}

impl<const N: usize> PacketData for [u8; N] {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        use std::convert::TryInto;

        let mut buf = [0; N];
        deser.read_exact(&mut buf)?;
        Ok(buf.try_into().unwrap())
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write_exact(self);
    }
}

impl PacketData for bool {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let value = deser.read::<u8>()?;
        match value {
            0x00 => Ok(false),
            0x01 => Ok(true),
            n => Err(format!("{:0X} is not a valid boolean.", n))
        }
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(*self as u8);
    }
}

impl PacketData for u8 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 1]>().map(u8::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes())
    }
}

impl PacketData for i8 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 1]>().map(i8::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes())
    }
}

impl PacketData for u16 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 2]>().map(u16::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

impl PacketData for i16 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 2]>().map(i16::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

impl PacketData for i32 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 4]>().map(i32::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

impl PacketData for i64 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 8]>().map(i64::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

impl PacketData for f32 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 4]>().map(f32::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

impl PacketData for f64 {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<[u8; 8]>().map(f64::from_be_bytes)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.to_be_bytes());
    }
}

pub struct VarInt(pub i32);

impl From<i32> for VarInt {
    fn from(x: i32) -> Self { VarInt(x) }
}

impl Into<i32> for VarInt {
    fn into(self) -> i32 { self.0 }
}

impl PacketData for VarInt {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let mut length = 1;
        let mut acc = 0;
        // VarInts must not be longer than 5 bytes.
        while length <= 5 {
            // If the highest bit is set, there are further bytes to be read from this VarInt;
            // the rest of the bits are the actual data in the VarInt.
            let read = deser.read::<u8>()?;
            acc |= (read & 0b01111111) as i32;

            // There are no more bytes.
            if (read & 0b10000000) == 0 {
                return Ok(VarInt(acc));
            }

            // Make space for the rest of the bits.
            acc = acc << 7;
            length += 1;
        }

        // The VarInt was too long!
        Err("VarInt was more than 5 bytes.".to_string())
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        let mut value = self.0;
        loop {
            let mut temp = (value & 0b01111111) as u8;
            value = value >> 7;
            if value != 0 {
                temp |= 0b10000000;
            }
            ser.write(temp);

            if value == 0 {
                break;
            }
        }
    }
}

pub struct VarLong(pub i64);

impl From<i64> for VarLong {
    fn from(x: i64) -> Self { VarLong(x) }
}

impl Into<i64> for VarLong {
    fn into(self) -> i64 { self.0 }
}

impl PacketData for VarLong {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let mut length = 1;
        let mut acc = 0;
        // VarLongs must not be longer than 5 bytes.
        while length <= 10 {
            // If the highest bit is set, there are further bytes to be read from this VarLong;
            // the rest of the bits are the actual data in the VarLong.
            let read = deser.read::<u8>()?;
            acc |= (read & 0b01111111) as i64;

            // There are no more bytes.
            if (read & 0b10000000) == 0 {
                return Ok(VarLong(acc));
            }

            // Make space for the rest of the bits.
            acc = acc << 7;
            length += 1;
        }

        // The VarLong was too long!
        Err("VarLong was more than 10 bytes.".to_string())
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        let mut value = self.0;
        loop {
            let mut temp = (value & 0b01111111) as u8;
            value = value >> 7;
            if value != 0 {
                temp |= 0b10000000;
            }
            ser.write(temp);

            if value == 0 {
                break;
            }
        }
    }
}

impl PacketData for Vec<u8> {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let length: i32 = deser.read::<VarInt>()?.into();
        if length < 0 {
            return Err("String length cannot be negative.".to_string());
        }

        let length = length as usize;
        if length > MAX_CLIENT_PACKET_SIZE {
            return Err("Byte array was too long.".to_string());
        }

        let mut it = Vec::with_capacity(length);
        it.resize(length, 0);
        deser.read_exact(it.as_mut_slice())?;

        Ok(it)
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(VarInt(self.len() as i32));
        ser.write_exact(self.as_slice());
    }
}

impl PacketData for String {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let bytes = deser.read()?;
        String::from_utf8(bytes).map_err(|_| "String contained invalid UTF-8.".to_string())
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.clone().into_bytes());
    }
}

pub trait PacketJson: DeserializeOwned + Serialize + Sized { }

impl PacketJson for crate::net::chat::Chat { }

impl<S: PacketJson> PacketData for S {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let bytes = deser.read::<Vec<u8>>()?;
        serde_json::from_slice(&bytes).map_err(|_| "Bad JSON syntax".to_string())
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(serde_json::to_vec(self).unwrap());
    }
}

pub trait PacketSerializer: Sized {
    /// Write a slice of bytes directly, without a length prefix.
    fn write_exact(&mut self, value: &[u8]);

    fn write<D: PacketData>(&mut self, value: D) {
        value.write(self)
    }
}

impl PacketSerializer for Vec<u8> {
    fn write_exact(&mut self, value: &[u8]) {
        self.extend_from_slice(value);
    }
}

pub trait PacketDeserializer: Sized {
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), String>;
    fn read_eof(&mut self) -> Result<(), String>;

    fn read<D: PacketData>(&mut self) -> Result<D, String> {
        D::read(self)
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
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), String> {
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
