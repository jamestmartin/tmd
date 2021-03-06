use serde::de::DeserializeOwned;
use serde::Serialize;
use std::borrow::Borrow;
use std::convert::{From, Into};
use uuid::Uuid;

pub trait PacketSerializer: Sized {
    /// Write a slice of bytes directly, without a length prefix.
    fn write_exact(&mut self, value: &[u8]);
    fn write_eof(&mut self);

    fn write<D: PacketWritable>(&mut self, value: D) {
        value.write(self)
    }
}

impl PacketSerializer for Vec<u8> {
    fn write_exact(&mut self, value: &[u8]) {
        self.extend_from_slice(value);
    }

    fn write_eof(&mut self) {}
}

pub trait PacketDeserializer: Sized {
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), String>;
    fn read_rest(&mut self) -> Result<Box<[u8]>, String>;
    fn read_eof(&mut self) -> Result<(), String>;

    fn read<D: PacketReadable>(&mut self) -> Result<D, String> {
        D::read(self)
    }
}

pub struct VecPacketDeserializer<'a> {
    data: &'a [u8],
    index: usize,
}

impl VecPacketDeserializer<'_> {
    pub fn new(data: &[u8]) -> VecPacketDeserializer<'_> {
        VecPacketDeserializer { data, index: 0 }
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

    fn read_rest(&mut self) -> Result<Box<[u8]>, String> {
        let mut it = Vec::new();
        it.copy_from_slice(&self.data[self.index..]);
        self.index = self.data.len();
        Ok(it.into_boxed_slice())
    }

    fn read_eof(&mut self) -> Result<(), String> {
        if self.index != self.data.len() {
            return Err("Packet contained more data than necessary.".to_string());
        }

        Ok(())
    }
}

pub trait PacketReadable: Sized {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String>;
}

pub trait PacketWritable {
    fn write(self, ser: &mut impl PacketSerializer);
}

pub trait PacketData: PacketReadable + PacketWritable {}
impl<T: PacketReadable + PacketWritable> PacketData for T {}

impl PacketReadable for bool {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let value = deser.read::<u8>()?;
        match value {
            0x00 => Ok(false),
            0x01 => Ok(true),
            n => Err(format!("{:0X} is not a valid boolean.", n)),
        }
    }
}

impl PacketWritable for bool {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(self as u8);
    }
}

macro_rules! impl_packet_data_for_num {
    ( $( $num:ty, $len:expr );+ ) => {
        $(
            impl PacketReadable for $num {
                fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
                    deser.read::<[u8; $len]>().map(Self::from_be_bytes)
                }
            }

            impl PacketWritable for $num {
                fn write(self, ser: &mut impl PacketSerializer) {
                    ser.write(self.to_be_bytes())
                }
            }
        )*
    }
}

impl_packet_data_for_num!(u8, 1; i8, 1; u16, 2; i16, 2; u32, 4; i32, 4; u64, 8; i64, 8; u128, 16;
                          f32, 4; f64, 8);

// HACK: There is probably a better solution to this than a macro.
//   Same goes for the above, but to a lesser degree.
macro_rules! impl_varnum {
    ( $( $name:ident, $wraps:ty, $length:expr);+ ) => {
        $(
            #[derive(Copy, Clone, Debug)]
            pub struct $name(pub $wraps);

            impl From<$wraps> for $name {
                fn from(x: $wraps) -> Self { $name(x) }
            }

            impl From<$name> for $wraps {
                fn from(x: $name) -> Self { x.0 }
            }

            impl PacketReadable for $name {
                fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
                    let mut num_read: usize = 0;
                    let mut acc = 0;
                    while num_read < $length {
                        // If the highest bit is set, there are further bytes to be read;
                        // the rest of the bits are the actual bits of the number.
                        let read = deser.read::<u8>()?;
                        acc |= ((read & 0b01111111) as $wraps) << num_read * 7;

                        num_read += 1;

                        if (read & 0b10000000) == 0 {
                            // There are no more bytes.
                            return Ok($name(acc));
                        }

                        // Make space for the rest of the bits.
                        acc <<= 7;
                    }

                    Err(format!("VarNum was more than {} bytes.", $length))
                }
            }

            impl PacketWritable for $name {
                fn write(self, ser: &mut impl PacketSerializer) {
                    let mut value = self.0;
                    loop {
                        let mut temp = (value & 0b01111111) as u8;
                        value >>= 7;
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
        )*
    }
}

impl_varnum!(VarInt, i32, 5; VarLong, i64, 10);

impl PacketWritable for &[u8] {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(VarInt(self.len() as i32));
        ser.write_exact(self);
    }
}

impl PacketReadable for Vec<u8> {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let length: i32 = deser.read::<VarInt>()?.into();
        if length < 0 {
            return Err("Array or string length cannot be negative.".to_string());
        }

        let mut it = vec![0; length as usize];
        deser.read_exact(it.as_mut_slice())?;

        Ok(it)
    }
}

impl PacketWritable for &Vec<u8> {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(self.as_slice());
    }
}

impl PacketReadable for Box<[u8]> {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<Vec<u8>>().map(|x| x.into_boxed_slice())
    }
}

impl PacketWritable for &Box<[u8]> {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write::<&[u8]>(self.borrow());
    }
}

pub struct Rest(pub Box<[u8]>);

impl PacketReadable for Rest {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read_rest().map(Rest)
    }
}

impl PacketWritable for &Rest {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(&self.0);
    }
}

impl PacketWritable for &str {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(self.as_bytes());
    }
}

impl PacketReadable for String {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let bytes = deser.read()?;
        String::from_utf8(bytes).map_err(|_| "String contained invalid UTF-8.".to_string())
    }
}

impl PacketWritable for &String {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write::<&str>(self.borrow());
    }
}

impl PacketReadable for Box<str> {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<String>().map(|x| x.into_boxed_str())
    }
}

impl PacketWritable for &Box<str> {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write::<&str>(self.borrow());
    }
}

impl PacketReadable for Uuid {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        deser.read::<u128>().map(Uuid::from_u128)
    }
}

impl PacketWritable for &Uuid {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(self.as_u128());
    }
}

/// A marker trait indicating that a JSON-serialiable type should be serialized
/// as JSON in packets. Most primitive types are already serializable as JSON,
/// but we explicitly *don't* want to serialize them as JSON in packets.
pub trait PacketJson {}

impl PacketJson for crate::net::chat::Chat {}

impl<S: DeserializeOwned + PacketJson + Sized> PacketReadable for S {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let bytes = deser.read::<Vec<u8>>()?;
        serde_json::from_slice(&bytes).map_err(|_| "Bad JSON syntax".to_string())
    }
}

impl<S: PacketJson + Serialize> PacketWritable for &S {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write(&serde_json::to_vec(self).unwrap())
    }
}

// Although according to my organizational scheme, this should go first,
// it goes last anyway because constant generics break Atom's syntax
// highlighting for all code below it.

impl<const N: usize> PacketReadable for [u8; N] {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let mut buf = [0; N];
        deser.read_exact(&mut buf)?;
        Ok(buf)
    }
}

impl<const N: usize> PacketWritable for [u8; N] {
    fn write(self, ser: &mut impl PacketSerializer) {
        ser.write_exact(&self);
    }
}
