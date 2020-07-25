use crate::net::serialize::{PacketDeserializer, PacketSerializer};

pub trait ProtocolState: Sized {
    /// Get a packet's id.
    fn id(&self) -> i32;
    /// Read a packet from the deserializer.
    fn read(id: i32, deser: &mut impl PacketDeserializer) -> Result<Self, String>;
    /// Write this packet's data to the serializer.
    fn write(&self, ser: &mut impl PacketSerializer);
}

#[macro_export]
macro_rules! define_states {
    { $( state $name:ident { $( $id:expr => $packet:ident ),* } )+ } => {
        $(
            pub enum $name {
                $( $packet($packet) ),*
            }

            impl crate::net::state::ProtocolState for $name {
                fn id(&self) -> i32 {
                    match *self {
                        $( $name::$packet(_) => $id ),*
                    }
                }

                #[allow(unused_variables)]
                fn read(id: i32, deser: &mut impl crate::net::serialize::PacketDeserializer) -> Result<Self, String> {
                    match id {
                        $( $id => deser.read::<$packet>().map($name::$packet), )*
                        id => Err(format!("Invalid packet id: {}", id))
                    }
                }

                #[allow(unused_variables)]
                fn write(&self, ser: &mut impl crate::net::serialize::PacketSerializer) {
                    match *self {
                        $( $name::$packet(ref pkt) => ser.write(pkt) ),*
                    }
                }
            }
        )*
    }
}
