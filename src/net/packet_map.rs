use crate::net::serialize::{PacketDeserializer, PacketSerializer};

pub trait PacketMap: Sized + Sync {
    /// Get a packet's id.
    fn id(&self) -> i32;
    /// Read a packet from the deserializer.
    fn read(id: i32, deser: &mut impl PacketDeserializer) -> Result<Self, String>;
    /// Write this packet's data to the serializer.
    fn write(&self, ser: &mut impl PacketSerializer);
}

impl PacketMap for ! {
    fn id(&self) -> i32 {
        match *self { }
    }

    fn read(_id: i32, _deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        Err("Cannot read packets; the connection state is disconnected.".to_string())
    }

    fn write(&self, _ser: &mut impl PacketSerializer) {
        match *self { }
    }
}

#[macro_export]
macro_rules! define_packet_maps {
    { $( packet_map $name:ident { $( $id:expr => $packet:ident ),* } )+ } => {
        $(
            pub enum $name {
                $( $packet($packet) ),*
            }

            impl crate::net::packet_map::PacketMap for $name {
                fn id(&self) -> i32 {
                    match *self {
                        $( $name::$packet(_) => $id ),*
                    }
                }

                #[allow(unused_variables)]
                fn read(id: i32, deser: &mut impl crate::net::serialize::PacketDeserializer)
                        -> Result<Self, String> {
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
