pub mod handshake;
pub mod status;

use crate::net::packet_map::PacketMap;

pub trait ProtocolState {
    type Clientbound: PacketMap;
    type Serverbound: PacketMap;
}

impl ProtocolState for ! {
    type Clientbound = !;
    type Serverbound = !;
}

#[macro_export]
macro_rules! define_state {
    ( $name:ident , $cb:ty , $sb:ty ) => {
        #[allow(dead_code)]
        pub enum $name {}

        impl crate::net::state::ProtocolState for $name {
            type Clientbound = $cb;
            type Serverbound = $sb;
        }
    }
}
