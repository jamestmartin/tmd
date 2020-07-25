use crate::{define_packets, define_states};
use crate::net::serialize::{PacketReadable, PacketWritable, PacketDeserializer, PacketSerializer, VarInt};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HandshakeNextState {
    Status,
    Login,
}

impl PacketReadable for HandshakeNextState {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        use HandshakeNextState::*;

        Ok(match deser.read::<VarInt>()?.into() {
            1 => Status,
            2 => Login,
            n => return Err(format!("Invalid next protocol state in handshake: {}", n))
        })
    }
}

impl PacketWritable for &HandshakeNextState {
    fn write(self, ser: &mut impl PacketSerializer) {
        use HandshakeNextState::*;

        ser.write(VarInt(match self {
            Status => 1,
            Login => 2,
        }));
    }
}

define_packets! {
    packet Handshake {
        protocol_version: VarInt,
        server_address: String,
        server_port: u16,
        next_state: HandshakeNextState
    }
}

define_states! {
    state HandshakeClientbound { }

    state HandshakeServerbound {
        0x00 => Handshake
    }
}
