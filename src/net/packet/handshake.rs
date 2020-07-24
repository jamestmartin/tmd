use crate::net::serialize::{PacketData, PacketDeserializer, PacketSerializer, VarInt};

#[derive(Debug, PartialEq, Eq)]
pub enum HandshakeNextState {
    Status,
    Login,
}

#[derive(Debug)]
pub struct PacketHandshake {
    pub protocol_version: i32,
    pub server_address: String,
    pub server_port: u16,
    pub next_state: HandshakeNextState,
}

impl PacketData for PacketHandshake {
    fn read(deser: &mut impl PacketDeserializer) -> Result<Self, String> {
        let protocol_version = deser.read::<VarInt>()?.into();
        let server_address = deser.read::<String>()?;
        let server_port = deser.read::<u16>()?;
        let next_state = match deser.read::<VarInt>()?.into() {
            1 => HandshakeNextState::Status,
            2 => HandshakeNextState::Login,
            n => return Err(format!("Invalid next protocol state in handshake: {}", n))
        };
        deser.read_eof()?;
        Ok(PacketHandshake {
            protocol_version: protocol_version,
            server_address: server_address,
            server_port: server_port,
            next_state: next_state,
        })
    }

    fn write(&self, ser: &mut impl PacketSerializer) {
        ser.write(self.protocol_version);
        ser.write(self.server_address.clone());
        ser.write(self.server_port);
    }
}

#[derive(Debug)]
pub enum PacketHandshakeServerbound {
    Handshake(PacketHandshake),
}

pub fn read_packet_handshake(id: i32, deser: &mut impl PacketDeserializer)
        -> Result<PacketHandshakeServerbound, String> {
    use PacketHandshakeServerbound::*;

    match id {
        0x00 => deser.read().map(Handshake),
        id => Err(format!("Invalid handshake packet id: {}", id))
    }
}
