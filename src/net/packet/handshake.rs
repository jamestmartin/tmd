use crate::net::serialize::PacketDeserializer;

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

#[derive(Debug)]
pub enum PacketHandshakeServerbound {
    Handshake(PacketHandshake),
}

pub fn read_packet_handshake(id: i32, deser: &mut impl PacketDeserializer)
        -> Result<PacketHandshakeServerbound, String> {
    use PacketHandshakeServerbound::*;

    match id {
        0x00 => {
            let protocol_version = deser.read_varint()?;
            let server_address = deser.read_string()?;
            let server_port = deser.read_u16()?;
            let next_state = match deser.read_varint()? {
                1 => HandshakeNextState::Status,
                2 => HandshakeNextState::Login,
                n => return Err(format!("Invalid next protocol state in handshake: {}", n))
            };
            deser.read_eof()?;
            Ok(Handshake(PacketHandshake {
                protocol_version: protocol_version,
                server_address: server_address,
                server_port: server_port,
                next_state: next_state,
            }))
        },
        id => Err(format!("Invalid handshake packet id: {}", id))
    }
}
