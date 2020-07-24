use crate::net::source::{PacketError, PacketSource, Result};
use PacketError::PktError;

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

pub async fn read_packet_handshake(source: &mut PacketSource<'_>) -> Result<PacketHandshakeServerbound> {
    use PacketHandshakeServerbound::*;

    let _length = source.read_varint().await?;
    let id = source.read_varint().await?;
    match id {
        0x00 => {
            let protocol_version = source.read_varint().await?;
            let server_address = source.read_string().await?;
            let server_port = source.read_u16().await?;
            let next_state = match source.read_varint().await? {
                1 => HandshakeNextState::Status,
                2 => HandshakeNextState::Login,
                n => return Err(PktError(format!("Invalid next protocol state in handshake: {}", n)))
            };
            Ok(Handshake(PacketHandshake {
                protocol_version: protocol_version,
                server_address: server_address,
                server_port: server_port,
                next_state: next_state,
            }))
        },
        id => Err(PktError(format!("Invalid handshake packet id: {}", id)))
    }
}
