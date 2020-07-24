use crate::net::chat::Chat;
use crate::net::serialize::{PacketDeserializer, PacketSerializer, PacketJson};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Clone, Deserialize, Serialize)]
pub struct PacketResponseVersion {
    pub name: String,
    pub protocol: u32,
}
impl PacketJson for PacketResponseVersion {}

#[derive(Clone, Deserialize, Serialize)]
pub struct PacketResponsePlayersSample {
    pub name: String,
    pub id: Uuid,
}
impl PacketJson for PacketResponsePlayersSample {}

#[derive(Clone, Deserialize, Serialize)]
pub struct PacketResponsePlayers {
    pub max: u32,
    pub online: u32,
    pub sample: Vec<PacketResponsePlayersSample>
}
impl PacketJson for PacketResponsePlayers {}

#[derive(Clone, Deserialize, Serialize)]
pub struct PacketResponse {
    pub version: PacketResponseVersion,
    pub players: PacketResponsePlayers,
    pub description: Chat,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub favicon: Option<String>,
}
impl PacketJson for PacketResponse {}

pub enum PacketStatusClientbound {
    Response(PacketResponse),
    Pong([u8; 8]),
}

#[derive(Debug)]
pub enum PacketStatusServerbound {
    Request,
    Ping([u8; 8]),
}

pub fn read_packet_status(id: i32, deser: &mut impl PacketDeserializer)
        -> Result<PacketStatusServerbound, String> {
    use PacketStatusServerbound::*;

    match id {
        0 => {
            deser.read_eof()?;
            Ok(Request)
        },
        1 => {
            let payload = deser.read::<[u8; 8]>()?;
            deser.read_eof()?;
            Ok(Ping(payload))
        }
        id => Err(format!("Invalid status packet id: {}", id))
    }
}

pub fn write_packet_status(ser: &mut impl PacketSerializer, packet: PacketStatusClientbound)
        -> i32 {
    use PacketStatusClientbound::*;

    match packet {
        Response(response) => {
            ser.write(response);
            0x00
        },
        Pong(payload) => {
            ser.write(payload);
            0x01
        }
    }
}
