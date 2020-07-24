use crate::net::chat::Chat;
use crate::net::serialize::{PacketSerializer, PacketDeserializer};
use serde::Serialize;
use std::convert::TryInto;
use uuid::Uuid;

#[derive(Serialize)]
pub struct PacketResponseVersion {
    pub name: String,
    pub protocol: u32,
}

#[derive(Serialize)]
pub struct PacketResponsePlayersSample {
    pub name: String,
    pub id: Uuid,
}

#[derive(Serialize)]
pub struct PacketResponsePlayers {
    pub max: u32,
    pub online: u32,
    pub sample: Vec<PacketResponsePlayersSample>
}

#[derive(Serialize)]
pub struct PacketResponse {
    pub version: PacketResponseVersion,
    pub players: PacketResponsePlayers,
    pub description: Chat,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub favicon: Option<String>,
}

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
            let mut buf = [0; 8];
            deser.read(&mut buf)?;
            deser.read_eof()?;
            Ok(Ping(buf.try_into().unwrap()))
        }
        id => Err(format!("Invalid status packet id: {}", id))
    }
}

pub fn write_packet_status(ser: &mut impl PacketSerializer, packet: PacketStatusClientbound)
        -> i32 {
    use PacketStatusClientbound::*;

    match packet {
        Response(response) => {
            ser.write_json(&response);
            0x00
        },
        Pong(payload) => {
            ser.write(&payload);
            0x01
        }
    }
}
