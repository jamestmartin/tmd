use crate::net::chat::Chat;
use crate::net::serialize::PacketJson;
use crate::{define_packet_maps, define_packets, define_state};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Clone, Deserialize, Serialize)]
pub struct ResponseVersion {
    pub name: String,
    pub protocol: u32,
}
impl PacketJson for ResponseVersion {}

#[derive(Clone, Deserialize, Serialize)]
pub struct ResponsePlayersSample {
    pub name: String,
    pub id: Uuid,
}
impl PacketJson for ResponsePlayersSample {}

#[derive(Clone, Deserialize, Serialize)]
pub struct ResponsePlayers {
    pub max: u32,
    pub online: u32,
    pub sample: Vec<ResponsePlayersSample>,
}
impl PacketJson for ResponsePlayers {}

#[derive(Clone, Deserialize, Serialize)]
pub struct ResponseData {
    pub version: ResponseVersion,
    pub players: ResponsePlayers,
    pub description: Chat,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub favicon: Option<String>,
}
impl PacketJson for ResponseData {}

define_packets! {
    // Clientbound

    packet Response {
        data: ResponseData
    }

    packet Pong {
        payload: [u8; 8]
    }

    // Serverbound

    packet Request { }

    packet Ping {
        payload: [u8; 8]
    }
}

define_packet_maps! {
    packet_map Clientbound {
        0x00 => Response,
        0x01 => Pong
    }

    packet_map Serverbound {
        0x00 => Request,
        0x01 => Ping
    }
}

define_state!(Status, Clientbound, Serverbound);
