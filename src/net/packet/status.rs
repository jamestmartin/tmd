use crate::{define_packets, define_states};
use crate::net::chat::Chat;
use crate::net::serialize::{PacketJson};
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
    pub sample: Vec<ResponsePlayersSample>
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

define_states! {
    state StatusClientbound {
        0x00 => Response,
        0x01 => Pong
    }

    state StatusServerbound {
        0x00 => Request,
        0x01 => Ping
    }
}
