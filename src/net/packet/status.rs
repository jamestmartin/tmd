use crate::net::chat::Chat;
use crate::net::source::{PacketError, PacketSource, Result};
use serde::Serialize;
use std::convert::TryInto;
use tokio::io::AsyncWriteExt;
use tokio::io::BufWriter;
use tokio::net::tcp::WriteHalf;
use uuid::Uuid;

use PacketError::PktError;

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

pub async fn read_packet_status(source: &mut PacketSource<'_>) -> Result<PacketStatusServerbound> {
    use PacketStatusServerbound::*;

    let _length = source.read_varint().await?;
    let id = source.read_varint().await?;

    match id {
        0 => Ok(Request),
        1 => {
            let mut buf = [0; 8];
            source.read_exact(&mut buf).await?;
            Ok(Ping(buf.try_into().unwrap()))
        }
        id => Err(PktError(format!("Invalid status packet id: {}", id)))
    }
}

fn write_varint(dest: &mut Vec<u8>, mut value: i32) {
    loop {
        let mut temp = (value & 0b01111111) as u8;
        value = value >> 7;
        if value != 0 {
            temp |= 0b10000000;
        }
        dest.push(temp);

        if value == 0 {
            break;
        }
    }
}

fn write_slice(dest: &mut Vec<u8>, bytes: &[u8]) {
    write_varint(dest, bytes.len() as i32);
    dest.extend_from_slice(bytes);
}

fn write_string(dest: &mut Vec<u8>, value: &str) {
    write_slice(dest, value.as_bytes());
}

pub async fn write_packet_status(dest: &mut BufWriter<WriteHalf<'_>>,
                                 packet: PacketStatusClientbound) -> std::io::Result<()> {
    use PacketStatusClientbound::*;

    let mut data = Vec::new();

    match packet {
        Response(response) => {
            write_varint(&mut data, 0x00);
            write_slice(&mut data, serde_json::to_vec(&response).unwrap().as_slice());
        },
        Pong(payload) => {
            write_varint(&mut data, 0x01);
            data.extend_from_slice(&payload);
        }
    }

    let mut packet_length_buf = Vec::new();
    write_varint(&mut packet_length_buf, data.len() as i32);

    dest.write_all(packet_length_buf.as_slice()).await?;
    dest.write_all(data.as_slice()).await?;
    dest.flush().await?;

    Ok(())
}
