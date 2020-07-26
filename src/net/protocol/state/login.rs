use crate::net::chat::Chat;
use crate::net::serialize::{Rest, VarInt};
use crate::{define_packet_maps, define_packets, define_state};
use uuid::Uuid;

define_packets! {
    // Clientbound

    packet Disconnect {
        reason: Chat
    }

    packet EncryptionRequest {
        server_id: Box<str>,
        public_key: Box<[u8]>,
        verify_token: Box<[u8]>
    }

    packet LoginSuccess {
        uuid: Uuid,
        username: Box<str>
    }

    packet SetCompression {
        threshold: VarInt
    }

    packet LoginPluginRequest {
        message_id: VarInt,
        // FIXME: Actually an Identifier.
        channel: String,
        data: Rest
    }

    // Serverbound

    packet LoginStart {
        name: Box<str>
    }

    packet EncryptionResponse {
        shared_secret: Box<[u8]>,
        verify_token: Box<[u8]>
    }

    packet LoginPluginResponse {
        message_id: VarInt,
        successful: bool,
        data: Rest
    }
}

define_packet_maps! {
    packet_map Clientbound {
        0x00 => Disconnect,
        0x01 => EncryptionRequest,
        0x02 => LoginSuccess,
        0x03 => SetCompression,
        0x04 => LoginPluginRequest
    }

    packet_map Serverbound {
        0x00 => LoginStart,
        0x01 => EncryptionResponse,
        0x02 => LoginPluginResponse
    }
}

define_state!(Login, Clientbound, Serverbound);
