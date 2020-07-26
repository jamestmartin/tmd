use crate::net::chat::Chat;
use crate::{define_packet_maps, define_packets, define_state};

// TODO: This protocol state isn't even close to entirely mapped.

define_packets! {
    packet Disconnect {
        reason: Chat
    }
}

define_packet_maps! {
    packet_map Clientbound {
        0x1a => Disconnect
    }

    packet_map Serverbound {

    }
}

define_state!(Play, Clientbound, Serverbound);
