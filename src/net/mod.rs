pub mod chat;
pub mod connection;
pub mod format;
pub mod packet;
pub mod packet_map;
pub mod serialize;
pub mod state;

use tokio::io::{BufReader, BufWriter};
use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};

pub type Reader = BufReader<OwnedReadHalf>;
pub type Writer = BufWriter<OwnedWriteHalf>;
