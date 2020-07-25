pub mod chat;
pub mod connection;
pub mod protocol;
pub mod serialize;

use tokio::io::{BufReader, BufWriter};
use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};

pub type Reader = BufReader<OwnedReadHalf>;
pub type Writer = BufWriter<OwnedWriteHalf>;
