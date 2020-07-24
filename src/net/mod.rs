pub mod chat;
pub mod format;
pub mod packet;
pub mod serialize;

use tokio::io::{BufReader, BufWriter};
use tokio::net::tcp::{ReadHalf, WriteHalf};

pub type Reader<'a> = BufReader<ReadHalf<'a>>;
pub type Writer<'a> = BufWriter<WriteHalf<'a>>;
