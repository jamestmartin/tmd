use crate::net::{Reader, Writer};
use crate::net::format::{PacketFormat, DefaultPacketFormat};
use crate::net::state::ProtocolState;
use crate::net::state::handshake::Handshake;
use crate::net::state::login::Login;
use crate::net::state::play::Play;
use crate::net::state::status::Status;
use std::io;
use std::marker::PhantomData;
use tokio::net::TcpStream;

pub struct Connection<St: ProtocolState> {
    src: Reader,
    dest: Writer,
    st: PhantomData<St>,
}

impl<St: ProtocolState> Connection<St> {
    pub async fn write(&mut self, pkt: &St::Clientbound) -> io::Result<()> {
        DefaultPacketFormat.send::<St::Clientbound>(&mut self.dest, pkt).await
    }

    pub async fn read(&mut self) -> io::Result<St::Serverbound> {
        DefaultPacketFormat.recieve::<St::Serverbound>(&mut self.src).await
    }

    pub fn into_disconnected(self) -> Connection<!> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}

impl Connection<Handshake> {
    pub fn new(stream: TcpStream) -> Self {
        use tokio::io::{BufReader, BufWriter};

        let (src, dest) = stream.into_split();

        Connection {
            src: BufReader::new(src),
            dest: BufWriter::new(dest),
            st: PhantomData,
        }
    }

    pub fn into_status(self) -> Connection<Status> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }

    pub fn into_login(self) -> Connection<Login> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}

impl Connection<Login> {
    pub fn into_play(self) -> Connection<Play> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}
