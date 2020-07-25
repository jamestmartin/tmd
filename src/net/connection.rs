use crate::net::{Reader, Writer};
use crate::net::format::{PacketFormat, DefaultPacketFormat};
use crate::net::state::ProtocolState;
use crate::net::packet::handshake::{HandshakeClientbound, HandshakeServerbound};
use crate::net::packet::status::{StatusClientbound, StatusServerbound};
use std::io;
use std::marker::PhantomData;
use tokio::net::TcpStream;

pub trait State {
    type Clientbound: ProtocolState + Sync;
    type Serverbound: ProtocolState + Sync;
}

#[allow(dead_code)]
pub enum StateHandshake {}

impl State for StateHandshake {
    type Clientbound = HandshakeClientbound;
    type Serverbound = HandshakeServerbound;
}

#[allow(dead_code)]
pub enum StateStatus {}

impl State for StateStatus {
    type Clientbound = StatusClientbound;
    type Serverbound = StatusServerbound;
}

#[allow(dead_code)]
pub enum StateDisconnected {}

impl State for StateDisconnected {
    type Clientbound = !;
    type Serverbound = !;
}

#[allow(dead_code)]
pub enum StateLogin {}

impl State for StateLogin {
    type Clientbound = !;
    type Serverbound = !;
}

#[allow(dead_code)]
pub enum StatePlay {}

impl State for StatePlay {
    type Clientbound = !;
    type Serverbound = !;
}

pub struct Connection<St: State> {
    src: Reader,
    dest: Writer,
    st: PhantomData<St>,
}

impl<St: State> Connection<St> {
    pub async fn write(&mut self, pkt: &St::Clientbound) -> io::Result<()> {
        DefaultPacketFormat.send::<St::Clientbound>(&mut self.dest, pkt).await
    }

    pub async fn read(&mut self) -> io::Result<St::Serverbound> {
        DefaultPacketFormat.recieve::<St::Serverbound>(&mut self.src).await
    }

    pub fn into_disconnected(self) -> Connection<StateDisconnected> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}

impl Connection<StateHandshake> {
    pub fn new(stream: TcpStream) -> Self {
        use tokio::io::{BufReader, BufWriter};

        let (src, dest) = stream.into_split();

        Connection {
            src: BufReader::new(src),
            dest: BufWriter::new(dest),
            st: PhantomData,
        }
    }

    pub fn into_status(self) -> Connection<StateStatus> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }

    pub fn into_login(self) -> Connection<StateLogin> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}

impl Connection<StateLogin> {
    pub fn into_play(self) -> Connection<StatePlay> {
        Connection {
            src: self.src,
            dest: self.dest,
            st: PhantomData,
        }
    }
}
