mod packet_format;

use crate::net::{Reader, Writer};
use crate::net::connection::packet_format::PacketFormat;
use crate::net::connection::packet_format::default::DefaultPacketFormat;
use crate::net::protocol::packet_map::PacketMap;
use crate::net::protocol::state::ProtocolState;
use crate::net::protocol::state::handshake::Handshake;
use crate::net::protocol::state::login::Login;
use crate::net::protocol::state::play::Play;
use crate::net::protocol::state::status::Status;
use std::io;
use std::marker::PhantomData;
use tokio::net::TcpStream;

pub struct Connection<St: ProtocolState> {
    src: Reader,
    dest: Writer,
    fmt: Box<dyn PacketFormat>,
    st: PhantomData<St>,
}

impl<St: ProtocolState> Connection<St> {
    pub async fn write(&mut self, pkt: &St::Clientbound) -> io::Result<()> {
        let mut buf = Vec::new();
        pkt.write(&mut buf);

        self.fmt.send(&mut self.dest, buf.as_ref()).await
    }

    pub async fn read(&mut self) -> io::Result<St::Serverbound> {
        use crate::net::serialize::VecPacketDeserializer;

        let buf = self.fmt.recieve(&mut self.src).await?;

        St::Serverbound::read(&mut VecPacketDeserializer::new(buf.as_ref()))
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
    }

    fn into_state<NewSt: ProtocolState>(self) -> Connection<NewSt> {
        Connection {
            src: self.src,
            dest: self.dest,
            fmt: self.fmt,
            st: PhantomData,
        }
    }

    pub fn into_disconnected(self) -> Connection<!> {
        self.into_state()
    }
}

impl Connection<Handshake> {
    pub fn new(stream: TcpStream) -> Self {
        use tokio::io::{BufReader, BufWriter};

        let (src, dest) = stream.into_split();

        Connection {
            src: BufReader::new(src),
            dest: BufWriter::new(dest),
            fmt: Box::new(DefaultPacketFormat),
            st: PhantomData,
        }
    }

    pub fn into_status(self) -> Connection<Status> {
        self.into_state()
    }

    pub fn into_login(self) -> Connection<Login> {
        self.into_state()
    }
}

impl Connection<Login> {
    pub async fn set_compression(&mut self, threshold: Option<u32>) -> io::Result<()> {
        use crate::net::connection::packet_format::compressed::CompressedPacketFormat;
        use crate::net::serialize::VarInt;
        use crate::net::protocol::state::login::{Clientbound, SetCompression};

        // Tell the client about the new compression threshold,
        // using a packet compressed with the old compression threshold.
        self.write(&Clientbound::SetCompression(SetCompression {
            // A negative threshold will disable compression.
            threshold: VarInt(threshold.map(|x| x as i32).unwrap_or(-1)),
        })).await?;

        // Further packets will use the new compression threshold.
        match threshold {
            Some(threshold) => {
                self.fmt = Box::new(CompressedPacketFormat { threshold: threshold as usize });
            },
            None => {
                self.fmt = Box::new(DefaultPacketFormat);
            }
        }

        Ok(())
    }

    pub fn into_play(self) -> Connection<Play> {
        self.into_state()
    }
}
