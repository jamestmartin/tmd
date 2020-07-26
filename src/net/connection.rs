mod packet_format;
mod stream;

use crate::net::connection::packet_format::PacketFormat;
use crate::net::connection::packet_format::default::DefaultPacketFormat;
use crate::net::connection::stream::Stream;
use crate::net::protocol::packet_map::PacketMap;
use crate::net::protocol::state::ProtocolState;
use crate::net::protocol::state::handshake::Handshake;
use crate::net::protocol::state::login::Login;
use crate::net::protocol::state::play::Play;
use crate::net::protocol::state::status::Status;
use std::io;
use std::marker::PhantomData;
use tokio::io::BufStream;
use tokio::net::TcpStream;

pub struct Connection<St: ProtocolState> {
    rw: Box<dyn Stream>,
    fmt: Box<dyn PacketFormat>,
    st: PhantomData<St>,
}

impl<St: ProtocolState> Connection<St> {
    pub async fn write(&mut self, pkt: &St::Clientbound) -> io::Result<()> {
        // Turn the packet into bytes.
        let mut contents = Vec::new();
        pkt.write(&mut contents);

        // Send the packet with the appropriate header.
        self.fmt.send(&mut self.rw, &contents).await
    }

    pub async fn read(&mut self) -> io::Result<St::Serverbound> {
        use crate::net::serialize::VecPacketDeserializer;

        let buf = self.fmt.recieve(&mut self.rw).await?;

        St::Serverbound::read(&mut VecPacketDeserializer::new(buf.as_ref()))
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
    }

    fn into_state<NewSt: ProtocolState>(self) -> Connection<NewSt> {
        Connection {
            rw: self.rw,
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
        Connection {
            rw: Box::new(BufStream::new(stream)),
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
    #[cfg(feature = "compression")]
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
                self.fmt = Box::new(CompressedPacketFormat::new(threshold as usize));
            },
            None => {
                self.fmt = Box::new(DefaultPacketFormat);
            }
        }

        Ok(())
    }

    /// WARNING: This function is not idempontent.
    /// Calling it twice will result in the underlying stream getting encrypted twice.
    #[cfg(feature = "encryption")]
    pub fn set_encryption(self, secret: &[u8]) -> Result<Self, String> {
        use cfb8::Cfb8;
        use cfb8::stream_cipher::NewStreamCipher;
        use crate::net::connection::stream::encrypted::EncryptedStream;

        let cipher: Cfb8<aes::Aes128> = Cfb8::new_var(secret, secret).map_err(|err| err.to_string())?;

        Ok(Connection {
            rw: Box::new(EncryptedStream::new(self.rw, cipher)),
            fmt: self.fmt,
            st: PhantomData,
        })
    }

    pub fn into_play(self) -> Connection<Play> {
        self.into_state()
    }
}
