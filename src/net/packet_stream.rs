#[cfg(feature = "encryption")]
mod encryption;
mod packet_format;

use crate::net::packet_stream::packet_format::{AutoPacketFormat, PacketFormat};
use crate::net::protocol::packet_map::PacketMap;
use crate::net::protocol::state::handshake::Handshake;
use crate::net::protocol::state::login::Login;
use crate::net::protocol::state::play::Play;
use crate::net::protocol::state::status::Status;
use crate::net::protocol::state::ProtocolState;
use crate::net::Stream;
use async_trait::async_trait;
use std::io;
use std::marker::PhantomData;

/// Prevents outside types from implementing a trait.
mod sealed {
    pub trait Sealed {}
}
use sealed::Sealed;

/// A remote end of a connection: either a client or server.
///
/// It is impossible to implement this trait for other types.
pub trait Remote: Sealed {}
/// The remote end of a connection is a client.
pub enum Client {}
/// The remote end of a connection is a server.
pub enum Server {}
impl Sealed for Client {}
impl Sealed for Server {}
impl Remote for Client {}
impl Remote for Server {}

/// A packet compression theshold (i.e. the minimum packet size to compress).
#[cfg(feature = "compression")]
pub type CompressionThreshold = usize;
#[cfg(not(feature = "compression"))]
pub type CompressionThreshold = !;

/// A shared secret used for packet encryption.
#[cfg(feature = "encryption")]
pub type SharedSecret = [u8];
#[cfg(not(feature = "encryption"))]
pub type SharedSecret = !;

/// A stream of packets.
///
/// The type parameters are used to ensure using the type system
/// that you can only send and receive the correct type of packets.
pub struct PacketStream<Rem: Remote, St: ProtocolState> {
    inner: Box<dyn Stream>,
    compression_threshold: Option<CompressionThreshold>,
    encryption_enabled: bool,
    remote: PhantomData<Rem>,
    state: PhantomData<St>,
}

impl<Rem: Remote, St: ProtocolState> PacketStream<Rem, St> {
    /// Coerce the packet stream to any protocol state of your choosing.
    /// This can be used to break PacketStream's type-enforced correctness guarantees,
    /// so it should only be used internally.
    ///
    /// The purpose of this function is to reduce code duplication
    /// for all the pre-defined safe protocol state transitions.
    fn into_state<NewSt: ProtocolState>(self) -> PacketStream<Rem, NewSt> {
        PacketStream {
            inner: self.inner,
            compression_threshold: self.compression_threshold,
            encryption_enabled: self.encryption_enabled,
            remote: self.remote,
            state: PhantomData,
        }
    }

    /// Send any packet over the stream, ignoring the declared ProtocolState.
    /// This can be used to break PacketStream's type-enforced correctness guarantees,
    /// so it should only be used internally.
    ///
    /// The purpose of this function is to reduce code duplication
    /// while implementing [`PacketStreamMaps`]'s send and receive;
    /// the sending and recieving code is always going to be the same,
    /// but due to some shortcomings of Rust's type system
    /// (specifically, because there's no way to approximate type-level functions;
    /// in Haskell you could use multi-parameter type classes or type families),
    /// we can't implement this generically safely over Remote/ProtocolState combinations.
    async fn send_generic<Pkt: PacketMap>(&mut self, pkt: &Pkt) -> io::Result<()> {
        let mut contents = Vec::new();
        pkt.write(&mut contents);

        AutoPacketFormat(self.compression_threshold)
            .send(&mut self.inner, &contents)
            .await
    }

    /// Receive any packet from the stream, ignoring the declared ProtocolState.
    /// This can be used to break PacketStream's type-enforced correctness guarantees,
    /// so it should only be used internally.
    ///
    /// The purpose of this function is to reduce code duplication
    /// while implementing [`PacketStreamMaps`]'s send and receive;
    /// the sending and recieving code is always going to be the same,
    /// but due to some shortcomings of Rust's type system
    /// (specifically, because there's no way to approximate type-level functions;
    /// in Haskell you could use multi-parameter type classes or type families),
    /// we can't implement this generically safely over Remote/ProtocolState combinations.
    async fn receive_generic<Pkt: PacketMap>(&mut self) -> io::Result<Pkt> {
        use crate::net::serialize::VecPacketDeserializer;

        let buf = AutoPacketFormat(self.compression_threshold)
            .receive(&mut self.inner)
            .await?;

        Pkt::read(&mut VecPacketDeserializer::new(buf.as_ref()))
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
    }

    /// Transition to the protocol disconnected state,
    /// which prevents reading or writing any more packets.
    #[allow(dead_code)]
    pub fn into_disconnected(self) -> PacketStream<Rem, !> {
        self.into_state()
    }
}

/// A valid combination of inbound and outbound packet maps,
/// which are determined by the protocol state and remote.
#[async_trait]
pub trait PacketStreamMaps: Sealed {
    /// The kind of packets that can be received from the remote.
    type Inbound: PacketMap;
    /// The kind of packets that can be sent to the remote.
    type Outbound: PacketMap;

    /// Receive a packet from the remote.
    async fn receive(&mut self) -> io::Result<Self::Inbound>;

    /// Send a packet to the remote.
    async fn send(&mut self, pkt: &Self::Outbound) -> io::Result<()>;
}

impl<St: ProtocolState> Sealed for PacketStream<Client, St> {}
#[async_trait]
impl<St: ProtocolState> PacketStreamMaps for PacketStream<Client, St> {
    type Inbound = St::Serverbound;
    type Outbound = St::Clientbound;

    async fn receive(&mut self) -> io::Result<Self::Inbound> {
        self.receive_generic().await
    }

    async fn send(&mut self, pkt: &Self::Outbound) -> io::Result<()> {
        self.send_generic(pkt).await
    }
}

impl<St: ProtocolState> Sealed for PacketStream<Server, St> {}
#[async_trait]
impl<St: ProtocolState> PacketStreamMaps for PacketStream<Server, St> {
    type Inbound = St::Clientbound;
    type Outbound = St::Serverbound;

    async fn receive(&mut self) -> io::Result<Self::Inbound> {
        self.receive_generic().await
    }

    async fn send(&mut self, pkt: &Self::Outbound) -> io::Result<()> {
        self.send_generic(pkt).await
    }
}

impl<Rem: Remote> PacketStream<Rem, Handshake> {
    pub fn new(inner: Box<dyn Stream>) -> Self {
        Self {
            inner,
            compression_threshold: None,
            encryption_enabled: false,
            remote: PhantomData,
            state: PhantomData,
        }
    }

    /// Transition to the protocol status state.
    pub fn into_status(self) -> PacketStream<Rem, Status> {
        self.into_state()
    }

    /// Transition to the protocol login state.
    pub fn into_login(self) -> PacketStream<Rem, Login> {
        self.into_state()
    }
}

#[cfg(feature = "encryption")]
pub type InvalidKeyNonceLength = cfb8::stream_cipher::InvalidKeyNonceLength;
#[cfg(not(feature = "encryption"))]
pub type InvalidKeyNonceLength = !;

impl<Rem: Remote> PacketStream<Rem, Login> {
    /// Transition to the protocol play state.
    pub fn into_play(self) -> PacketStream<Rem, Play> {
        self.into_state()
    }

    /// Set the compression threshold, i.e. the minimum size of packet to compress.
    /// Setting the compression threshold to None disables compression.
    pub fn set_compression(&mut self, threshold: Option<CompressionThreshold>) {
        self.compression_threshold = threshold;
    }

    // I want to leave the function available even when encryption is disabled
    // so that users of PacketStream can use the `Option<SharedSecret>` pattern
    // to support encryption configuration without needing config features everywhere.

    /// Sets the shared secret, which enables encryption.
    /// Trying to enable encryption twice is an error, and the function will panic.
    pub fn enable_encryption(&mut self, shared_secret: &SharedSecret) -> Result<(), InvalidKeyNonceLength> {
        if self.encryption_enabled {
            panic!("Tried to enable encryption twice!");
        }

        #[cfg(feature = "encryption")]
        {
            use crate::net::packet_stream::encryption::EncryptedStream;
            use cfb8::stream_cipher::NewStreamCipher;
            use cfb8::Cfb8;

            let cipher: Cfb8<aes::Aes128> = Cfb8::new_var(shared_secret, shared_secret)?;
            take_mut::take(&mut self.inner, |inner| Box::new(EncryptedStream::new(inner, cipher)));

            Ok(())
        }

        #[cfg(not(feature = "encryption"))]
        *shared_secret
    }
}
