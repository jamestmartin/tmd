#![feature(const_generics)]

mod net;

use crate::net::{Reader, Writer};
use crate::net::chat::Chat;
use crate::net::format::{PacketFormat, DefaultPacketFormat};
use crate::net::serialize::VecPacketDeserializer;
use tokio::io::{BufReader, BufWriter};
use tokio::net::{TcpListener, TcpStream};
use std::io;
use std::net::IpAddr;

#[tokio::main]
async fn main() -> io::Result<()> {
    let yaml = clap::load_yaml!("cli.yml");
    let args = clap::App::from_yaml(yaml).get_matches();
    let host: IpAddr = args.value_of("host").unwrap_or("::").parse()
        .expect("Invalid host IP address.");
    let port: u16 = args.value_of("port").unwrap_or("25565").parse()
        .expect("Port must be an integer between 1 an 65535.");

    let listener = TcpListener::bind((host, port)).await
        .expect(&format!("Failed to bind to {}:{}.", host, port));

    listen(listener).await;

    Ok(())
}

async fn listen(mut listener: TcpListener) {
    loop {
        let (socket, _) = match listener.accept().await {
            Ok(x) => x,
            Err(e) => {
                eprintln!("Failed to accept client: {:?}", e);
                continue;
            }
        };

        tokio::spawn(accept_connection(socket));
    }
}

async fn accept_connection(mut socket: TcpStream) {
    let (read, write) = socket.split();
    let mut source = BufReader::new(read);
    let mut dest = BufWriter::new(write);

    eprintln!("Client connected.");
    match interact_handshake(&mut source, &mut dest).await {
        Err(err) => { eprintln!("Client disconnected with error: {:?}", err); },
        Ok(_) => { eprintln!("Client disconnected without error."); }
    }
}

async fn interact_handshake<'a>(source: &mut Reader<'a>, dest: &mut Writer<'a>) -> io::Result<()> {
    use crate::net::packet::handshake::*;
    use PacketHandshakeServerbound::*;

    let (id, data) = DefaultPacketFormat.recieve(source).await?;
    let mut deser = VecPacketDeserializer::new(&data);

    match read_packet_handshake(id, &mut deser)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err)) {
        Ok(pkt) => match pkt {
            Handshake(pkt) => {
                if pkt.next_state == HandshakeNextState::Status {
                    interact_status(source, dest).await
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, "We do not support client log-in yet.".to_string()))
                }
            }
        },
        Err(err) => Err(io::Error::new(io::ErrorKind::Other, err))
    }
}

async fn interact_status<'a>(source: &mut Reader<'a>, dest: &mut Writer<'a>) -> io::Result<()> {
    use crate::net::packet::status::*;
    use PacketStatusClientbound::*;
    use PacketStatusServerbound::*;

    loop {
        let (id, data) = DefaultPacketFormat.recieve(source).await?;
        let mut deser = VecPacketDeserializer::new(&data);

        match read_packet_status(id, &mut deser)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, err)) {
            Ok(pkt) => match pkt {
                Request => {
                    let mut buf = Vec::new();
                    let id = write_packet_status(&mut buf, Response(PacketResponse {
                        version: PacketResponseVersion {
                            name: "1.16.1".to_string(),
                            protocol: 736,
                        },
                        players: PacketResponsePlayers {
                            max: 255,
                            online: 0,
                            sample: Vec::new(),
                        },
                        description: Chat { text: "Hello, world!".to_string() },
                        favicon: None,
                    }));
                    DefaultPacketFormat.send(dest, id, buf.as_slice()).await?;
                },
                Ping(payload) => {
                    let mut buf = Vec::new();
                    let id = write_packet_status(&mut buf, Pong(payload));
                    DefaultPacketFormat.send(dest, id, buf.as_slice()).await?;

                    return Ok(());
                }
            },
            Err(err) => return Err(io::Error::new(io::ErrorKind::Other, err))
        }
    }
}
