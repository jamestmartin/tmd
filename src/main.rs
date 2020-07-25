#![feature(const_generics)]

mod net;

use crate::net::{Reader, Writer};
use crate::net::chat::Chat;
use crate::net::format::{PacketFormat, DefaultPacketFormat};
use tokio::io::{BufReader, BufWriter};
use tokio::net::{TcpListener, TcpStream};
use std::io;
use std::net::IpAddr;

#[tokio::main]
async fn main() -> io::Result<()> {
    use clap::{App, load_yaml};

    let yaml = load_yaml!("cli.yml");
    let args = App::from_yaml(yaml).get_matches();
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

    let pkt = DefaultPacketFormat.recieve::<HandshakeServerbound>(source).await?;

    match pkt {
        HandshakeServerbound::Handshake(handshake) => {
            if handshake.next_state == HandshakeNextState::Status {
                interact_status(source, dest).await
            } else {
                Err(io::Error::new(io::ErrorKind::Other, "We do not support client log-in yet.".to_string()))
            }
        }
    }
}

async fn interact_status<'a>(source: &mut Reader<'a>, dest: &mut Writer<'a>) -> io::Result<()> {
    use crate::net::packet::status::*;

    loop {
        let pkt = DefaultPacketFormat.recieve::<StatusServerbound>(source).await?;

        match pkt {
            StatusServerbound::Request(Request {}) => {
                DefaultPacketFormat.send(dest, &StatusClientbound::Response(Response {
                    data: ResponseData {
                        version: ResponseVersion {
                            name: "1.16.1".to_string(),
                            protocol: 736,
                        },
                        players: ResponsePlayers {
                            max: 255,
                            online: 0,
                            sample: Vec::new(),
                        },
                        description: Chat { text: "Hello, world!".to_string() },
                        favicon: None,
                    }
                })).await?;
            },
            StatusServerbound::Ping(ping) => {
                DefaultPacketFormat.send(dest, &StatusClientbound::Pong(Pong {
                    payload: ping.payload
                })).await?;

                // The status ping is now over so the server ends the connection.
                return Ok(());
            },
        }
    }
}
