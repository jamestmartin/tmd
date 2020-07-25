#![feature(const_generics)]
#![feature(never_type)]

mod net;

use crate::net::chat::Chat;
use crate::net::connection::Connection;
use crate::net::state::handshake::Handshake;
use crate::net::state::status::Status;
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

async fn accept_connection(socket: TcpStream) {
    let con = Connection::new(socket);

    eprintln!("Client connected.");
    match interact_handshake(con).await {
        Err(err) => { eprintln!("Client disconnected with error: {:?}", err); },
        Ok(_) => { eprintln!("Client disconnected without error."); }
    }
}

async fn interact_handshake<'a>(mut con: Connection<Handshake>) -> io::Result<()> {
    use crate::net::state::handshake::*;

    match con.read().await? {
        Serverbound::HandshakePkt(handshake) => {
            use HandshakeNextState::*;

            match handshake.next_state {
                Status => interact_status(con.into_status()).await,
                Login => Err(io::Error::new(io::ErrorKind::Other, "We do not support client log-in yet.".to_string()))
            }
        }
    }
}

async fn interact_status<'a>(mut con: Connection<Status>) -> io::Result<()> {
    use crate::net::state::status::*;

    loop {
        match con.read().await? {
            Serverbound::Request(Request {}) => {
                con.write(&Clientbound::Response(Response {
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
            Serverbound::Ping(ping) => {
                con.write(&Clientbound::Pong(Pong {
                    payload: ping.payload
                })).await?;

                // The status ping is now over so the server ends the connection.
                return Ok(());
            },
        }
    }
}
