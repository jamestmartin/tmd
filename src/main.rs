mod net;

use crate::net::chat::Chat;
use crate::net::source;
use crate::net::source::{PacketError, PacketSource};
use tokio::io::BufWriter;
use tokio::net::{TcpListener, TcpStream};
use tokio::net::tcp::WriteHalf;
use std::io;
use std::net::IpAddr;

use PacketError::*;

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
    let (mut read, write) = socket.split();
    let mut source = PacketSource::new(&mut read);
    let mut dest = BufWriter::new(write);

    eprintln!("Client connected.");
    match interact_handshake(&mut source, &mut dest).await {
        Err(err) => { eprintln!("Client disconnected with error: {:?}", err); },
        Ok(_) => { eprintln!("Client disconnected without error."); }
    }
}

async fn interact_handshake(source: &mut PacketSource<'_>, dest: &mut BufWriter<WriteHalf<'_>>) -> source::Result<()> {
    use crate::net::packet::handshake::*;
    use PacketHandshakeServerbound::*;

    match read_packet_handshake(source).await? {
        Handshake(pkt) => {
            if pkt.next_state == HandshakeNextState::Status {
                interact_status(source, dest).await
            } else {
                Err(PktError("We do not support client log-in yet.".to_string()))
            }
        }
    }
}

async fn interact_status(source: &mut PacketSource<'_>, dest: &mut BufWriter<WriteHalf<'_>>) -> source::Result<()> {
    use crate::net::packet::status::*;
    use PacketStatusClientbound::*;
    use PacketStatusServerbound::*;

    loop {
        match read_packet_status(source).await? {
            Request => {
                match write_packet_status(dest, Response(PacketResponse {
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
                })).await {
                    Ok(_) => {},
                    Err(err) => return Err(IoError(err))
                }
            },
            Ping(payload) => {
                match write_packet_status(dest, Pong(payload)).await {
                    Ok(_) => {},
                    Err(err) => return Err(IoError(err))
                }
                return Ok(());
            }
        }
    }
}
