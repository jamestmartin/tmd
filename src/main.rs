#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(never_type)]

mod net;

use crate::net::chat::Chat;
use crate::net::connection::Connection;
use crate::net::protocol::state::handshake::Handshake;
use crate::net::protocol::state::login::Login;
use crate::net::protocol::state::play::Play;
use crate::net::protocol::state::status::Status;
use std::io;
use std::net::IpAddr;
use tokio::net::{TcpListener, TcpStream};

#[tokio::main]
async fn main() -> io::Result<()> {
    use clap::{load_yaml, App};

    let yaml = load_yaml!("cli.yml");
    let args = App::from_yaml(yaml).get_matches();
    let host: IpAddr = args
        .value_of("host")
        .unwrap_or("::")
        .parse()
        .expect("Invalid host IP address.");
    let port: u16 = args
        .value_of("port")
        .unwrap_or("25565")
        .parse()
        .expect("Port must be an integer between 1 an 65535.");

    let listener = TcpListener::bind((host, port))
        .await
        .unwrap_or_else(|_| panic!("Failed to bind to {}:{}.", host, port));

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
            },
        };

        tokio::spawn(accept_connection(socket));
    }
}

async fn accept_connection(socket: TcpStream) {
    let con = Connection::new(socket);

    eprintln!("Client connected.");
    match interact_handshake(con).await {
        Err(err) => {
            eprintln!("Client disconnected with error: {:?}", err);
        },
        Ok(_) => {
            eprintln!("Client disconnected without error.");
        },
    }
}

fn mk_err<A, S: std::borrow::Borrow<str>>(str: S) -> io::Result<A> {
    Err(io::Error::new(io::ErrorKind::Other, str.borrow().to_string()))
}

async fn interact_handshake(mut con: Connection<Handshake>) -> io::Result<()> {
    use crate::net::protocol::state::handshake::*;

    match con.read().await? {
        Serverbound::HandshakePkt(handshake) => {
            use HandshakeNextState::*;

            match handshake.next_state {
                Status => interact_status(con.into_status()).await,
                Login => interact_login(con.into_login()).await,
            }
        },
    }
}

async fn interact_status(mut con: Connection<Status>) -> io::Result<()> {
    use crate::net::protocol::state::status::*;

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
                        description: Chat {
                            text: "Hello, world!".to_string(),
                        },
                        favicon: None,
                    },
                }))
                .await?;
            },
            Serverbound::Ping(ping) => {
                con.write(&Clientbound::Pong(Pong { payload: ping.payload })).await?;

                // The status ping is now over so the server ends the connection.
                return Ok(());
            },
        }
    }
}

async fn interact_login(mut con: Connection<Login>) -> io::Result<()> {
    use crate::net::protocol::state::login::*;

    let name = match con.read().await? {
        Serverbound::LoginStart(login_start) => login_start.name,
        _ => {
            con.write(&Clientbound::Disconnect(Disconnect {
                reason: Chat {
                    text: "Unexpected packet (expected Login Start).".to_string(),
                },
            }))
            .await?;
            return mk_err("Unexpected packet (expected Login Start).");
        },
    };

    #[cfg(feature = "encryption")]
    {
        use rand::rngs::OsRng;
        use rand::Rng;
        use rsa::{PaddingScheme, RSAPrivateKey};

        use std::fs::File;
        use std::io::Read;
        let mut public_key = Vec::new();
        File::open("private/pub.der")
            .expect("missing public key")
            .read_to_end(&mut public_key)?;

        let mut private_key = Vec::new();
        File::open("private/priv.der")
            .expect("missing private key")
            .read_to_end(&mut private_key)?;

        let key = RSAPrivateKey::from_pkcs1(&private_key).expect("Invalid private key.");

        let mut verify_token = Vec::new();
        verify_token.resize(4, 0u8);
        OsRng.fill(verify_token.as_mut_slice());

        let server_id = "";

        con.write(&Clientbound::EncryptionRequest(EncryptionRequest {
            server_id: server_id.to_string().into_boxed_str(),
            public_key: public_key.clone().into_boxed_slice(),
            verify_token: verify_token.clone().into_boxed_slice(),
        }))
        .await?;

        let secret = match con.read().await? {
            Serverbound::EncryptionResponse(encryption_response) => {
                let token = key
                    .decrypt(PaddingScheme::PKCS1v15Encrypt, &encryption_response.verify_token)
                    .expect("Failed to decrypt verify token.");
                if token.as_slice() != verify_token.as_slice() {
                    return mk_err("Incorrect verify token.");
                }

                key.decrypt(PaddingScheme::PKCS1v15Encrypt, &encryption_response.shared_secret)
                    .expect("Failed to decrypt shared secret.")
            },
            _ => {
                return mk_err("Unexpected packet (expected Encryption Response).");
            },
        };

        con = con.set_encryption(&secret).expect("Failed to set encryption.");

        #[cfg(feature = "authentication")]
        {
            let server_hash = {
                let server_hash_bytes = {
                    use sha1::{Digest, Sha1};
                    let mut hasher = Sha1::new();
                    hasher.update(server_id.as_bytes());
                    hasher.update(&secret);
                    hasher.update(&public_key);
                    hasher.finalize()
                };

                format!("{:x}", num_bigint::BigInt::from_signed_bytes_be(&server_hash_bytes))
            };

            use reqwest::Client;

            println!(
                "{:?}",
                Client::new()
                    .get("https://sessionserver.mojang.com/session/minecraft/hasJoined")
                    .header("Content-Type", "application/json")
                    .query(&[("username", name.clone()), ("serverId", server_hash.into_boxed_str())])
                    .send()
                    .await
                    .expect("Request failed.")
                    .text()
                    .await
                    .unwrap()
            );
        }
    }

    #[cfg(feature = "compression")]
    con.set_compression(Some(64)).await?;

    con.write(&Clientbound::LoginSuccess(LoginSuccess {
        uuid: uuid::Uuid::nil(),
        username: name,
    }))
    .await?;

    interact_play(con.into_play()).await
}

async fn interact_play(mut con: Connection<Play>) -> io::Result<()> {
    use crate::net::protocol::state::play::*;

    con.write(&Clientbound::Disconnect(Disconnect {
        reason: Chat {
            text: "Goodbye!".to_string(),
        },
    }))
    .await?;

    Ok(())
}
