#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(never_type)]

mod net;

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

async fn listen(listener: TcpListener) {
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

async fn accept_connection(socket: TcpStream) -> Option<!> {
    use crate::net::chat::Chat;
    use crate::net::listener::*;
    use crate::net::packet_stream::PacketStreamMaps;
    use crate::net::protocol::state::play::*;

    #[cfg(not(feature = "compression"))]
    let compression_threshold = None;
    #[cfg(feature = "compression")]
    let compression_threshold = Some(64);

    #[cfg(not(feature = "encryption"))]
    let encryption_config = None;
    #[cfg(feature = "encryption")]
    let encryption_config = Some({
        use rsa::RSAPrivateKey;
        use std::fs::File;
        use std::io::Read;

        let mut public_key_bytes = Vec::new();
        File::open("private/pub.der")
            .expect("missing public key")
            .read_to_end(&mut public_key_bytes)
            .unwrap();

        let mut private_key_bytes = Vec::new();
        File::open("private/priv.der")
            .expect("missing private key")
            .read_to_end(&mut private_key_bytes)
            .unwrap();

        let private_key = RSAPrivateKey::from_pkcs1(&private_key_bytes).expect("Invalid private key.");

        EncryptionConfig {
            private_key,
            public_key_bytes: public_key_bytes.into_boxed_slice(),
        }
    });

    #[cfg(not(feature = "authentication"))]
    let authentication_config = None;
    #[cfg(feature = "authentication")]
    let authentication_config = Some(AuthenticationConfig::default());

    let config = ConnectionConfig {
        enforce_host: Some(vec!["localhost".to_string().into_boxed_str()].into_boxed_slice()),
        compression_threshold,
        encryption_config,
        authentication_config,
    };

    let (mut con, login_state) = setup_connection(&config, Box::new(socket), || {
        use crate::net::protocol::state::status::{ResponseData, ResponsePlayers, ResponseVersion};

        ResponseData {
            version: ResponseVersion {
                name: "1.16.1".to_string().into_boxed_str(),
                protocol: 736,
            },
            players: ResponsePlayers {
                max: 255,
                online: 0,
                sample: Vec::new(),
            },
            description: Chat {
                text: "Hello, world!".to_string().into_boxed_str(),
            },
            favicon: None,
        }
    })
    .await?;

    eprintln!("Client logged in as: {:?}", login_state);

    con.send(&Clientbound::Disconnect(Disconnect {
        reason: Chat {
            text: "Goodbye!".to_string().into_boxed_str(),
        },
    }))
    .await
    .ok()?;

    None
}
