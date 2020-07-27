use crate::net::chat::Chat;
pub use crate::net::packet_stream::CompressionThreshold;
use crate::net::packet_stream::{Client, PacketStream, PacketStreamMaps};
use crate::net::protocol::state::handshake;
use crate::net::protocol::state::login;
use crate::net::protocol::state::login::Login;
use crate::net::protocol::state::play::Play;
use crate::net::protocol::state::status;
use crate::net::protocol::state::status::{ResponseData, Status};
use crate::net::Stream;
use uuid::Uuid;

/// The configuration necessary to use Minecraft protocol encryption.
#[cfg(feature = "encryption")]
#[derive(Clone)]
pub struct EncryptionConfig {
    /// An RSA key pair. The Notchian server uses a 1024-bit key.
    /// You don't need to worry about signing it or anything like that,
    /// because it isn't ever checked for authenticity.
    ///
    /// The Notchian server generates a fresh key every time the server starts.
    /// The rsa crate allows doing something similar using [`rsa::RSAPrivateKey::new`],
    /// but this is unusable because you need a DER-formatted public key for [`public_key_bytes`],
    /// and currently the rsa crate does not allow creating one.
    /// Instead, I would recommend reading a private key using [`rsa::RSAPrivateKey::from_pkcs1`].
    ///
    /// You can generate a suitable private key for this purpose using OpenSSL:
    ///
    /// ```bash
    /// openssl genpkey -algorithm RSA -outform der -pkeyopt rsa_keygen_bits:1024 -out priv.der
    /// ```
    pub private_key: rsa::RSAPrivateKey,

    /// The verbatim bytes of a DER-encoded RSA public key.
    ///
    /// You can generate this from your private key using OpenSSL:
    ///
    /// ```bash
    /// openssl rsa -pubout -inform der -outform der -in priv.der -out pub.der
    /// ```
    ///
    /// There is no reason it shouldn't be possible to extract this from the `private_key`,
    /// but the crate I use for RSA doesn't currently support it.
    pub public_key_bytes: Box<[u8]>,
}
#[cfg(not(feature = "encryption"))]
type EncryptionConfig = !;

/// The configuration necessary to allow the server to run in online mode.
/// The default value will be suitable in most cases.
#[cfg(feature = "authentication")]
#[derive(Clone)]
pub struct AuthenticationConfig {
    /// This is the base URL that the server will send the player's username
    /// and the shared server id hash to to check whether the user is who they claim to be.
    ///
    /// There's no real need to know the technical details:
    /// if you don't know what this is, just use the default value,
    /// which will allow login using Mojang/minecraft.net accounts,
    /// which is probably what you want.
    pub has_joined_endpoint: reqwest::Url,

    /// Enable a limited form of defense against
    /// proxied connections using the authentication server.
    /// This checks if the IP the client authenticated with
    /// is the same as the IP of the client connected to the server,
    /// and if it's not, the client is kicked.
    pub prevent_proxy_connections: bool,

    /// Allow players to join the server even if authentication fails,
    /// treating them as though the server were in offline mode.
    /// **You probably want this to be false.**
    pub allow_unauthenticated_players: bool,
}
#[cfg(not(feature = "authentication"))]
type AuthenticationConfig = !;

#[cfg(feature = "authentication")]
impl std::default::Default for AuthenticationConfig {
    fn default() -> Self {
        Self {
            has_joined_endpoint: reqwest::Url::parse("https://sessionserver.mojang.com/session/minecraft/hasJoined")
                .unwrap(),
            prevent_proxy_connections: false,
            allow_unauthenticated_players: false,
        }
    }
}

/// Configuration for how connections with clients should be set up.
#[derive(Clone)]
pub struct ConnectionConfig {
    /// Enforce that one of the correct hostnames or IP addresses is used to connect to the server.
    /// This gives you flexibility as a server admin
    /// (e.g. you can update your SRV record or IP address without losing players
    /// who are connecting directly via IP, because they never count),
    /// and adds security (to prevent someone from pointing
    /// to their name and then maliciously changing it later).
    pub enforce_host: Option<Box<[Box<str>]>>,

    /// The protocol compression theshold (i.e. the minimum packet size to compress).
    /// Setting this to `None` disables compression.
    pub compression_threshold: Option<CompressionThreshold>,

    /// The protocol encryption configuration.
    /// Setting this to `None` disables encryption *and authentication, which requires encryption*.
    pub encryption_config: Option<EncryptionConfig>,

    /// The authentication configuration.
    /// Setting this to `None` disables authentication.
    /// *Authentication requires encryption, so if encryption is disabled, this will be ignored.*
    pub authentication_config: Option<AuthenticationConfig>,
}

#[derive(Debug)]
pub struct LoginState {
    name: Box<str>,
    /// None if the user is logged in but not authenticated.
    uuid: Option<Uuid>,
}

pub async fn setup_connection<F>(
    cfg: &ConnectionConfig,
    stream: Box<dyn Stream>,
    get_status: F,
) -> Option<(PacketStream<Client, Play>, LoginState)>
where
    F: FnOnce() -> ResponseData,
{
    let mut con = PacketStream::new(stream);

    let handshake::Serverbound::HandshakePkt(handshake_pkt) = con.receive().await.ok()?;

    let mut con = match handshake_pkt.next_state {
        handshake::HandshakeNextState::Status => {
            respond_status(con.into_status(), get_status).await?;
        },
        handshake::HandshakeNextState::Login => con.into_login(),
    };

    if let Some(hosts) = &cfg.enforce_host {
        enforce_hosts(&mut con, &hosts, &handshake_pkt.server_address).await?;
    }

    use login::*;

    let name = match con.receive().await.ok()? {
        Serverbound::LoginStart(LoginStart { name }) => name,
        _ => {
            con.send(&Clientbound::Disconnect(Disconnect {
                reason: Chat {
                    text: "You're supposed to send a LoginStart packet!"
                        .to_string()
                        .into_boxed_str(),
                },
            }))
            .await
            .ok()?;

            return None;
        },
    };

    let uuid = if let Some(ecfg) = &cfg.encryption_config {
        let shared_secret = enable_encryption(&mut con, &ecfg).await?;
        if let Some(acfg) = &cfg.authentication_config {
            authenticate(&mut con, &ecfg, &acfg, &name, &shared_secret).await?
        } else {
            None
        }
    } else {
        None
    };

    if let Some(threshold) = cfg.compression_threshold {
        con.send(&Clientbound::SetCompression(SetCompression {
            threshold: (threshold as i32).into(),
        }))
        .await
        .ok()?;

        con.set_compression(Some(threshold));
    }

    con.send(&Clientbound::LoginSuccess(LoginSuccess {
        uuid: uuid.unwrap_or_else(Uuid::nil),
        username: name.clone(),
    }))
    .await
    .ok()?;

    let con = con.into_play();

    let login_state = LoginState { name, uuid };

    Some((con, login_state))
}

async fn respond_status<F>(mut con: PacketStream<Client, Status>, get_status: F) -> Option<!>
where
    F: FnOnce() -> ResponseData,
{
    use status::*;

    // There's no reason theoretically that we couldn't
    // accept these packets repeated or out of order,
    // but the `setup_connection` function is supposed to return in finite time,
    // and looping here would violate that contract.

    match con.receive().await.ok()? {
        Serverbound::Request(_) => {
            con.send(&Clientbound::Response(Response { data: get_status() }))
                .await
                .ok()?;
        },
        // That's not how the status ping is supposed to work!
        _ => return None,
    }

    match con.receive().await.ok()? {
        Serverbound::Ping(status::Ping { payload }) => {
            con.send(&Clientbound::Pong(Pong { payload })).await.ok()?;
        },
        _ => return None,
    }

    None
}

async fn enforce_hosts(con: &mut PacketStream<Client, Login>, hosts: &[Box<str>], server_address: &str) -> Option<()> {
    use login::*;

    if hosts.iter().any(|host| host.as_ref() == server_address) {
        return Some(());
    }

    let mut reason = "You can't connect to this server through that IP or domain.\n\
         Please use one of these instead:\n"
        .to_string();
    for host in hosts {
        reason.push_str(host);
        reason.push('\n');
    }

    con.send(&Clientbound::Disconnect(Disconnect {
        reason: Chat {
            text: reason.into_boxed_str(),
        },
    }))
    .await
    .ok()?;

    None
}

#[cfg(not(feature = "encryption"))]
async fn enable_encryption(_con: &mut PacketStream<Client, Login>, cfg: &EncryptionConfig) -> Option<Box<[u8]>> {
    *cfg
}

#[cfg(feature = "encryption")]
async fn enable_encryption(con: &mut PacketStream<Client, Login>, cfg: &EncryptionConfig) -> Option<Box<[u8]>> {
    use login::*;

    use rand::rngs::OsRng;
    use rand::Rng;
    use rsa::padding::PaddingScheme;

    let mut verify_token = Vec::new();
    verify_token.resize(4, 0u8);
    OsRng.fill(verify_token.as_mut_slice());

    con.send(&Clientbound::EncryptionRequest(EncryptionRequest {
        server_id: "".to_string().into_boxed_str(),
        public_key: cfg.public_key_bytes.clone(),
        verify_token: verify_token.clone().into_boxed_slice(),
    }))
    .await
    .ok()?;

    let shared_secret: Box<[u8]> = match con.receive().await.ok()? {
        Serverbound::EncryptionResponse(EncryptionResponse {
            shared_secret,
            verify_token: encrypted_verify_token,
        }) => {
            let decrypted_verify_token = cfg
                .private_key
                .decrypt(PaddingScheme::PKCS1v15Encrypt, &encrypted_verify_token)
                .ok()?;

            if decrypted_verify_token != verify_token {
                con.send(&Clientbound::Disconnect(Disconnect {
                    reason: Chat {
                        text: "Verify token was not encrypted correctly.".to_string().into_boxed_str(),
                    },
                }))
                .await
                .ok()?;

                return None;
            }

            cfg.private_key
                .decrypt(PaddingScheme::PKCS1v15Encrypt, &shared_secret)
                .ok()?
                .into_boxed_slice()
        },
        _ => {
            con.send(&Clientbound::Disconnect(Disconnect {
                reason: Chat {
                    text: "You're supposed to send an EncryptionResponse packet!"
                        .to_string()
                        .into_boxed_str(),
                },
            }))
            .await
            .ok()?;

            return None;
        },
    };

    con.enable_encryption(&shared_secret).ok()?;

    Some(shared_secret)
}

#[cfg(feature = "authentication")]
#[derive(serde::Deserialize)]
struct HasJoinedResponse {
    // The response also contains a signature,
    // but I don't need to check it because I use HTTPS.
    // There's also additional properties like the player's skin,
    // but I don't care about those for now.
    id: Uuid,
}

#[cfg(not(feature = "authentication"))]
async fn authenticate(
    _con: &mut PacketStream<Client, Login>,
    _ecfg: &EncryptionConfig,
    acfg: &AuthenticationConfig,
    _name: &str,
    _shared_secret: &[u8],
) -> Option<Option<Uuid>> {
    *acfg
}

#[cfg(feature = "authentication")]
async fn authenticate(
    con: &mut PacketStream<Client, Login>,
    ecfg: &EncryptionConfig,
    acfg: &AuthenticationConfig,
    name: &str,
    shared_secret: &[u8],
) -> Option<Option<Uuid>> {
    use login::*;

    let result = try_authenticate(ecfg, acfg, name, shared_secret).await;

    if acfg.allow_unauthenticated_players {
        return Some(result);
    }

    match result {
        Some(uuid) => Some(Some(uuid)),
        None => {
            con.send(&Clientbound::Disconnect(Disconnect {
                reason: Chat {
                    text: "Authentication failed.".to_string().into_boxed_str(),
                },
            }))
            .await
            .ok()?;

            None
        },
    }
}

#[cfg(feature = "authentication")]
async fn try_authenticate(
    ecfg: &EncryptionConfig,
    acfg: &AuthenticationConfig,
    name: &str,
    shared_secret: &[u8],
) -> Option<Uuid> {
    use num_bigint::BigInt;
    use reqwest::Client;
    use sha1::{Digest, Sha1};

    let server_hash = {
        let server_hash_bytes = {
            let mut hasher = Sha1::new();
            hasher.update(b"");
            hasher.update(shared_secret);
            hasher.update(ecfg.public_key_bytes.clone());
            hasher.finalize()
        };

        format!("{:x}", BigInt::from_signed_bytes_be(&server_hash_bytes)).into_boxed_str()
    };

    // TODO: Allow checking IPs for the anti-proxy feature.
    let response_body = Client::new()
        .get(acfg.has_joined_endpoint.clone())
        .header("Content-Type", "application/json")
        .query(&[("username", name), ("serverId", &server_hash)])
        .send()
        .await
        .ok()?
        .text()
        .await
        .ok()?;

    let response: HasJoinedResponse = serde_json::from_str(&response_body).ok()?;

    Some(response.id)
}
