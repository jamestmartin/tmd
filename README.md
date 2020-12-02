# tmd
A Minecraft protocol-compatible server written from scratch in Rust.

Supports Minecraft version 1.16.1 (protocol version 736),
including protocol compression, encryption, and authentication.

Most of the protocol infrastructure is there, so complete protocol support
is mostly a matter of defining Play state packets with my handy macros.
The protocol infrastructure is actually written bi-directionally
and in theory should work as a protocol client as well,
although that is not currently actively persued or tested.

## Features
### Compression
The server will use protocol compression by default.
You can disable it by disabling the crate's `compression` feature,
although disabling it would probably be pointless.

The compression threshold is 64 bytes.

### Encryption
The server will also use protocol encryption by default.
You can disable it by disabling the crate's `encryption` feature.

To use encryption, you will need to a 1024-bit RSA keypair
and store it in DER format as `private/pub.der` and `private/priv.der`.

You can do that with OpenSSL using these comamnds:

```
openssl genpkey -algorithm RSA -aes256 -pkeyopt rsa_keygen_bits:1024 -outform DER -out priv.der
openssl rsa -pubout -inform DER -in priv.der -outform DER -out pub.der
```

### Authentication
The server will run in 'online mode' by default,
which means only players authenticated with Mojang can connect.
You can disable it by disabling the crate's `authentication` feature.
This feature also depends on the `encryption` feature.

**If you wish to set your server to 'offline mode', you will have to disable encryption as well.**
Although disabling authentication means that the server will allow unauthenticated clients to connect,
the official client will disconnect from an encrypted server if it fails to authenticate with Mojang.
To be able to connect to a server with authentication off and encryption on,
you would either need to be logged in (which defeats the point) or use a modified client.
To my knowedge, no such modified client currently exists.
