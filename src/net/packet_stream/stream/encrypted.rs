use crate::net::packet_stream::stream::Stream;
use aes::Aes128;
use cfb8::stream_cipher::StreamCipher;
use cfb8::Cfb8;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use tokio::io::{AsyncRead, AsyncWrite, Result};

pub struct EncryptedStream {
    rw: Box<dyn Stream>,
    cipher: Cfb8<Aes128>,
    write_buf: Vec<u8>,
}

impl EncryptedStream {
    pub fn new(rw: Box<dyn Stream>, cipher: Cfb8<Aes128>) -> Self {
        Self {
            rw,
            cipher,
            write_buf: Vec::new(),
        }
    }

    fn flush(&mut self, cx: &mut Context) -> Poll<Result<()>> {
        // We don't know when the internal writer will be ready,
        // so we have to coax it into returning "pending" and scheduling an interrupt
        // for us. Either that, or we finish writing our buffer and flush.
        loop {
            if self.write_buf.is_empty() {
                break;
            }

            match Pin::new(&mut self.rw).poll_write(cx, &self.write_buf) {
                Poll::Ready(Ok(length)) => {
                    let mut new_buf = Vec::new();
                    new_buf.copy_from_slice(&self.write_buf[length..]);
                    self.write_buf = new_buf;
                },
                other => return other.map(|x| x.map(|_| ())),
            }
        }

        Pin::new(&mut self.rw).poll_flush(cx)
    }
}

impl AsyncRead for EncryptedStream {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context, buf: &mut [u8]) -> Poll<Result<usize>> {
        let me = Pin::into_inner(self);

        match Pin::new(&mut me.rw).poll_read(cx, buf) {
            Poll::Ready(Ok(bytes)) => {
                me.cipher.decrypt(&mut buf[..bytes]);
                Poll::Ready(Ok(bytes))
            },
            other => other,
        }
    }
}

impl AsyncWrite for EncryptedStream {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context, buf: &[u8]) -> Poll<Result<usize>> {
        let me = Pin::into_inner(self);

        let index = me.write_buf.len();
        // Copy data to our write buffer and then encrypt it.
        me.write_buf.extend_from_slice(buf);
        me.cipher.encrypt(&mut me.write_buf[index..]);

        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<()>> {
        Pin::into_inner(self).flush(cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Result<()>> {
        let me = Pin::into_inner(self);

        match me.flush(cx) {
            Poll::Ready(Ok(())) => Pin::new(&mut me.rw).poll_shutdown(cx),
            other => other,
        }
    }
}
