module Main where

import Control.Category.Free
import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.Indexed
import Control.Monad.State (MonadIO)
import Data.Exists
import qualified Data.Vector as V
import Minecraft.Protocol
import Minecraft.Protocol.ByteReader
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction
import Minecraft.Protocol.State
import Minecraft.Protocol.State.Handshake
import Minecraft.Protocol.State.Login
import Minecraft.Protocol.State.Status
import Minecraft.Protocol.Version
import Network.Socket hiding (recv, Closed)

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "25565"
    bracket (open addr) close loop
  where resolve port = do
          let hints = defaultHints {
                  addrFlags = [ AI_PASSIVE, AI_NUMERICSERV ]
                , addrSocketType = Stream
                }
          addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
          return addr
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          setSocketOption sock ReuseAddr 1
          -- TODO: use withFdSocket when we hit network 3.1
          setCloseOnExecIfNeeded $ fdSocket sock
          bind sock (addrAddress addr)
          listen sock 10
          return sock
        loop sock = forever $ do
          (conn, peer) <- accept sock
          putStrLn $ "Connection from " ++ show peer
          void $ forkFinally (readBytes conn talk) (\_ -> close conn)

data PacketIOF (st :: ProtocolState) (st' :: ProtocolState) a where
  ReadPacket  :: SProtocolStateI st => PacketIOF st st (Packet 'Serverbound st)
  WritePacket :: Packet 'Clientbound st -> PacketIOF st st ()
  Transition  :: ProtocolStateTransition st st' -> PacketIOF st st' ()

type PacketIO = Program PacketIOF

runPacketIO :: forall r a. (MonadIO r, ByteReader r) => PacketIO 'Handshake 'Closed a -> r a
runPacketIO = runProgram runPacketIO'
  where runPacketIO' :: forall st st' a. PacketIOF st st' a -> r a
        runPacketIO' ReadPacket        = nextPacket :: r (Packet 'Serverbound st)
        runPacketIO' (WritePacket pkt) = writePacket pkt
        runPacketIO' (Transition _)    = return ()

talk :: forall r. (MonadIO r, ByteReader r) => r ()
talk = runPacketIO $ talk' SHandshake
  where talk' :: forall (st :: ProtocolState). SProtocolState st -> PacketIO st 'Closed ()
        talk' SClosed = ireturn ()
        talk' st = readPacket st `ibind` \pkt ->
          case pkt :: Packet 'Serverbound st of
            PktHandshake (PHandshake _ _ _ CloseConnection) -> transition CloseConnection :: PacketIO 'Handshake 'Closed ()
            PktHandshake (PHandshake _ _ _ HandshakeStatus) ->
              transition HandshakeStatus
              `iskip` talk' SStatus
            PktHandshake (PHandshake _ _ _ HandshakeLogin)  ->
              transition HandshakeLogin
              `iskip` talk' SLogin
            PktStatus Request ->
              writePacket (PktStatus $ Response $ StatusResponse
                { srVersion = PV498
                , srPlayersMax = 42
                , srPlayersOnline = 0
                , srPlayersSample = V.empty
                , srDescription = ChatText "Hello, world!"
                })
              `iskip` talk' SStatus
            PktStatus (Ping payload) ->
              writePacket (PktStatus $ Pong payload)
              `iskip` transition CloseConnection :: PacketIO 'Status 'Closed ()
            PktLogin (LoginStart name) ->
              writePacket (PktLogin $ LDisconnect $ ChatText "Login is not yet implemented.")
              `iskip` transition CloseConnection
        readPacket :: SProtocolState st -> PacketIO st st (Packet 'Serverbound st)
        readPacket st = rProtocolState st $ Command ReadPacket
        writePacket :: Packet 'Clientbound st -> PacketIO st st ()
        writePacket = Command . WritePacket
        transition :: ProtocolStateTransition st st' -> PacketIO st st' ()
        transition     = Command . Transition
