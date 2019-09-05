module Main where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.State (MonadIO)
import Data.Serialize (put)
import qualified Data.Vector as V
import Minecraft.Protocol
import Minecraft.Protocol.ByteReader
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction
import Minecraft.Protocol.State
import Minecraft.Protocol.State.Handshake
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
          void $ forkFinally (readBytes conn $ talk Handshake) (\_ -> close conn)

talk :: (MonadIO r, ByteReader r) => ProtocolState -> r ()
talk Closed = return ()
talk Login = error "Login state not yet supported."
talk Handshake = do
  pkt <- nextPacket :: (ByteReader r, MonadIO r) => r (Packet 'Handshake 'Serverbound)
  let (st, pktReply) = reply pkt
  case pktReply of
    Just pkt' -> writePacket $ put pkt'
    Nothing -> return ()
  talk st
talk Status = do
  pkt <- nextPacket :: (ByteReader r, MonadIO r) => r (Packet 'Status 'Serverbound)
  let (st, pktReply) = reply pkt
  case pktReply of
    Just pkt' -> writePacket $ put pkt'
    Nothing -> return ()
  talk st
talk Play = error "Play state not yet supported."

reply :: Packet st 'Serverbound -> (ProtocolState, Maybe (Packet st 'Clientbound))
reply (PktHandshake (PHandshake _ _ _ PSTClose))  = (Closed, Nothing)
reply (PktHandshake (PHandshake _ _ _ PSTStatus)) = (Status, Nothing)
reply (PktHandshake (PHandshake _ _ _ PSTLogin))  = (Login, Nothing)
reply (PktStatus Request) = (Status, Just $ PktStatus $ Response $ StatusResponse
  { srVersion = PV498
  , srPlayersMax = 42
  , srPlayersOnline = 0
  , srPlayersSample = V.empty
  , srDescription = ChatText "Hello, world!"
  })
reply (PktStatus (Ping payload)) = (Closed, Just $ PktStatus $ Pong payload)
