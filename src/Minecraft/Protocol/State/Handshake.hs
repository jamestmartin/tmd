module Minecraft.Protocol.State.Handshake where

import Data.Attoparsec.Zepto
import Data.ByteString.Builder (word16BE)
import Data.Exists
import Data.Text (Text)
import Data.Word (Word16)
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction
import Minecraft.Protocol.State (ProtocolState (Handshake), ProtocolStateTransition)
import Minecraft.Protocol.Version

type ServerAddress = Text

type ServerPort = Word16

data PacketHandshake :: PacketDirection -> * where
  -- | https://wiki.vg/Server_List_Ping#Handshake
  -- | The first packet sent from the client to the server on initiating a connection.
  PHandshake
    -- | An unspecified 'ProtocolVersion' means the client is querying what version the server prefers.
    :: Maybe ProtocolVersion
    -- | The server address is the address (hostname or IP) the client used to connect, after any SRV records have been resolved.
    -> ServerAddress
    -- | The port the client used to connect.
    -> ServerPort
    -- | The next protocol state to be transitioned to.
    -> ProtocolStateTransition 'Handshake st'
    -> PacketHandshake 'Serverbound

instance Serialize (PacketHandshake dir) where
  serialize (PHandshake ver addr port next) =
    serialize (VarInt 0)
    <> serialize (AmbProtocolVersion ver)
    <> serialize addr
    <> word16BE port
    <> serialize (Exists next)

instance SPacketDirectionI dir => Deserialize (PacketHandshake dir) where
  deserialize = a \case
    SClientbound -> fail "There are no clientbound handshake packets."
    SServerbound -> do
      VarInt 0 <- deserialize
      AmbProtocolVersion ver <- deserialize
      addr <- deserialize
      port <- anyWord16BE
      Exists next <- deserialize
      return $ PHandshake ver addr port next
    where a :: SPacketDirectionI dir => (SPacketDirection dir -> f (g dir)) -> f (g dir)
          a f = f sPacketDirection

  
