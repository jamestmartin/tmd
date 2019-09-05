module Minecraft.Protocol.State.Handshake where

import Data.Serialize (Serialize, get, put)
import Data.Serialize.Get (getWord16be)
import Data.Serialize.Put (putWord16be)
import Data.Text (Text)
import Data.Word (Word16)
import Minecraft.Protocol.DataTypes (VarInt (VarInt), BytePrefixedText (BytePrefixedText))
import Minecraft.Protocol.Direction (PacketDirection (Clientbound, Serverbound))
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
    -> ProtocolStateTransition 'Handshake
    -> PacketHandshake 'Serverbound

instance Serialize (PacketHandshake 'Serverbound) where
  get = do
    VarInt 0 <- get
    AmbProtocolVersion ver <- get
    BytePrefixedText addr <- get
    port <- getWord16be
    next <- get
    return $ PHandshake ver addr port next

  put (PHandshake ver addr port next) = do
    put $ VarInt 0
    put $ AmbProtocolVersion ver
    put $ BytePrefixedText addr
    putWord16be port
    put next

instance Serialize (PacketHandshake 'Clientbound) where
  get   = fail  "There are no clientbound handshake packets."
  put _ = error "There are no clientbound handshake packets."
