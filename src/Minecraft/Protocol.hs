module Minecraft.Protocol where

import Minecraft.Protocol.DataTypes (Serialize, Deserialize, serialize, deserialize)
import Minecraft.Protocol.Direction
import Minecraft.Protocol.State
import Minecraft.Protocol.State.Handshake
import Minecraft.Protocol.State.Login
import Minecraft.Protocol.State.Status

data Packet :: PacketDirection -> ProtocolState -> * where
  PktHandshake :: PacketHandshake dir -> Packet dir 'Handshake
  PktStatus    :: PacketStatus dir    -> Packet dir 'Status
  PktLogin     :: PacketLogin dir     -> Packet dir 'Login

instance (SProtocolStateI st, SPacketDirectionI dir) => Deserialize (Packet dir st) where
  deserialize = a \case
    SClosed    -> fail  "No packets exist for a closed connection."
    SHandshake -> PktHandshake <$> deserialize
    SStatus    -> PktStatus    <$> deserialize
    SLogin     -> PktLogin     <$> deserialize
    _          -> undefined
    where a :: (SProtocolStateI st, SPacketDirectionI dir) => (SProtocolState st -> f (g dir st)) -> f (g dir st)
          a f = f sProtocolState

instance Serialize (Packet dir st) where
  serialize (PktHandshake pkt) = serialize pkt
  serialize (PktStatus pkt) = serialize pkt
  serialize (PktLogin pkt) = serialize pkt

class MonadProtocol m where
  sendPacket :: Packet dir -> m dir ()
  recvPacket :: SPacketDirectionI dir => m dir Packet
