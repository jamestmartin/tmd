module Minecraft.Protocol where

import Data.Serialize (Serialize, get, put)
import Minecraft.Protocol.Direction
import Minecraft.Protocol.State
import Minecraft.Protocol.State.Handshake
import Minecraft.Protocol.State.Status

data Packet :: ProtocolState -> PacketDirection -> * where
  PktHandshake :: PacketHandshake dir -> Packet 'Handshake dir
  PktStatus    :: PacketStatus dir    -> Packet 'Status    dir

instance Serialize (Packet 'Handshake 'Serverbound) where
  get = PktHandshake <$> get
  put (PktHandshake pkt) = put pkt

instance Serialize (Packet 'Status    'Serverbound) where
  get = PktStatus <$> get
  put (PktStatus pkt) = put pkt

instance Serialize (Packet 'Closed    dir) where
  get   = fail  "No packets exist for a closed connection."
  put _ = error "No packets exist for a closed connection."

instance Serialize (Packet 'Handshake 'Clientbound) where
  get = PktHandshake <$> get
  put (PktHandshake pkt) = put pkt

instance Serialize (Packet 'Status    'Clientbound) where
  get = PktStatus <$> get
  put (PktStatus pkt) = put pkt
