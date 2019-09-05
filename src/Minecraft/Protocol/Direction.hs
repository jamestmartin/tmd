module Minecraft.Protocol.Direction where

data PacketDirection = Clientbound | Serverbound

data SPacketDirection :: PacketDirection -> * where
  SClientbound :: SPacketDirection 'Clientbound
  SServerbound :: SPacketDirection 'Serverbound

class SPacketDirectionI (dir :: PacketDirection) where
  sPacketDirection :: SPacketDirection dir

instance SPacketDirectionI 'Clientbound where
  sPacketDirection = SClientbound

instance SPacketDirectionI 'Serverbound where
  sPacketDirection = SServerbound
