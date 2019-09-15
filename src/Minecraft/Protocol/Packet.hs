class Minecraft.Protocol.Packet where

import Data.NetEncoding

class Packet a where
  decode :: NetDecoder a
  encode :: NetEncoder a
