module Minecraft.Protocol.State where

import Data.Serialize (Serialize, get, put)
import Data.Serialize.Get (getWord8)
import Data.Serialize.Put (putWord8)

data ProtocolState = Handshake
                   | Status
                   | Login
                   | Play
                   | Closed

data ProtocolStateTransition :: ProtocolState -> * where
  PSTStatus :: ProtocolStateTransition 'Handshake
  PSTLogin  :: ProtocolStateTransition 'Handshake
  PSTPlay   :: ProtocolStateTransition 'Login
  PSTClose  :: ProtocolStateTransition a

data SProtocolState :: ProtocolState -> * where
  SHandshake :: SProtocolState 'Handshake
  SStatus    :: SProtocolState 'Status
  SLogin     :: SProtocolState 'Login
  SPlay      :: SProtocolState 'Play
  SClosed    :: SProtocolState 'Closed

protocolState :: SProtocolState st -> ProtocolState
protocolState SHandshake = Handshake
protocolState SStatus    = Status
protocolState SLogin     = Login
protocolState SPlay      = Play
protocolState SClosed    = Closed

class SProtocolStateI (st :: ProtocolState) where
  sProtocolState :: SProtocolState st

instance SProtocolStateI 'Handshake where
  sProtocolState = SHandshake

instance SProtocolStateI 'Status where
  sProtocolState = SStatus

instance SProtocolStateI 'Login where
  sProtocolState = SLogin

instance SProtocolStateI 'Play where
  sProtocolState = SPlay

instance SProtocolStateI 'Closed where
  sProtocolState = SClosed

rProtocolState :: SProtocolState st -> (SProtocolStateI st => t) -> t
rProtocolState SHandshake x = x
rProtocolState SStatus    x = x
rProtocolState SLogin     x = x
rProtocolState SPlay      x = x
rProtocolState SClosed    x = x

instance Enum (ProtocolStateTransition 'Handshake) where
  toEnum 0 = PSTClose
  toEnum 1 = PSTStatus
  toEnum 2 = PSTLogin
  toEnum n = error $ "Invalid PST enum: " ++ show n
  
  fromEnum PSTClose  = 0
  fromEnum PSTStatus = 1
  fromEnum PSTLogin  = 2

instance Serialize (ProtocolStateTransition 'Handshake) where
  get = getWord8 >>= \case
    0 -> return PSTClose
    1 -> return PSTStatus
    2 -> return PSTLogin
    n -> fail $ "Invalid next state: " ++ show n

  put = putWord8 . fromIntegral . fromEnum
