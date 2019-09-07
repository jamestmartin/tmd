{-# LANGUAGE UndecidableInstances #-}
module Minecraft.Protocol.State where

import Control.Category.Free (Cat (Id, (:.:)))
import Data.ByteString.Builder (word8)
import Data.Exists (Exists (Exists))
import Minecraft.Protocol.DataTypes (Serialize, Deserialize, serialize, deserialize, anyWord8)

-- | The packets that the client and server may send and recieve
-- are dependent on the protocol state.
-- Each protocol state uses a separate set of packet IDs.
--
-- Other sources, like wiki.vg, may refer to this as the connection state.
-- I refer to it as the protocol state to distinguish it
-- from the state of the connection as a whole,
-- which may include information about the player,
-- compression and encryption state, and so forth.
data ProtocolState
  -- | The initial state of a new connection.
  = Handshake
  -- | The client is querying the server status, but not joining the game.
  | Status
  -- | Logging in and initializing protocol state (compression and encryption).
  | Login
  -- | The standard gameplay protocol state after login.
  | Play
  -- | The connection is closed. No more packets will be sent or recieved.
  | Closed

-- | A ProtocolState available as both a type and a value.
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

-- | An implicitly-passed protocol state.
class SProtocolStateI (st :: ProtocolState) where
  -- | Retrieve the known value of the protocol state singleton.
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

-- | Explicitly specify an implicitly-passed protocol state.
rProtocolState :: SProtocolState st -> (SProtocolStateI st => t) -> t
rProtocolState SHandshake x = x
rProtocolState SStatus    x = x
rProtocolState SLogin     x = x
rProtocolState SPlay      x = x
rProtocolState SClosed    x = x

-- | The valid protocol state transitions.
data ProtocolStateTransition
  -- | A transition from this state ...
  :: ProtocolState
  -- | ... to this one.
  -> ProtocolState
  -> * where
  HandshakeStatus :: ProtocolStateTransition 'Handshake 'Status
  HandshakeLogin  :: ProtocolStateTransition 'Handshake 'Login
  LoginPlay       :: ProtocolStateTransition 'Login     'Play
  CloseConnection :: ProtocolStateTransition a          'Closed

class ProtocolStateTransitionI (to :: ProtocolState) (from :: ProtocolState) where
  protocolStateTransition :: ProtocolStateTransition to from

instance ProtocolStateTransitionI 'Handshake 'Status where
  protocolStateTransition = HandshakeStatus

instance ProtocolStateTransitionI 'Handshake 'Login where
  protocolStateTransition = HandshakeLogin

instance ProtocolStateTransitionI 'Login 'Play where
  protocolStateTransition = LoginPlay

instance ProtocolStateTransitionI a 'Closed where
  protocolStateTransition = CloseConnection

rProtocolStateTransition :: ProtocolStateTransition st st' -> (ProtocolStateTransitionI st st' => a) -> a
rProtocolStateTransition HandshakeStatus x = x
rProtocolStateTransition HandshakeLogin  x = x
rProtocolStateTransition LoginPlay       x = x
rProtocolStateTransition CloseConnection x = x

nextState :: ProtocolStateTransition st st' -> SProtocolState st'
nextState HandshakeStatus = SStatus
nextState HandshakeLogin  = SLogin
nextState LoginPlay       = SPlay
nextState CloseConnection = SClosed

instance Enum (Exists (ProtocolStateTransition 'Handshake)) where
  toEnum 0 = Exists $ CloseConnection
  toEnum 1 = Exists $ HandshakeStatus
  toEnum 2 = Exists $ HandshakeLogin
  toEnum n = Exists $ error $ "Invalid transition from handshake: " ++ show n
  
  fromEnum (Exists t) = case t of
    CloseConnection -> 0
    HandshakeStatus -> 1
    HandshakeLogin  -> 2

instance Deserialize (Exists (ProtocolStateTransition 'Handshake)) where
  deserialize = anyWord8 >>= \case
    0 -> return $ Exists CloseConnection
    1 -> return $ Exists HandshakeStatus
    2 -> return $ Exists HandshakeLogin
    n -> fail $ "Invalid next state: " ++ show n

instance Serialize (Exists (ProtocolStateTransition 'Handshake)) where
  serialize = word8 . fromIntegral . fromEnum
