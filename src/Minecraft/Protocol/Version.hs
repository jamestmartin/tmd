module Minecraft.Protocol.Version where

import Control.Arrow ((<<<), (>>>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.Serialize (Serialize, get, put)
import Data.Serialize.Put (putWord8)
import Data.Text (Text)
import Minecraft.Protocol.DataTypes

data ProtocolVersion = PV498 deriving Show
newtype AmbProtocolVersion = AmbProtocolVersion { fromAmbProtocolVersion :: Maybe ProtocolVersion }

versionName :: ProtocolVersion -> Text
versionName PV498 = "1.14.4"

safeToEnum :: Int -> Maybe (Maybe ProtocolVersion)
safeToEnum 0   = Just Nothing
safeToEnum 498 = Just $ Just PV498
safeToEnum  _  = Nothing

instance Enum (Maybe ProtocolVersion) where
  toEnum = fromJust . safeToEnum

  fromEnum Nothing      = 0
  fromEnum (Just PV498) = 498

instance Enum ProtocolVersion where
  toEnum   = toEnum                                     >>> fromJust
  fromEnum = (fromEnum :: Maybe ProtocolVersion -> Int) <<< Just

instance Serialize AmbProtocolVersion where
  get = AmbProtocolVersion <$> do
    VarInt verNum <- get
    maybe (fail $ "Unknown protocol version: " ++ show verNum) return $ safeToEnum verNum
  put = fromAmbProtocolVersion >>> fromEnum >>> fromIntegral >>> putWord8

instance Serialize ProtocolVersion where
  get = get >>= (fromAmbProtocolVersion >>> maybe (fail "Must specify protocol version.") return)
  put = put <<<      AmbProtocolVersion <<< Just

instance ToJSON ProtocolVersion where
  toJSON pv = AT.Object $ HM.fromList
    [ ("name",     AT.String $ versionName pv)
    , ("protocol", AT.Number $ fromIntegral $ fromEnum pv)
    ]

instance FromJSON ProtocolVersion where
  parseJSON = AT.withObject "Protocol Version" \obj -> do
    let Just (AT.Number verNum) = HM.lookup "protocol" obj
    -- Outer just: whether the integer fits in our data type
    -- Middle just: whether 'protocol' is present in this object
    -- Inner just: whether the protocol version was specified or ambiguous
    let Just (Just (Just ver)) = safeToEnum <$> toBoundedInteger verNum
    return ver
