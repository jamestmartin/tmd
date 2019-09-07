module Minecraft.Protocol.State.Status where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Vector (Vector)
import Data.Word (Word16, Word64)
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction
import Minecraft.Protocol.Version

data SamplePlayer = SamplePlayer
  { spName :: Text
  , spUUID :: UUID
  }

instance ToJSON SamplePlayer where
  toJSON sp = AT.Object $ HM.fromList
    [ ("name", AT.String $            spName sp)
    , ("id"  , AT.String $ U.toText $ spUUID sp)
    ]

instance FromJSON SamplePlayer where
  parseJSON json = flip (AT.withObject "Sample Player") json \obj -> do
    let (Just name) = HM.lookup "name" obj >>= fromATString
    let (Just uuid) = HM.lookup "id"   obj >>= fromATString >>= U.fromText
    return $ SamplePlayer name uuid
    where fromATString :: AT.Value -> Maybe Text
          fromATString (AT.String str) = Just str
          fromATString _               = Nothing
  

-- | https://wiki.vg/Server_List_Ping#Response
data StatusResponse = StatusResponse
  { srVersion :: ProtocolVersion
  , srPlayersMax :: Word16
  , srPlayersOnline :: Word16
  , srPlayersSample :: Vector SamplePlayer
  , srDescription :: Chat
  -- TODO: add support for favicons
  }

instance ToJSON StatusResponse where
  toJSON sr = AT.Object $ HM.fromList
    [ ("version", toJSON $ srVersion sr)
    , ("players", AT.Object $ HM.fromList
        [ ("max", AT.Number $ fromIntegral $ srPlayersMax sr)
        , ("online", AT.Number $ fromIntegral $ srPlayersOnline sr)
        , ("sample", AT.Array $ fmap toJSON $ srPlayersSample sr)
        ])
    , ("description", toJSON $ srDescription sr)
    ]

instance Serialize StatusResponse where
  serialize sr =
    serialize (VarInt $ fromIntegral $ BS.length bytes)
    <> byteString bytes
    where bytes = BSL.toStrict $ A.encode sr

data PacketStatus :: PacketDirection -> * where
  -- ## request / response
  -- | https://wiki.vg/Server_List_Ping#Request
  Request   :: PacketStatus 'Serverbound
  -- | https://wiki.vg/Server_List_Ping#Response
  Response  :: StatusResponse -> PacketStatus 'Clientbound
  -- ## ping / pong
  -- | https://wiki.vg/Server_List_Ping#Ping
  Ping
    -- | The ping "payload", chosen by the client to ensure that the server isn't cheating its 'Pong's.
    :: Word64
    -> PacketStatus 'Serverbound
  -- | https://wiki.vg/Server_List_Ping#Pong
  Pong
    -- | The same "payload" provided in 'Ping'.
    :: Word64
    -> PacketStatus 'Clientbound

psId :: PacketStatus a -> VarInt
psId Request      = VarInt 0
psId (Response _) = VarInt 0
psId (Ping _)     = VarInt 1
psId (Pong _)     = VarInt 1

instance SPacketDirectionI dir => Deserialize (PacketStatus dir) where
  deserialize = a \case
    {-SClientbound -> do
      VarInt pid <- deserialize
      case pid of
        0 -> Response <$> deserialize
        1 -> Pong <$> anyWord64be
        n -> fail $ "Unknown packet ID for clientbound status: " ++ show n-}
    SServerbound -> do
      VarInt pid <- deserialize
      case pid of
        0 -> return Request
        1 -> Ping <$> anyWord64be
        n -> fail $ "Unknown packet ID for serverbound status: " ++ show n
    where a :: SPacketDirectionI dir => (SPacketDirection dir -> f (g dir)) -> f (g dir)
          a f = f sPacketDirection

instance Serialize (PacketStatus dir) where
  serialize pkt@(Response sr) =
    serialize (psId pkt)
    <> serialize sr
  serialize pkt@(Pong payload) =
    serialize (psId pkt)
    <> word64BE payload
  serialize pkt@Request = serialize $ psId pkt
  serialize pkt@(Ping payload) =
    serialize (psId pkt)
    <> word64BE payload
