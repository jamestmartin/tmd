module Minecraft.Protocol.State.Status where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize, get, put)
import Data.Serialize.Get (getWord64be)
import Data.Serialize.Put (putWord64be, putByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Vector (Vector)
import Data.Word (Word16, Word64)
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction (PacketDirection (Clientbound, Serverbound))
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
  put sr = do
    put $ VarInt $ fromIntegral $ BS.length bytes
    putByteString bytes
    where bytes = BSL.toStrict $ A.encode sr

  get = error "not yet implemented: serialize statusresponse"

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
  
instance Serialize (PacketStatus 'Clientbound) where
  get = do
    VarInt pid <- get
    case pid of
      0 -> Response <$> get
      1 -> Pong <$> getWord64be
      n -> fail $ "Unknown packet ID for clientbound status: " ++ show n

  put pkt@(Response sr) = do
    put $ psId pkt
    put sr
  put pkt@(Pong payload) = do
    put $ psId pkt
    putWord64be payload

instance Serialize (PacketStatus 'Serverbound) where
  get = do
    VarInt pid <- get
    case pid of
      0 -> return Request
      1 -> Ping <$> getWord64be
      n -> fail $ "Unknown packet ID for serverbound status: " ++ show n

  put pkt@Request = put $ psId pkt
  put pkt@(Ping payload) = do
    put $ psId pkt
    putWord64be payload
