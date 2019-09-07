module Minecraft.Protocol.State.Login where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)
import Minecraft.Protocol.DataTypes
import Minecraft.Protocol.Direction

-- Max 20 characters, empty by default.
newtype ServerID = ServerID Text
newtype PublicKey = PublicKey ByteString
-- 4 bytes by default.
newtype VerifyToken = VerifyToken ByteString
newtype Identifier = Identifier Text
newtype SharedSecret = SharedSecret ByteString

data PacketLogin :: PacketDirection -> * where
  LDisconnect :: Chat -> PacketLogin 'Clientbound
  EncryptionRequest :: ServerID -> PublicKey -> VerifyToken -> PacketLogin 'Clientbound
  LoginSuccess
    -- | The player's UUID, presented as a 36-character string.
    :: UUID
    -- | The player's username. Max 16 characters.
    -> Text
    -> PacketLogin 'Clientbound
  SetCompression
    -- | The compression threshold, in bytes. The maximum size of a pre-compressed packet.
    -- Negative values disable compression.
    :: Int
    -> PacketLogin 'Clientbound
  LoginPluginRequest
    -- | MessageID
    :: Int
    -> Identifier
    -- | Any data, depending on the channel.
    -- This is not marked with an explicit length;
    -- it is inferred from the length of the packet.
    -> ByteString
    -> PacketLogin 'Clientbound
  LoginStart
    -- | The player's username.
    :: Text
    -> PacketLogin 'Serverbound
  EncryptionResponse
    :: SharedSecret
    -> VerifyToken
    -> PacketLogin 'Serverbound
  LoginPluginResponse
    -- | MessageID
    :: Int
    -- | Whether the client understands the request.
    -> Bool
    -- | Channel-specific data, length inferred from the packet length.
    -- | No data will be sent if the client does not understand the request.
    -> ByteString
    -> PacketLogin 'Serverbound

instance Serialize (PacketLogin dir) where
  serialize (LDisconnect reason) =
    serialize (VarInt 0)
    <> serialize reason
  serialize (LoginStart name) =
    serialize (VarInt 0)
    <> serialize name

instance SPacketDirectionI dir => Deserialize (PacketLogin dir) where
  deserialize = a \case
    {-SClientbound -> do
      VarInt 0 <- deserialize
      LDisconnect <$> deserialize-}
    SServerbound -> do
      VarInt 0 <- deserialize
      LoginStart <$> deserialize
    where a :: SPacketDirectionI dir => (SPacketDirection dir -> f (g dir)) -> f (g dir)
          a f = f sPacketDirection
