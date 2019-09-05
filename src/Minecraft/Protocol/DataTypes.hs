module Minecraft.Protocol.DataTypes where

import Data.Aeson (ToJSON, toJSON)
import qualified Data.Aeson.Types as AT
import Data.Bits ((.|.), clearBit, setBit, testBit, shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Serialize, put, get)
import Data.Serialize.Get (getBytes,      getWord8)
import Data.Serialize.Put (putByteString, putWord8)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8)

newtype VarInt = VarInt Int deriving Show
-- Prefixed by bytes instead of length
newtype BytePrefixedText = BytePrefixedText Text

instance Serialize VarInt where
  put (VarInt x)
    | x' /= 0 = do putWord8 (setBit word 7) ; put $ VarInt x'
    | otherwise = putWord8 word
    where x'   = shiftR x 7
          word = clearBit (fromIntegral x :: Word8) 7
  get = VarInt <$> do
    word <- getWord8
    let value = (fromIntegral $ clearBit word 7) :: Int
    if testBit word 7
      then do VarInt rest <- get
              return $ value .|. shiftL rest 7
      else return value

instance Serialize BytePrefixedText where
  put (BytePrefixedText str) = do
    put $ VarInt $ BS.length bytes
    putByteString bytes
    where bytes = encodeUtf8 str
  get = BytePrefixedText <$> decodeUtf8 <$> do
    VarInt len <- get
    getBytes len

data Chat = ChatText Text

instance ToJSON Chat where
  toJSON (ChatText text) = AT.Object $ HM.fromList
    [ ("text", AT.String text)
    ]
