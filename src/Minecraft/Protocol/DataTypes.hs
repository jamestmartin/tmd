module Minecraft.Protocol.DataTypes where

import Data.Aeson (ToJSON, toJSON, encode)
import qualified Data.Aeson.Types as AT
import Data.Attoparsec.Zepto
import Data.Bits (Bits, (.|.), clearBit, setBit, testBit, shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word8, Word16, Word32, Word64)
import Prelude hiding (take, takeWhile)

newtype VarInt = VarInt Int deriving Show

class Serialize s where
  serialize :: s -> Builder

class Deserialize s where
  deserialize :: Parser s

anyWord8 :: Parser Word8
anyWord8 = BS.head <$> take 1

buildWord16 :: Word8 -> Word8 -> Word16
buildWord16 low' high' = low .|. shiftL high 8
  where low = fromIntegral low'
        high = fromIntegral high'

anyWord16BE :: Parser Word16
anyWord16BE = do
  high <- anyWord8
  low  <- anyWord8
  return $ buildWord16 low high

buildWord32 :: Word16 -> Word16 -> Word32
buildWord32 low' high' = low .|. shiftL high 16
  where low = fromIntegral low'
        high = fromIntegral high'

buildWordBE :: (Integral b, Bits b) => BS.ByteString -> b
buildWordBE ws = case BS.foldl' (\(i, high) low -> (i + 1, fromIntegral low .|. shiftL high i)) (0, 0) ws of
  (_, x) -> x
  where imap f xs = imap' xs 0
          where imap' [] _ = []
                imap' (x:xs) i = f i x : imap' xs (i + 1)

anyWord64be :: Parser Word64
anyWord64be = buildWordBE <$> take 8

instance Serialize VarInt where
  serialize (VarInt x)
    | x' /= 0 = word8 (setBit word 7) <> serialize (VarInt x')
    | otherwise = word8 word
    where x'   = shiftR x 7
          word = clearBit (fromIntegral x :: Word8) 7

instance Deserialize VarInt where
  deserialize = VarInt <$> do
    bytes <- BS.map (`clearBit` 7) <$> takeWhile (`testBit` 7)
    last <- fromIntegral <$> anyWord8
    return $ BS.foldr' (\x acc -> fromIntegral x .|. shiftL acc 7) last bytes

instance Serialize Text where
  serialize str =
    serialize (VarInt $ BS.length bytes)
    <> byteString bytes
    where bytes = encodeUtf8 str

instance Deserialize Text where
  deserialize = do
    VarInt len <- deserialize
    decodeUtf8 <$> take len

data Chat = ChatText Text

instance ToJSON Chat where
  toJSON (ChatText text) = AT.Object $ HM.fromList
    [ ("text", AT.String text)
    ]

instance Serialize Chat where
  serialize chat =
    serialize (VarInt $ fromIntegral $ BS.length bytes)
    <> byteString bytes
    where bytes = BSL.toStrict $ encode chat
