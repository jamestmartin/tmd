{-# LANGUAGE UndecidableInstances #-}
module Minecraft.Protocol.ByteReader where

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError, ExceptT, throwError, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.State (MonadState, StateT, evalStateT, MonadIO, liftIO)
import qualified Control.Monad.State as St
import Data.Bits ((.|.), testBit, clearBit, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word (Word8)
import Minecraft.Protocol.DataTypes (VarInt (VarInt))
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendMany)

class (MonadError String m, MonadReader Socket m) => ByteReader m where
  nextByte :: m Word8

instance (MonadReader Socket m, MonadState ByteString m, MonadIO m, MonadError String m) => ByteReader m where
  nextByte = BS.uncons <$> St.get >>= \case
    Just (byte, bytes) -> St.put bytes >> return byte
    Nothing -> do
      conn <- ask
      fresh <- liftIO $ recv conn 4096
      -- A null return value means the connection was closed.
      if BS.null fresh
        then throwError "EOF"
        else do St.put fresh ; nextByte

type ByteReaderStack a = ExceptT String (StateT ByteString (ReaderT Socket IO)) a

nextVarInt :: ByteReader r => r Int
nextVarInt = do
  h <- nextByte
  let value = fromIntegral $ clearBit h 7 :: Int
  if testBit h 7
     then do r <- nextVarInt
             return $ value .|. shiftL r 7
     else return value

readBytes :: Socket -> ByteReaderStack a -> IO (Either String a)
readBytes conn = flip runReaderT conn . flip evalStateT BS.empty . runExceptT

nextPacket :: (ByteReader r, Serialize a, MonadIO r) => r a
nextPacket = do
  len <- nextVarInt
  packet <- runGet (isolate len get) <$> BS.pack <$> replicateM len nextByte
  case packet of
    Left err -> throwError err
    Right x -> return x

writePacket :: (MonadIO m, MonadReader Socket m) => Put -> m ()
writePacket packet = do
  conn <- ask
  liftIO $ sendMany conn [runPut $ put $ VarInt $ BS.length bytes, bytes]
  where bytes = runPut packet
