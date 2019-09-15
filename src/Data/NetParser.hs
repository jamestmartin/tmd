module Data.NetParser where

import Data.ByteString

data NetParserF a where
  Isolate :: Int -> NetParserF a -> NetParserF a
  Rest    :: NetParserF ByteString
