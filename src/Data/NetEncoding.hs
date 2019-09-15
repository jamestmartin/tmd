module Data.NetEncoding where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

data Nat = Z | S Nat

data Prod a b = Prod a b

type family (x :: Nat) :+ (y :: Nat) :: (sum :: Nat) where
  Z   :+ m = m
  S n :+ m = S (n :+ m)

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family (xs :: [a]) :++ (ys :: [a]) :: [a] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

type family Cart (xs :: [a]) (ys :: [b]) :: [(a, b)] where
  Cart '[]       _  = '[]
  Cart (x ': xs) ys = Map (Prod x) ys :++ Cart xs ys

type family Uncurry (f :: a -> b -> c) (x :: Product a b) :: c where
  Uncurry f ('Prod x y) = f x y

type EverySequence xs ys = Map (Uncurry (:+))

type family SeqCart (xs :: [Nat]) (ys :: [Nat]) :: (rs :: [Nat]) where
  SeqCart '[]     _  = '[]
  SeqCart '(x:xs) ys = Map (x :+) ys :++ SeqCart xs ys

data NatDecoder :: * -> * where
  -- Take :: SNat n -> NatDecoder [n] (Vec n Word8)
  Take :: Int -> NatDecoder ByteString
  -- FMap :: (a -> b) -> NatDecoder ls a -> NatDecoder ls b
  FMap :: (a -> b) -> NatDecoder a -> NatDecoder b
  -- Pure :: a -> NatDecoder [Z] a
  Pure :: a -> NatDecoder a
  -- Sequence :: NatDecoder lens (a -> b) -> NatDecoder lens' a -> NatDecoder (SeqCart lens lens') b
  Sequence :: NatDecoder (a -> b) -> NatDecoder a -> NatDecoder b
  -- Bind      :: NatDecoder lens a -> (a -> NatDecoder lens' b) -> NatDecoder (SeqCart lens lens') b
  Bind :: NatDecoder a -> (a -> NatDecoder b) -> NatDecoder b
  -- Empty     :: NatDecoder [] a
  Empty :: NatDecoder a
  -- Alternate :: NatDecoder lens a -> NatDecoder lens' a -> NatDecoder (Append lens lens')
  Alternate :: NatDecoder a -> NatDecoder a -> NatDecoder a

quantumBogoParse :: (ByteString -> ByteString -> a) -> ByteString -> [a]
quantumBogoParse f bs = map (uncurry f . flip BS.splitAt bs) [0 .. length bs]

runNatDecoder :: ByteString -> NatDecoder a -> [a]
runNatDecoder bs (Take m)
  | BS.length n == m = [bs]
  | otherwise = []
runNatDecoder bs (FMap f x) = map f $ runNatDecoder bs x
runNatDecoder bs (Pure x)
  | BS.length bs == 0 = [x]
  | otherwise = []
runNatDecoder bs (Sequence f x) 

data Nat = Z
         | S !Nat

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data Vec :: Nat -> * -> * where
  VNil  :: Vec 'Z a
  (:::) :: a -> Vec len a -> Vec ('S len) a

type BS n = Vec n Word8

data NatDecoderF :: Nat -> * -> * where
  Sequence :: NatDecoderF n a -> (a -> NatDecoderF m b) -> NatDecoderF (n :+ m) b
  Alternate :: NatDecoderF 

data NetDecoderF a
  = Isolate (Int, NetDecoderF a)
  | Retrieve (ByteString -> a)
  | Join (NetDecoderF b, b -> NetDecoderF a)

data NetDecoderF a
  = Isolate Int (NetDecoderF a)
  | Retrieve (ByteString -> a)

data NetEncoderF a = NetEncoderF
  { coIsolate :: (Int, NetEncoderF a)
  , coRetrieve :: a -> ByteString
  }

data NetDecoder :: * -> * where
  Isolate  :: (Int, NetDecoderF a) -> NetDecoderF a
  Retrieve :: (ByteString -> a) -> NetDecoderF a
  Join     :: (NetDecoderF b, (b -> NetDecoderF a)) -> NetDecoderF a
  Return   :: a -> NetDecoderF a

data NetEncoder :: * -> * where
  CoIsolate  :: NetEncoderF a -> (Int, NetEncoderF a)
  CoRetrieve :: NetEncoderF a -> (a -> ByteString)
  CoJoin     :: NetEncoderF a -> (NetEncoderF a -> b) -> NetEncoderF b
  CoReturn   :: NetEncoderF a -> a

data FreeFunctor :: (* -> *) -> * -> * where
  Fmap :: (a -> b) -> f a -> Lan f b

instance Functor (Lan f) where
  fmap f (Fmap g x) = Fmap (f . g) x

data FreeContravariant :: (* -> 

NetDecoderF ByteString & ((Int, NetDecoderF a) -> NetDecoderF a)
NetEncoderF (-ByteString) + (
