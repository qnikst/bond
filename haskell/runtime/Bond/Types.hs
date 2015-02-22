{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bond.Types (
    Blob(..),
    Bool,
    Bonded(..),
    Double,
    EncodedString(..),
    Float,
    H.HashSet,
    Int,
    Int16,
    Int32,
    Int64,
    Int8,
    Maybe,
    M.Map,
    Utf16(..),
    Utf8(..),
    V.Vector,
    Word16,
    Word32,
    Word64,
    Word8,
    ProtoSig(..),
    compactSig,
    fastSig,
    simpleSig
  ) where

import Bond.Data.Bonded
import Bond.Protocol.Types

import Data.Int
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

newtype Utf8 = Utf8 BS.ByteString
    deriving (Eq, Ord, Hashable)

newtype Utf16 = Utf16 BS.ByteString
    deriving (Eq, Ord, Hashable)

newtype Blob = Blob BS.ByteString
    deriving (Show, Eq, Ord, Hashable)

class EncodedString a where
    fromString :: String -> a

instance EncodedString Utf8 where fromString = Utf8 . T.encodeUtf8 . T.pack
instance EncodedString Utf16 where fromString = Utf16 . T.encodeUtf16LE . T.pack

instance Show Utf8 where show (Utf8 s) = show $ T.unpack $ T.decodeUtf8 s
instance Show Utf16 where show (Utf16 s) = show $ T.unpack $ T.decodeUtf16LE s

compactSig, simpleSig, fastSig :: ProtoSig
compactSig = ProtoSig 0x4342
simpleSig = ProtoSig 0x5350
fastSig = ProtoSig 0x4D46
