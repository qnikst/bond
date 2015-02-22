{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
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
    StringHead(..),
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

import Bond.Binary.Class
import Bond.Data.Bonded
import Bond.Protocol.Types
import Bond.Protocol.Wire

import Control.Applicative
import Control.Monad (unless)
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.Word
import Data.Hashable
import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

newtype StringHead = StringHead Int

newtype Utf8 = Utf8 BS.ByteString
    deriving (Eq, Ord, Hashable)

instance BondBinary t StringHead => BondBinary t Utf8 where
    bondPut (Utf8 s) = do
        bondPut $ StringHead $ BS.length s
        BondPut $ putByteString s
    bondGet = do
        StringHead n <- bondGet
        Utf8 <$> (BondGet $ getByteString n)

newtype Utf16 = Utf16 BS.ByteString
    deriving (Eq, Ord, Hashable)

instance BondBinary t StringHead => BondBinary t Utf16 where
    bondPut (Utf16 s) = do
        bondPut $ StringHead $ BS.length s `div` 2
        BondPut $ putByteString s
    bondGet = do
        StringHead n <- bondGet
        Utf16 <$> (BondGet $ getByteString (n * 2))

newtype Blob = Blob BS.ByteString
    deriving (Show, Eq, Ord, Hashable)

instance BondBinary t ListHead => BondBinary t Blob where
    bondPut (Blob s) = do
        bondPut $ ListHead (Just BT_INT8) (BS.length s)
        BondPut $ putByteString s
    bondGet = do
        ListHead t n <- bondGet
        unless (maybe True (== BT_INT8) t) $ fail "bondGet Blob: type mismatch"
        BondGet (Blob <$> getByteString n)

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
