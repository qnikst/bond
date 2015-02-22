{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Bond.Wire (
    FieldTag(..),
    ItemType(..),
    ListHead(..),
    MapHead(..),
    Ordinal(..),
    StringHead(..),
    WireType(..),
    toWireType,
    fromWireType
  ) where

import Bond.Protocol.Wire
import Bond.Types

import Data.Proxy


class WireType a where
    getWireType :: Proxy a -> ItemType

instance WireType Bool where getWireType _ = BT_BOOL
instance WireType Double where getWireType _ = BT_DOUBLE
instance WireType Float where getWireType _ = BT_FLOAT
instance WireType Int8 where getWireType _ = BT_INT8
instance WireType Int16 where getWireType _ = BT_INT16
instance WireType Int32 where getWireType _ = BT_INT32
instance WireType Int64 where getWireType _ = BT_INT64
instance WireType Word8 where getWireType _ = BT_UINT8
instance WireType Word16 where getWireType _ = BT_UINT16
instance WireType Word32 where getWireType _ = BT_UINT32
instance WireType Word64 where getWireType _ = BT_UINT64
instance WireType (Maybe a) where getWireType _ = BT_LIST
instance WireType [a] where getWireType _ = BT_LIST
instance WireType Blob where getWireType _ = BT_LIST
instance WireType Utf8 where getWireType _ = BT_STRING
instance WireType Utf16 where getWireType _ = BT_WSTRING
instance WireType (Map a b) where getWireType _ = BT_MAP
instance WireType (HashSet a) where getWireType _ = BT_SET
instance WireType (Vector a) where getWireType _ = BT_LIST
instance WireType (Bonded a) where getWireType _ = BT_STRUCT
