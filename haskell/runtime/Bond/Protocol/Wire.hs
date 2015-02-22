-- | Copyright: 2015 (C) Copyright holder

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Bond.Protocol.Wire
    ( FieldTag(..)
    , ListHead(..)
    , MapHead(..)
    , ItemType(..)
    , Ordinal(..)
    , toWireType
    , fromWireType
    ) where

import Bond.Binary.Class

import Control.Applicative
import Data.Word

data ItemType =
      BT_STOP
    | BT_STOP_BASE
    | BT_BOOL
    | BT_UINT8
    | BT_UINT16
    | BT_UINT32
    | BT_UINT64
    | BT_FLOAT
    | BT_DOUBLE
    | BT_STRING
    | BT_STRUCT
    | BT_LIST
    | BT_SET
    | BT_MAP
    | BT_INT8
    | BT_INT16
    | BT_INT32
    | BT_INT64
    | BT_WSTRING
    deriving (Show, Enum, Eq)

toWireType :: Word8 -> ItemType
toWireType = toEnum . fromIntegral -- XXX: why not toEnum/fromEnum ?

fromWireType :: ItemType -> Word8
fromWireType = fromIntegral . fromEnum

newtype Ordinal = Ordinal Word16
    deriving (Eq, Show)

data FieldTag = FieldTag ItemType Ordinal
data ListHead = ListHead (Maybe ItemType) Int                   -- XXX: s/Head/Header?
data MapHead = MapHead (Maybe ItemType) (Maybe ItemType) Int    -- XXX: s/Head/Header?

instance BondBinary t ItemType where
    bondGet = toWireType <$> bondGet
    bondPut = bondPut . fromWireType

