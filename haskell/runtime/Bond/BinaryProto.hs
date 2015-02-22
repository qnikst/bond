{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, MultiWayIf, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Bond.BinaryProto (
    BondBinary(..),
    BondBinaryProto(..),
    BondBinaryStruct(..),
    BondGet(..),
    BondPutM(..),
    BondPut,
    VarInt(..),
    fromWireType,
    toWireType,
    skipBinaryValue
  ) where

import Bond.Binary.Class
import Bond.Protocol.Class
import Bond.Data.Bonded

import Bond.Default
import Bond.Schema
import Bond.Types
import Bond.Wire
import Control.Applicative
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Hashable
import Data.Proxy
-- import qualified Data.ByteString as BS
import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Vector as V

newtype VarInt = VarInt { fromVarInt :: Int }

instance BondBinary t VarInt where
    bondGet = VarInt <$> step 0
        where
        step :: Int -> BondGet t Int
        step n | n > 4 = fail "VarInt: sequence too long"
        step n = do
            b <- fromIntegral <$> BondGet getWord8
            rest <- if b `testBit` 7 then step (n + 1)  else return (0 :: Int)
            return $ (b `clearBit` 7) .|. (rest `shiftL` 7)
    bondPut (VarInt i) | i < 0 = fail "VarInt with negative value"
    bondPut (VarInt i) | i < 128 = BondPut $ putWord8 $ fromIntegral i
    bondPut (VarInt i) = do
        let iLow = fromIntegral $ i .&. 0x7F
        BondPut $ putWord8 $ iLow `setBit` 7
        bondPut $ VarInt (i `shiftR` 7)

class BondBinary t a => BondBinaryStruct t a where
    bondGetBase :: BondGet t a
    bondPutBase :: a -> BondPut t
    bondGetSchema :: Proxy (t, a) -> StructSchema


instance (BondBinary t ListHead, BondBinary t a, WireType a) => BondBinary t (Maybe a) where
    bondPut Nothing = bondPut $ ListHead (Just $ getWireType (Proxy :: Proxy a)) 0
    bondPut (Just v) = do
        bondPut $ ListHead (Just $ getWireType (Proxy :: Proxy a)) 1
        bondPut v
    bondGet = do
        ListHead t n <- bondGet
        unless (maybe True (== getWireType (Proxy :: Proxy a)) t) $ fail "nullable: type mismatch"
        if | n == 0 -> return Nothing
           | n == 1 -> Just <$> bondGet
           | otherwise -> fail "bondGet nullable: count isn't 0 or 1"

instance (BondBinary t ListHead, BondBinary t a, WireType a) => BondBinary t [a] where
    bondPut xs = do
        bondPut $ ListHead (Just $ getWireType (Proxy :: Proxy a)) (length xs)
        mapM_ bondPut xs
    bondGet = do
        ListHead t n <- bondGet
        unless (maybe True (== getWireType (Proxy :: Proxy a)) t) $ fail "bondGet [a]: type mismatch"
        replicateM n bondGet

instance (Hashable a, Eq a, BondBinary t ListHead, BondBinary t a, WireType a) => BondBinary t (HashSet a) where
    bondPut xs = bondPut $ H.toList xs
    bondGet = H.fromList <$> bondGet

instance (Ord a, BondBinary t a, WireType a, BondBinary t b, WireType b, BondBinary t MapHead) => BondBinary t (Map a b) where
    bondPut xs = do
        bondPut $ MapHead (Just $ getWireType (Proxy :: Proxy a)) (Just $ getWireType (Proxy :: Proxy b)) (M.size xs)
        forM_ (M.toList xs) $ \(k, v) -> do
            bondPut k
            bondPut v
    bondGet = do
        MapHead tkey tvalue n <- bondGet
        unless (maybe True (== getWireType (Proxy :: Proxy a)) tkey) $ fail "bondGet (Map a b): key type mismatch"
        unless (maybe True (== getWireType (Proxy :: Proxy b)) tvalue) $ fail "bondGet (Map a b): value type mismatch"
        fmap M.fromList $ replicateM n $ do
            k <- bondGet
            v <- bondGet
            return (k, v)

instance (BondBinary t ListHead, BondBinary t a, WireType a) => BondBinary t (Vector a) where
    bondPut xs = do
        bondPut $ ListHead (Just $ getWireType (Proxy :: Proxy a)) (V.length xs)
        V.mapM_ bondPut xs
    bondGet = do
        ListHead t n <- bondGet
        unless (maybe True (== getWireType (Proxy :: Proxy a)) t) $ fail "bondGet (Vector a): type mismatch"
        V.replicateM n bondGet

instance (BondBinaryProto t, BondBinaryStruct t a) => BondBinary t (Bonded a) where
    bondPut = putBonded
    bondGet = getBonded

class (BondBinary t Int8, BondBinary t Int16, BondBinary t Int32, BondBinary t Int64,
       BondBinary t Word8, BondBinary t Word16, BondBinary t Word32, BondBinary t Word64,
       BondBinary t FieldTag, BondBinary t ListHead, BondBinary t StringHead,
       BondBinary t MapHead, IsProtocol t) =>
        BondBinaryProto t where
    checkTypeAndGet :: (BondBinary t a, WireType a) => ItemType -> BondGet t a
    checkTypeAndGet = checkTypeAndGet'
    putField :: (BondBinary t a, WireType a, Default a) => Ordinal -> a -> a -> BondPut t
    putField = putField'
    putMaybeField :: (BondBinary t a, WireType a) => Ordinal -> Maybe a -> BondPut t
    putMaybeField = putMaybeField'
    putStructField :: (BondBinaryStruct t a, WireType a) => Ordinal -> a -> BondPut t
    putStructField = doPutField
    readFieldsWith :: BondBinaryStruct t a => (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
    readFieldsWith = readStructFieldsWith BT_STOP
    readBaseFieldsWith :: BondBinaryStruct t a => (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
    readBaseFieldsWith = readStructFieldsWith BT_STOP_BASE
    putEnumValue :: Int32 -> BondPut t
    putEnumValue = bondPut
    getEnumValue = bondGet
    getEnumValue :: BondGet t Int32
    skipValue :: ItemType -> BondGet t ()
    skipValue = skipBinaryValue
    readStruct :: BondGet t a -> BondGet t a
    readStruct = id
    putStruct :: BondPut t -> BondPut t
    putStruct = id
    putBonded :: BondBinaryStruct t a => Bonded a -> BondPut t
    putBonded v = withBonded v (bondPut)
    getBonded :: BondBinaryStruct t a => BondGet t (Bonded a)
    getBonded = V <$> bondGet
    putStructStop :: BondBinaryProto t => BondPut t
    putStructStop = bondPut BT_STOP
    putStructStopBase :: BondBinaryProto t => BondPut t
    putStructStopBase = bondPut BT_STOP_BASE

checkTypeAndGet' :: forall t a . (BondBinary t a, WireType a) => ItemType -> BondGet t a
checkTypeAndGet' t | t == getWireType (Proxy :: Proxy a) = bondGet
checkTypeAndGet' t = fail $ "invalid field type " ++ show t ++ " expected " ++ show (getWireType (Proxy :: Proxy a))

doPutField :: forall t a. (BondBinary t FieldTag, BondBinary t a, WireType a) => Ordinal -> a -> BondPut t
doPutField n f = do
    bondPut $ FieldTag (getWireType (Proxy :: Proxy a)) n
    bondPut f

putField' :: (BondBinary t FieldTag, BondBinary t a, WireType a, Default a) => Ordinal -> a -> a -> BondPut t
putField' n defValue f = unless (equalToDefault f defValue) (doPutField n f)

putMaybeField' :: (BondBinary t FieldTag, BondBinary t a, WireType a) => Ordinal -> Maybe a -> BondPut t
putMaybeField' _ Nothing = return ()
putMaybeField' n (Just f) = doPutField n f

readStructFieldsWith :: (BondBinary t FieldTag) => ItemType -> (a -> ItemType -> Ordinal -> BondGet t a) -> a -> BondGet t a
readStructFieldsWith stop updateFunc = loop
    where
    loop v = do
        FieldTag t n <- bondGet
        if t == stop
            then return v
            else do
                    v' <- updateFunc v t n
                    loop v'


skipBinaryValue :: (BondBinaryProto t, BondBinary t FieldTag, BondBinary t ListHead) => ItemType -> BondGet t ()
skipBinaryValue BT_STOP = fail "internal error: skipValue BT_STOP"
skipBinaryValue BT_STOP_BASE = fail "internal error: skipValue BT_STOP_BASE"
skipBinaryValue BT_BOOL = BondGet $ skip 1
skipBinaryValue BT_UINT8 = BondGet $ skip 1
skipBinaryValue BT_UINT16 = BondGet $ skip 2
skipBinaryValue BT_UINT32 = BondGet $ skip 4
skipBinaryValue BT_UINT64 = BondGet $ skip 8
skipBinaryValue BT_FLOAT = BondGet $ skip 4
skipBinaryValue BT_DOUBLE = BondGet $ skip 8
skipBinaryValue BT_INT8 = BondGet $ skip 1
skipBinaryValue BT_INT16 = BondGet $ skip 2
skipBinaryValue BT_INT32 = BondGet $ skip 4
skipBinaryValue BT_INT64 = BondGet $ skip 8
skipBinaryValue BT_STRING = do
    VarInt n <- bondGet
    BondGet $ skip n
skipBinaryValue BT_WSTRING = do
    VarInt n <- bondGet
    BondGet $ skip (n * 2)
skipBinaryValue BT_LIST = do
    ListHead (Just t) n <- bondGet
    replicateM_ n (skipValue t)
skipBinaryValue BT_SET = skipValue BT_LIST
skipBinaryValue BT_MAP = do
    MapHead (Just tkey) (Just tvalue) n <- bondGet
    replicateM_ n $ do
        skipValue tkey
        skipValue tvalue
skipBinaryValue BT_STRUCT = loop
    where
    loop = do
        FieldTag t _ <- bondGet
        case t of
            BT_STOP -> return ()
            BT_STOP_BASE -> loop
            _ -> do
                    skipValue t
                    loop
