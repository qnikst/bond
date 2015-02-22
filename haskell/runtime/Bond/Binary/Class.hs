-- | Copyright: 2015 (C) Coipyright holder
-- 
-- Internal classes
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts #-}
module Bond.Binary.Class 
    ( BondGet(..)
    , BondPutM(..)
    , BondPut
    , BondBinary(..)
    ) where

import Control.Applicative

import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.Int

import Control.Monad.ST (runST, ST)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)

-- | Wrapper for parameterized Get            -- XXX: move to Bond.Binary.Get ?
newtype BondGet t a = BondGet (Get a)
    deriving (Functor, Applicative, Monad)

-- | Wrapper for parameterized Put            -- XXX: move to Bond.Binary.Put ?
newtype BondPutM t a = BondPut (PutM a)
    deriving (Functor, Applicative, Monad)

-- | Helper
type BondPut t = BondPutM t ()

-- | Parameterized Binary Wrapper
class BondBinary t a where
    bondGet :: BondGet t a
    bondPut :: a -> BondPut t

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance BondBinary t Bool where
    bondGet = do
        v <- BondGet getWord8
        return (v /= 0)
    bondPut v = if v then BondPut (putWord8 1) else BondPut (putWord8 0)

instance BondBinary t Word8 where
    bondGet = BondGet getWord8
    bondPut = BondPut . putWord8

instance BondBinary t Int8 where
    bondGet = BondGet (fromIntegral <$> getWord8)
    bondPut = BondPut . putWord8 . fromIntegral

instance BondBinary t Double where
    bondGet = BondGet (wordToDouble <$> getWord64le)
    bondPut = BondPut . putWord64le . doubleToWord

instance BondBinary t Float where
    bondGet = BondGet (wordToFloat <$> getWord32le)
    bondPut = BondPut . putWord32le . floatToWord

{-# INLINE wordToFloat #-}
wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE floatToWord #-}
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

{-# INLINE wordToDouble #-}
wordToDouble :: Word64 -> Double
wordToDouble x = runST (cast x)

{-# INLINE doubleToWord #-}
doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) =>
        a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
