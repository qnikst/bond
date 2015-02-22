-- | Copyright: 2015 (C) Coipyright holder
-- 
-- Internal classes
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Bond.Binary.Class 
    ( BondGet(..)
    , BondPutM(..)
    , BondPut
    , BondBinary(..)
    ) where

import Control.Applicative

import Data.Binary.Get
import Data.Binary.Put

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
