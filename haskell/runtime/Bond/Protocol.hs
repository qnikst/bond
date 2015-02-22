-- | Copyright: 2015 (C) Copyright holder
--
-- Protocol interface
{-# LANGUAGE ExistentialQuantification #-}
module Bond.Protocol
    ( SomeProtocol(..)
    ) where

import Bond.Protocol.Class

import Data.Proxy

data SomeProtocol = forall p . IsProtocol p => SomeProtocol (Proxy p)
