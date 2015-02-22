-- | Copyright: 2015 (C) Copyright holder
--
-- Registry where information for all supported protocols can live
-- 
-- XXX: only statically known registry is supported for now
{-# LANGUAGE Rank2Types #-}
module Bond.Protocol.Registry
    ( withProtocol
    ) where

import Bond.Protocol
import Bond.Protocol.Types
import Bond.Protocol.Class

import Bond.Types
import Bond.FastBinary
import Bond.CompactBinary
import {-# SOURCE #-} Bond.SimpleBinary

import           Data.Proxy
import qualified Data.Map as Map
import           Data.Maybe

registry :: Map ProtoSig (Map ProtoVer SomeProtocol)
registry = Map.fromList
    [ (fastSig,    Map.fromList [ (0, SomeProtocol (Proxy :: Proxy FastBinaryProto))])
    , (compactSig, Map.fromList [ (0, SomeProtocol (Proxy :: Proxy CompactBinaryProto))
                                , (1, SomeProtocol (Proxy :: Proxy CompactBinaryV1Proto))]
                                )
    , (simpleSig,  Map.fromList [ (0, SomeProtocol (Proxy :: Proxy SimpleBinaryProto))
                                , (1, SomeProtocol (Proxy :: Proxy SimpleBinaryV1Proto))]
                                )
    ]

withProtocol :: ProtoSig
             -> ProtoVer
             -> (forall p . IsProtocol p => Proxy p -> b)
             -> b
withProtocol sig version f =
    (\(SomeProtocol p) -> f p)
    (fromJust (Map.lookup version =<< sig `Map.lookup` registry))
