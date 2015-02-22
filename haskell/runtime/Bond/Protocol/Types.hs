-- | Copyright: 2015 (C) Copyright holder

module Bond.Protocol.Types
    ( ProtoSig(..)
    , ProtoVer
    ) where

import Data.Word

-- | Protocol signature.
-- (see upstream docs)
newtype ProtoSig = ProtoSig Word16
    deriving (Eq, Show, Read)

-- | Protocol version
type ProtoVer = Word16   -- XXX: make this a newtype?
