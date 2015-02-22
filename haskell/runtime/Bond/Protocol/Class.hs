-- | Copyright: 2015 (C) Copyright holder
--
-- Module for various classes that can be used for protocol description.
module Bond.Protocol.Class
    ( ProtocolSignature(..)
    , ProtocolVersion(..)
    , IsProtocol
    , sameProtocol
    ) where

import Bond.Protocol.Types

-- XXX: implement signletons for protocol signature, as we
-- may want statically known sigs.

class ProtocolSignature p where
    protocolSignature :: proxy p -> ProtoSig

class ProtocolVersion p where
    protocolVersion :: proxy p -> ProtoVer

class (ProtocolSignature p, ProtocolVersion p) => IsProtocol p

-- | Check (at runtime) are protocols equiavent or not.
sameProtocol :: (IsProtocol p1, IsProtocol p2) => proxy p1 -> proxy p2 -> Bool -- XXX: move to another module?
sameProtocol p1 p2 =  protocolSignature p1 == protocolSignature p2
                   && protocolVersion p1   == protocolVersion   p2
