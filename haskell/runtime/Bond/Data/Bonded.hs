-- | Copyright: 2015 (C) Copyright holder
--
-- Bond structure. 
-- (Add documentation and examples from site)
{-# Language GADTs, FlexibleContexts, ScopedTypeVariables #-}
module Bond.Data.Bonded (
        Bonded(..),
        mkS,
        withBonded,
        deserialize
    ) where

import Bond.Binary.Class
import Bond.Protocol.Class

import           Data.Binary.Get (runGet)
import           Data.ByteString.Lazy (ByteString)

-- type Decoder a = Lazy.ByteString -> Either (Lazy.ByteString, Int64, String) (Lazy.ByteString, Int64, a)

-- | A wrapper for Bonded type see (XXX: link to documentation)
data Bonded a where
   V :: a -> Bonded a                         -- ^ Value
   S :: (IsProtocol p)
     => BondGet p a -> ByteString -> Bonded a -- ^ Serialized value for known protocol
                                              -- XXX: possibly that we need more compilcated
                                              --      stuff here like Proxy p or other, but
                                              --      we don't need that yet, or really
                                              --      we can extract it from BondGet :)
                                             
instance Show a => Show (Bonded a) where
    show (S _ b) = "S _" ++ show b            -- ^ XXX: write Show for BondGet? 
                                              --        or deserialize?
    show (V   v) = show v

instance Eq a => Eq (Bonded a) where
    ba == bb = withBonded ba (\a ->
                 withBonded bb (\b -> a == b))

-- XXX: is bonded an instance of functor ?
-- XXX: is bonded an instance of traversable ?
-- XXX: is bonded an instance of foldable?

-- | Create serialized representation.
mkS :: forall proxy p a . (IsProtocol p, BondBinary p a) => proxy p -> ByteString -> Bonded a
mkS p bs = S (mkGet p) bs
    where mkGet :: proxy p -> BondGet p a 
          mkGet _ = bondGet

-- | Perform an action with current bondable, deserialized value is
-- not shared between computations.
--
-- XXX: do we need safe versions?
withBonded :: Bonded a -> (a -> b) -> b
withBonded (V a) f = f a
withBonded (S (BondGet g) s) f = f (runGet g s)


-- | Deserialize message using protocol that message was transmitted (serialized),
-- or use value if it was not serialized.
deserialize :: Bonded a -> a
deserialize v = withBonded v id 

-- XXX: implement serialize

{-
unpackBonded :: (BondBinary CompactBinaryProto a,
                 BondBinary CompactBinaryV1Proto a,
                 BondBinary FastBinaryProto a,
                 BondBinary SimpleBinaryProto a,
                 BondBinary SimpleBinaryV1Proto a
                ) => Bonded a -> Either String a
unpackBonded (BondedObject v) = Right v
unpackBonded (BondedStream s proto ver)
    = let decoder = getDecoder proto ver
       in case decoder s of
            Right (rest, _, msg) | Lazy.null rest -> Right msg
            Right (_, _, _) -> Left "Not all input consumed"
            Left (_, _, msg) -> Left msg

makeBonded :: a -> Bonded a
makeBonded = BondedObject

getDecoder :: (BondBinary CompactBinaryProto a,
               BondBinary CompactBinaryV1Proto a,
               BondBinary FastBinaryProto a,
               BondBinary SimpleBinaryProto a,
               BondBinary SimpleBinaryV1Proto a
              ) => ProtoSig -> Word16 -> Decoder a
getDecoder proto ver
    | proto == compactSig && ver == 1 = runCompactBinaryV1Get bondGet
    | proto == compactSig && ver == 2 = runCompactBinaryGet bondGet
    | proto == simpleSig && ver == 1 = runSimpleBinaryV1Get bondGet
    | proto == simpleSig && ver == 2 = runSimpleBinaryGet bondGet
    | proto == fastSig && ver == 1 = runFastBinaryGet bondGet
getDecoder _ _ = const $ Left (Lazy.empty, 0, "unknown protocol or version")
-}
