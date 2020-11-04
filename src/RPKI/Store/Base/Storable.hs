{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module RPKI.Store.Base.Storable where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Control.DeepSeq
import Codec.Serialise
import GHC.Generics

newtype Storable = Storable { unStorable :: BS.ByteString }
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SValue = SValue { unSValue :: Storable }
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SKey = SKey { unSKey :: Storable }
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

-- Strictness here is important
data StorableUnit a e = SObject {-# UNPACK #-} (StorableObject a) | SError e

data StorableObject a = StorableObject a SValue
    deriving (Show, Eq, Generic)

toStorableObject :: Serialise a => a -> StorableObject a 
toStorableObject a = StorableObject a $ force (storableValue a)

toStorable :: Serialise v => v -> Storable
toStorable = Storable . LBS.toStrict . serialise

storableValue :: Serialise v => v -> SValue
storableValue = SValue . toStorable

storableKey :: Serialise v => v -> SKey
storableKey = SKey . toStorable

-- TODO Consider having type-safe storage errors
-- 
-- fromStorable :: Serialise t => Storable -> Either StorageError t
-- fromStorable (Storable b) = first (DeserialisationError . fmtEx) $ 
--     deserialiseOrFail $ LBS.fromStrict b

-- fromSValue :: Serialise t => SValue -> Either StorageError t
-- fromSValue (SValue b) = fromStorable b

fromStorable :: Serialise t => Storable -> t
fromStorable (Storable b) = deserialise $ LBS.fromStrict b

fromSValue :: Serialise t => SValue -> t
fromSValue (SValue b) = fromStorable b


data SStats = SStats {
    statSize       :: Int,
    statKeyBytes   :: Int,
    statValueBytes :: Int
} deriving stock (Show, Eq, Generic)


incrementStats :: SStats -> SKey -> SValue -> SStats
incrementStats stat (SKey (Storable k)) (SValue (Storable v)) = 
    SStats { 
        statSize = statSize stat + 1, 
        statKeyBytes = statKeyBytes stat + BS.length k,
        statValueBytes = statValueBytes stat + BS.length v
    }  