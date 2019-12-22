{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Base.MultiMap where

import           Codec.Serialise

import GHC.TypeLits

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage as S

data SMultiMap (name :: Symbol) s k v where
    SMMap :: Storage s => s -> MSMap' s name -> SMultiMap name s k v

instance Storage s => WithStorage s (SMultiMap name s k v) where
    storage (SMMap s _) = s
 
put :: (Serialise k, Serialise v, Storage s) =>
        Tx s 'RW -> SMultiMap name s k v -> k -> v -> IO ()
put tx (SMMap _ s) k v = S.putMu tx s (storableKey k) (storableValue v)    

delete :: (Serialise k, Serialise v) =>
         Tx s 'RW -> SMultiMap name s k v -> k -> v -> IO ()
delete tx (SMMap _ s) k v = S.deleteMu tx s (storableKey k) (storableValue v)

deleteAll :: (Serialise k, Serialise v) =>
         Tx s 'RW -> SMultiMap name s k v -> k -> IO ()
deleteAll tx (SMMap _ s) k = S.deleteAllMu tx s (storableKey k)

