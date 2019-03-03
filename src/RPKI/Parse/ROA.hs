{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.ROA where

import qualified Data.ByteString as B  

import Data.Word

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import RPKI.Domain  
import RPKI.IP
import RPKI.Parse.Common 
import RPKI.Parse.SignedObject 


parseRoa :: B.ByteString -> ParseResult (SignedObject [ROA])
parseRoa bs = 
  case decodeASN1' BER bs of
    Left e     -> (Left . fmtErr . show) e
    Right asns -> mapParseErr $ runParseASN1 (parseSignedObject parseRoa') asns  
  where 
    parseRoa' = onNextContainer Sequence $ do
      asId <- getInteger (pure . fromInteger) "Wrong ASID"
      onNextContainer Sequence $ getMany $ 
        onNextContainer Sequence $ 
          getAddressFamily "Expected an address family here" >>= \case 
              Right Ipv4F -> getRoa asId ipv4
              Right Ipv6F -> getRoa asId ipv6
              Left af     -> throwParseError $ "Unsupported address family: " ++ show af

    getRoa :: Int -> (B.ByteString -> Word64 -> APrefix) -> ParseASN1 ROA
    getRoa asId mkPrefix = getNextContainerMaybe Sequence >>= \case       
      Just [Start Sequence, BitString (BitArray nzBits bs), End Sequence] -> 
        pure $ roa' bs nzBits (fromIntegral nzBits) 
      Just [Start Sequence, BitString (BitArray nzBits bs), IntVal maxLength, End Sequence] -> 
        pure $ roa' bs nzBits (fromInteger maxLength)           
      Just a  -> throwParseError $ "Unexpected ROA content: " ++ show a
      Nothing -> throwParseError $ "Unexpected ROA content"
      where 
        roa' bs nz maxL = ROA (ASN asId) (mkPrefix bs nz) maxL

    ipv4 bs nzBits = AV4 $ Ipv4P $ mkV4Prefix bs (fromIntegral nzBits)
    ipv6 bs nzBits = AV6 $ Ipv6P $ mkV6Prefix bs (fromIntegral nzBits)


          
    
      
