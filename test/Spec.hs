{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.RRDP.UpdateSpec
import RPKI.ResourcesSpec
import RPKI.AppMonadSpec
import RPKI.RepositorySpec
import RPKI.RTR.RtrSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        appMonadSpec,        
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        storeGroup,
        resourceGroup,
        repositoryGroup,
        rtrGroup
    ]  
