{-# LANGUAGE ImpredicativeTypes #-}

import Control.Monad ( when )
import System.Exit ( exitFailure )
import System.Loader.Package

main :: IO ()
main = do
    initLinker
    Nothing <- loadPackageFunction "base" "GHC.Base" "no_function"
                       :: IO (Maybe Int)
    Just loadedId <- loadPackageFunction "base" "GHC.Base" "id"
                       :: IO (Maybe (forall a . a -> a))
    Control.Monad.when (loadedId "ok" /= "ok") exitFailure
