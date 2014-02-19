{-# LANGUAGE CPP                        #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE UnboxedTuples              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
--
-- Copyright (C) 2004-5 Don Stewart
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
-- USA
--

-- | An interface to the GHC runtime's dynamic linker, providing runtime
-- loading and linking of Haskell packages and object files.
--
-- Call initLinker first of all.
--
module System.Loader.Package (

        initLinker      -- start it up
      , loadPackageFunction
      , loadPackage     -- load a ghc library and its cbits
      , unloadPackage   -- unload a ghc library and its cbits
      , loadPackageWith -- load a pkg using the package.conf provided
      , loadShared      -- load a .so object file
      , resolveObjs     -- and resolve symbols

      , loadRawObject   -- load a bare .o. no dep chasing, no .hi file reading
      , PackageConf

  ) where

#include "../../../config.h"

import System.Loader.Package.Env
import System.Loader.Package.ZEncoding
import System.Loader.Package.Constants  ( sysPkgSuffix, prefixUnderscore )

import Data.List                ( isSuffixOf, isPrefixOf )
import Control.Monad            ( when, liftM )
import Foreign.C.String         ( CString, withCString, peekCString )

import GHC.Ptr                  ( Ptr(..), nullPtr )
#if !MIN_VERSION_base(4,5,0)
import GHC.Exts                 ( addrToHValue# )
#else
import GHC.Exts                 ( addrToAny# )
#endif

#if DEBUG
import System.IO                ( hFlush, stdout )
#endif

-- | Call the initLinker function first, before calling any of the other
-- functions in this module - otherwise you\'ll get unresolved symbols.

-- initLinker :: IO ()
-- our initLinker transparently calls the one in GHC

loadFunction__ :: Maybe String
              -> String
              -> String
              -> IO (Maybe a)
loadFunction__ pkg m valsym
   = do let symbol = prefixUnderscore++(maybe "" (\p -> encode p++"_") pkg)
                     ++encode m++"_"++(encode valsym)++"_closure"
#if DEBUG
        putStrLn $ "Looking for <<"++symbol++">>"
#endif
        ptr@(Ptr addr) <- withCString symbol c_lookupSymbol
        if (ptr == nullPtr)
            then return Nothing
#if !MIN_VERSION_base(4,5,0)
            else case addrToHValue# addr of
#else
            else case addrToAny# addr of
#endif
                (# hval #) -> return ( Just hval )


-- | Loads a function from a package module, given the package name,
--   module name and symbol name.
loadPackageFunction :: String -- ^ Package name, including version number.
                    -> String -- ^ Module name
                    -> String -- ^ Symbol to lookup in the module
                    -> IO (Maybe a)
loadPackageFunction pkgName modName functionName =
    do loadPackage pkgName
       resolveObjs (unloadPackage pkgName)
       loadFunction__ (Just pkgName) modName functionName

--
-- | Load a GHC-compiled Haskell vanilla object file.
-- The first arg is the path to the object file
--
-- We make it idempotent to stop the nasty problem of loading the same
-- .o twice. Also the rts is a very special package that is already
-- loaded, even if we ask it to be loaded. N.B. we should insert it in
-- the list of known packages.
--
-- NB the environment stores the *full path* to an object. So if you
-- want to know if a module is already loaded, you need to supply the
-- *path* to that object, not the name.
--
-- NB -- let's try just the module name.
--
-- loadObject loads normal .o objs, and packages too. .o objs come with
-- a nice canonical Z-encoded modid. packages just have a simple name.
-- Do we want to ensure they won't clash? Probably.
--
--
--
-- the second argument to loadObject is a string to use as the unique
-- identifier for this object. For normal .o objects, it should be the
-- Z-encoded modid from the .hi file. For archives\/packages, we can
-- probably get away with the package name
--
loadObject :: FilePath -> Key -> IO ()
loadObject p ky@(Object k)  = loadObject' p ky k
loadObject p ky@(Package k) = loadObject' p ky k

loadObject' :: FilePath -> Key -> String -> IO ()
loadObject' p ky k
    | ("HSrts"++sysPkgSuffix) `isSuffixOf` p = return ()

    | otherwise
    = do alreadyLoaded <- isLoaded k
         when (not alreadyLoaded) $ do
              r <- withCString p c_loadObj
              when (not r) (panic $ "Could not load module `"++p++"'")
         addModule k (emptyMod p)   -- needs to Z-encode module name

    where emptyMod q = Module q (mkModid q) ky

panic s = ioError ( userError s )

-- | work out the mod name from a filepath
mkModid :: String -> String
mkModid = (takeWhile (/= '.')) . reverse . (takeWhile (\x -> ('/'/= x) && ('\\' /= x))) . reverse

--
-- | Load a generic .o file, good for loading C objects.
-- You should know what you're doing..
-- Returns a fairly meaningless iface value.
--
loadRawObject :: FilePath -> IO ()
loadRawObject obj = loadObject obj (Object k)
    where
        k = encode (mkModid obj)  -- Z-encoded module name

--
-- | Resolve (link) the modules loaded by the 'loadObject' function.
--
resolveObjs :: IO a -> IO ()
resolveObjs unloadLoaded
    = do r <- c_resolveObjs
         when (not r) $ unloadLoaded >> panic "resolvedObjs failed."

--
-- | from ghci\/ObjLinker.c
--
-- Load a .so type object file.
--
loadShared :: FilePath -> IO ()
loadShared str = do
#if DEBUG
    putStrLn $ " shared: " ++ str
#endif
    maybe_errmsg <- withCString str $ \dll -> c_addDLL dll
    if maybe_errmsg == nullPtr
        then return ()
        else do e <- peekCString maybe_errmsg
                panic $ "loadShared: couldn't load `"++str++"\' because "++e


--
-- | Load a -package that we might need, implicitly loading the cbits too
-- The argument is the name of package (e.g.  \"concurrent\")
--
-- How to find a package is determined by the package.conf info we store
-- in the environment. It is just a matter of looking it up.
--
-- Not printing names of dependent pkgs
--
loadPackage :: String -> IO ()
loadPackage p = do
#if DEBUG
        putStr (' ':p) >> hFlush stdout
#endif
        (libs,dlls) <- lookupPkg p
        mapM_ (\l -> loadObject l (Package (mkModid l))) libs
#if DEBUG
        putStr (' ':show libs) >> hFlush stdout
        putStr (' ':show dlls) >> hFlush stdout
#endif
        mapM_ loadShared dlls



--
-- | Unload a -package, that has already been loaded. Unload the cbits
-- too. The argument is the name of the package.
--
-- May need to check if it exists.
--
-- Note that we currently need to unload everything. grumble grumble.
--
-- We need to add the version number to the package name with 6.4 and
-- over. "yi-0.1" for example. This is a bug really.
--
unloadPackage :: String -> IO ()
unloadPackage pkg = do
    let pkg' = takeWhile (/= '-') pkg   -- in case of *-0.1
    libs <- liftM (\(a,_) -> (filter (isSublistOf pkg') ) a) (lookupPkg pkg)
    flip mapM_ libs $ \p -> withCString p $ \c_p -> do
                        r <- c_unloadObj c_p
                        when (not r) (panic "unloadObj: failed")
                        rmModule (mkModid p)      -- unrecord this module

-- | 'isSublistOf' takes two arguments and returns 'True' iff the first
-- list is a sublist of the second list. This means that the first list
-- is wholly contained within the second list.
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf [] _ = True
isSublistOf _ [] = False
isSublistOf x y@(_:ys)
    | isPrefixOf x y = True
    | otherwise      = isSublistOf x ys

type PackageConf = FilePath

--
-- | load a package using the given package.conf to help
-- TODO should report if it doesn't actually load the package, instead
-- of mapM_ doing nothing like above.
--
loadPackageWith :: String -> [PackageConf] -> IO ()
loadPackageWith p pkgconfs = do
#if DEBUG
        putStr "Loading package" >> hFlush stdout
#endif
        mapM_ addPkgConf pkgconfs
        loadPackage p
#if DEBUG
        putStrLn " done"
#endif


-- ---------------------------------------------------------------------
-- C interface
--
foreign import ccall safe "lookupSymbol"
   c_lookupSymbol :: CString -> IO (Ptr a)

foreign import ccall unsafe "loadObj"
   c_loadObj :: CString -> IO Bool

foreign import ccall unsafe "unloadObj"
   c_unloadObj :: CString -> IO Bool

foreign import ccall unsafe "resolveObjs"
   c_resolveObjs :: IO Bool

foreign import ccall unsafe "addDLL"
   c_addDLL :: CString -> IO CString

-- | Initializes the RTS linker.
foreign import ccall unsafe "initLinker"
   initLinker :: IO ()
