{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

-- | Module    : Plegg
-- Description : High-level interface for pledge(2)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
-- This module contains high-level interfaces for OpenBSD's @pledge(2)@ and
-- @unveil(2)@.
module Plegg where

#ifdef openbsd_HOST_OS
  import Foreign (Ptr, nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);
  import System.Directory;

  foreign import ccall "unistd.h pledge" pledge :: CString -> Ptr [CString] -> IO Int;
  foreign import ccall "unistd.h unveil" unveil :: CString -> CString -> IO Int;

  -- | @plegg k@ is equivalent to C's "@pledge(k, nulll)@".
  --
  -- The available pledges are documented pretty well in @pledge(2)@'s
  -- manual page.
  plegg :: String
        -- ^ The pledges which should be used
        -> IO ();
  plegg k = throwErrnoIfMinus1_ "pledge fails!" $
            withCString k $ \premises ->
            pledge premises nullPtr;

  -- | @univac@ is a high-level interface for @unveil(2)@.
  --
  -- For all @(a,b)@ in @g@, @univac g@ runs @unveil(a, b)@.
  univac :: [(String, String)]
         -- ^ [(PATH WHICH SHOULD BE EXPOSED, PERMISSIONS)]
         -> IO ();
  univac = mapM_ expose
    where
    expose :: (String, String) -> IO ()
    expose (path, perms) =
      throwErrnoIfMinus1_ "unveil hath fallen!" $
      withCString path $ \pathC ->
      withCString perms $ \permsC ->
      unveil pathC permsC;
#else
  -- | This library is compiled on a system which does not support
  -- @2pledge(2)@; as such, @plegg k@ does nothing.
  plegg :: String -> IO ();
  plegg _ = return ();

  -- | This library is compiled on a system which does not support
  -- @unveil(2)@; as such, @univac k@ does nothing.
  univac :: [(String, String)] -> IO ();
  univac = return ();
#endif
