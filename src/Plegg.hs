{-# LANGUAGE CApiFFI #-}
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
  import Foreign.Ptr (nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);

  foreign import capi "unistd.h pledge" pledge :: CString -> CString -> IO Int;
  foreign import capi "unistd.h unveil" unveil :: CString -> CString -> IO Int;

  -- | @plegg k@ is equivalent to C's "@pledge(k, nulll)@".
  --
  -- The available pledges are documented pretty well in @pledge(2)@'s
  -- manual page.
  plegg :: String
        -- ^ The pledges which should be used
        -> IO ();
  plegg k = throwErrnoIfMinus1_ "pledge" $
            withCString k $ \premises ->
            withCString "" $ \execpremises ->
            pledge premises execpremises;

  -- | @univac@ is a high-level interface for @unveil(2)@.
  --
  -- For all @(a,b)@ in @g@, @univac g@ runs C's @unveil(a, b)@.
  --
  -- @univac "" ""@ is equivalent to C's @unveil(NULL, NULL)@.
  --
  -- @unveil(2)@'s manual page documents the specifics of this thing
  -- pretty well.
  univac :: [(String, String)]
         -- ^ [(PATH WHICH SHOULD BE EXPOSED, PERMISSIONS)]
         -> IO ();
  univac = mapM_ expose
    where
    expose :: (String, String) -> IO ()
    expose ("", "") =
      throwErrnoIfMinus1_ "unveil" $
      unveil nullPtr nullPtr
    expose (path, perms) =
      throwErrnoIfMinus1_ "unveil" $
      withCString path $ \pathC ->
      withCString perms $ \permsC ->
      unveil pathC permsC;
#else
  -- | This library is compiled on a system which does not support
  -- @pledge(2)@; as such, @plegg k@ does nothing.
  plegg :: String -> IO ();
  plegg _ = return ();

  -- | This library is compiled on a system which does not support
  -- @unveil(2)@; as such, @univac k@ does nothing.
  univac :: [(String, String)] -> IO ();
  univac _ = return ();
#endif
