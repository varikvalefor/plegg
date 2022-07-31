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
import Data.Char;

#ifdef openbsd_HOST_OS
  import Foreign.Ptr (nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);

  foreign import capi "unistd.h pledge" pledge :: CString -> CString -> IO Int;
  foreign import capi "unistd.h unveil" unveil :: CString -> CString -> IO Int;

  -- | For all 'Promise' @k@, @k@ is a 'Promise' which can be passed to
  -- 'plegg'.  Each 'Promise' is best documented by the documentation of
  -- @pledge(2)@.
  data Promise = Stdio
               | RPath
               | WPath
               | CPath
               | DPath
               | TmpPath
               | INet
               | MCast
               | FAttr
               | Chown
               | Flock
               | Unix
               | Dns
               | GetPW
               | SendFD
               | RecvFD
               | Tape
               | TTY
               | Proc
               | Exec
               | Prot_Exec
               | Settime
               | Ps
               | VMInfo
               | Id
               | Pf
               | Route
               | WRoute
               | Audio
               | Video
               | Bpf
               | Unveil
               | Error
               deriving Show;

  -- | @plegg k@ is roughly equivalent to C's "@pledge(k, nulll)@".
  -- However, unlike the standard @pledge(2)@, @plegg@'s 'Promise's are
  -- typed.
  --
  -- 'Promise's which are passed to @plegg@ are converted into text
  -- which is passed to @pledge(2)@.
  plegg :: [Promise]
        -- ^ The pledges which should be used
        -> IO ();
  plegg k = throwErrnoIfMinus1_ "pledge" $
            withCString (promises k) $ \premises ->
            withCString "" $ \execpremises ->
            pledge premises execpremises;

  -- | @promises k@ is a @pledge(2)@-suitable list of the promises
  -- which are specified by @k@.
  promises :: [Promise] -> String;
  promises = map toLower . concat . map show;

  -- | @univac@ is a high-level interface for @unveil(2)@.
  --
  -- For all @(a,b)@ in @g@, @univac g@ runs C's @unveil(a, b)@.
  --
  -- @univac [("", "")]@ is equivalent to C's @unveil(NULL, NULL)@.
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
      throwErrnoIfMinus1_ "unveil" $ unveil nullPtr nullPtr
    expose (path, perms) =
      throwErrnoIfMinus1_ "unveil" $
      withCString path $ \pathC ->
      withCString perms $ \permsC ->
      unveil pathC permsC;
#else
  -- | This library is compiled on a system which does not support
  -- @pledge(2)@; as such, @plegg k@ does nothing.
  plegg :: [Promise] -> IO ();
  plegg _ = return ();

  -- | This library is compiled on a system which does not support
  -- @unveil(2)@; as such, @univac k@ does nothing.
  univac :: [(String, String)] -> IO ();
  univac _ = return ();
#endif
