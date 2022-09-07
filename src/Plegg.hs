{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}

-- | Module    : Plegg
-- Description : High-level interface for pledge(2)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
--
-- = la .lojban.
--
-- ni'o le me'oi .module. cu vasru le mu'oi glibau. high-level .glibau.
-- korbipe be lo me'oi .Haskell. proga je vu'oi la'o gy. pledge(2) .gy.
-- je la'o gy. unveil(2) .gy. vu'o pe la'oi .OpenBSD.
--
-- = English
--
-- This module contains high-level interfaces for OpenBSD's @pledge(2)@ and
-- @unveil(2)@.
module Plegg where
import Data.Char;

-- | = la .lojban.
--
-- ni'o ro da poi me'oi .'Promise'. zo'u cumki fa lo nu pilno da lo
-- selru'e be la'oi .plegg. kei je cu se skicu ve lo mu'oi gy. man(1)
-- .gy. papri poi velski la'o gy. pledge(2) .gy.
--
-- = English
--
-- For all 'Promise' @k@, @k@ is a 'Promise' which can be passed to
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

-- | = la .lojban.
--
-- ni'o la'o zoi. @plegg k@ .zoi. na ru'e dunli la'o zoi.
-- @pledge(k, null)@ pe la'oi .C.  .i ku'i drata ki'u le nu le me'oi
-- .'Promise'. pe la'oi .@plegg@. cu se me'oi .type.
--
-- .i lo me'oi .'Promise'. poi selru'e la'oi .@plegg@. cu binxo lo
-- selci'a poi selru'e la'o gy. @pledge(2)@ .gy.
--
-- = English
--
-- @plegg k@ is roughly equivalent to C's "@pledge(k, nulll)@".
-- However, unlike the standard @pledge(2)@, @plegg@'s 'Promise's are
-- typed.
--
-- 'Promise's which are passed to @plegg@ are converted into text
-- which is passed to @pledge(2)@.
plegg :: [Promise]
      -- ^ = la .lojban.
      --
      -- zo'e poi ke'a goi ko'a zo'u cadga fa lo nu me'oi .enforce.
      -- tu'a ko'a
      --
      -- = English
      --
      -- The pledges which should be used
      -> IO ();

-- | = la .lojban.
--
-- ni'o la'oi .@univac@. korbipe la'oi .Haskell. je ko'a goi la'o gy.
-- @unveil(2)@ .gy.
--
-- .i ro da poi .orsi re je poi se vasru la'oi .@g@. zo'u la'o zoi.
-- @univac g@ .zoi. pilno lo te .orsi fa'u lo ve .orsi lo pamoi sumti
-- be ko'a be'o fa'u lo remoi sumti be ko'a je cu co'e ko'a
--
-- .i la'o zoi. @univac [("", "")]@ .zoi. dunli la'o zoi.
-- @unveil(NULL, NULL)@ .zoi. pe la'oi .C.
--
-- .i le clinoi be la'o gy. @unveil(2)@ .gy. cu xamgu velski le tcila
-- be la'o gy. @unveil(2) .gy.
--
-- = English
--
-- @univac@ is a high-level interface for @unveil(2)@.
--
-- For all @(a,b)@ in @g@, @univac g@ runs C's @unveil(a, b)@.
--
-- @univac [("", "")]@ is equivalent to C's @unveil(NULL, NULL)@.
--
-- @unveil(2)@'s manual page documents the specifics of this thing
-- pretty well.
univac :: [(FilePath, String)]
       -- ^ = la .lojban.
       --
       -- [(le me'oi .filepath. poi ke'a goi ko'a zo'u cadga fa lo nu
       -- me'oi .expose. ko'a, lo me'oi .permission.)]
       --
       -- = English
       --
       -- [(PATH WHICH SHOULD BE EXPOSED, PERMISSIONS)]
       -> IO ();
#ifdef openbsd_HOST_OS
  import Foreign.Ptr (nullPtr);
  import Foreign.C.Error (throwErrnoIfMinus1_);
  import Foreign.C.String (CString, withCString);

  foreign import capi "unistd.h pledge" pledge :: CString -> CString -> IO Int;
  foreign import capi "unistd.h unveil" unveil :: CString -> CString -> IO Int;

  plegg k = throwErrnoIfMinus1_ "pledge" $
            withCString (promises k) $ \premises ->
            withCString "" $ \execpremises ->
            pledge premises execpremises;

  -- | = la .lojban.
  --
  -- ni'o ko'a goi la'o zoi. @promises k@ .zoi. selci'a je cu liste lo
  -- me'oi .promise. pe la'oi @k@.  .i cumki fa lo nu ko'a selru'e la'o
  -- gy. @pledge(2)@ .gy.
  --
  -- = English
  --
  -- @promises k@ is a @pledge(2)@-suitable list of the promises
  -- which are specified by @k@.
  promises :: [Promise] -> String;
  promises = map toLower . concat . map show;

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
  plegg _ = return ();

  univac _ = return ();
#endif
