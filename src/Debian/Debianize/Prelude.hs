-- | Functions and instances used by but not related to cabal-debian.
-- These could conceivably be moved into more general libraries.
{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Debianize.Prelude
    ( curry3
    , DebMap
    , buildDebVersionMap
    , (!)
    , strip
    , stripWith
    , strictReadF
    , replaceFile
    , modifyFile
    , diffFile
    , removeIfExists
    , dpkgFileMap
    , debOfFile
    , cond
    , readFile'
    , readFileMaybe
    , showDeps
    , showDeps'
    , withCurrentDirectory
    , getDirectoryContents'
    , setMapMaybe
    , zipMaps
    , foldEmpty
    , maybeL
    , indent
    , maybeRead
    , read'
    , modifyM
    , intToVerbosity'
    , listElemLens
    , maybeLens
    , fromEmpty
    , fromSingleton
    , (.?=)
    , escapeDebianWildcards
    , module Distribution.Version
    , module Distribution.Package
    ) where


import Control.Applicative ((<$>))
import Control.Exception as E (bracket, catch, throw, try)
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.State (get, MonadState, StateT, put)
import Data.Char (isSpace)
import Data.List as List (dropWhileEnd, intersperse, lines, map)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>), mconcat)
import Data.Set as Set (Set, toList)
import qualified Data.Set as Set (findMin, fromList, null, size)
import Data.Text as Text (Text, unpack)
import Data.Text.IO (hGetContents)
import Debian.Control (stripWS)
import Debian.Orphans ()
import Debian.Pretty (PP(PP))
import qualified Debian.Relation as D (BinPkgName(..), Relations)
import Debian.Relation.Common ()
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Distribution.Package (PackageIdentifier(..), PackageName, mkPackageName, unPackageName)
import Distribution.Version
import Distribution.Pretty (Pretty(pretty))
import Distribution.Verbosity (intToVerbosity, Verbosity)
import GHC.IO.Exception (ExitCode(ExitFailure, ExitSuccess), IOErrorType(InappropriateType, NoSuchThing), IOException(IOError, ioe_description, ioe_type))
import Prelude hiding (map)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents, removeDirectory, removeFile, renameFile, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (hSetBinaryMode, IOMode(ReadMode), openFile, withFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Process (readProcess, readProcessWithExitCode, showCommandForUser)
import Text.PrettyPrint.HughesPJClass as PP (text)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

type DebMap = Map D.BinPkgName (Maybe DebianVersion)

-- | Query versions of installed packages
buildDebVersionMap :: IO DebMap
buildDebVersionMap =
    Map.fromList . List.map lineToKV . Prelude.lines <$> readProcess "dpkg-query" ["--showformat", "${Package}\\t${Version}\\n"] ""
    where
        lineToKV = bimap (D.BinPkgName . stripWS) (Just . parseDebianVersion' . stripWS) . span (/= '\t')

(!) :: DebMap -> D.BinPkgName -> DebianVersion
m ! k = maybe (error ("No version number for " ++ (show . pretty . PP $ k) ++ " in " ++ show (Map.map (maybe Nothing (Just . prettyDebianVersion)) m))) id (Map.findWithDefault Nothing k m)

strip :: String -> String
strip = stripWith isSpace

stripWith :: (a -> Bool) -> [a] -> [a]
stripWith f = dropWhile f . dropWhileEnd f

strictReadF :: (Text -> r) -> FilePath -> IO r
strictReadF f path = withFile path ReadMode (\h -> hGetContents h >>= (\x -> return $! f x))
-- strictRead = strictReadF id

-- | Write a file which we might still be reading from in
-- order to compute the text argument.
replaceFile :: FilePath -> String -> IO ()
replaceFile path s =
    do removeFile back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       renameFile path back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       writeFile path s
    where
      back = path ++ "~"

-- | Compute the new file contents from the old.  If f returns Nothing
-- do not write.
modifyFile :: FilePath -> (String -> IO (Maybe String)) -> IO ()
modifyFile path f =
    do removeFile back `E.catch` (\ (e :: IOException) -> when (not (isDoesNotExistError e)) (ioError e))
       try (renameFile path back) >>=
           either (\ (e :: IOException) -> if not (isDoesNotExistError e)
                                           then ioError e
                                           else f "" >>= maybe (return ()) (writeFile path))
                  (\ () -> readFile back >>= f >>= maybe (return ()) (writeFile path))
    where
      back = path ++ "~"

diffFile :: FilePath -> Text -> IO (Maybe String)
diffFile path s =
    readProcessWithExitCode cmd args (unpack s) >>= \ (code, out, _err) ->
    case code of
      ExitSuccess -> return Nothing
      ExitFailure 1 -> return (Just out)
      _ -> error (showCommandForUser cmd args {- ++ " < " ++ show s -} ++ " -> " ++ show code)
    where
      cmd = "diff"
      args = ["-ruw", path, "-"]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists x = doesFileExist x >>= (`when` (removeFile x))

removeDirectoryIfExists :: FilePath -> IO ()
removeDirectoryIfExists x = doesDirectoryExist x >>= (`when` (removeDirectory x))

removeIfExists :: FilePath -> IO ()
removeIfExists x = removeFileIfExists x >> removeDirectoryIfExists x

-- |Create a map from pathname to the names of the packages that contains that pathname using the
-- contents of the debian package info directory @/var/lib/dpkg/info@.
dpkgFileMap :: IO (Map FilePath (Set D.BinPkgName))
dpkgFileMap =
    do
      installedPackages <- Map.keys <$> buildDebVersionMap

      files <- mapM (listFiles . D.unBinPkgName) installedPackages
      return $ Map.fromList $ zip (List.map D.unBinPkgName installedPackages) (List.map (Set.fromList . List.map D.BinPkgName) $ files)
    where
        listFiles pkg = Prelude.lines <$> readProcess "dpkg-query" ["--listfiles", pkg] ""

-- |Given a path, return the name of the package that owns it.
debOfFile :: FilePath -> ReaderT (Map FilePath (Set D.BinPkgName)) IO (Maybe D.BinPkgName)
debOfFile path =
    do mp <- ask
       return $ testPath (Map.lookup path mp)
    where
      -- testPath :: Maybe (Set FilePath) -> Maybe FilePath
      testPath Nothing = Nothing
      testPath (Just s) =
          case Set.size s of
            1 -> Just (Set.findMin s)
            _ -> Nothing

cond :: t -> t -> Bool -> t
cond ifF _ifT False = ifF
cond _ifF ifT True = ifT

readFile' :: FilePath -> IO Text
readFile' path =
    do file <- openFile path ReadMode
       hSetBinaryMode file True
       hGetContents file

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe path = (Just <$> readFile' path) `catchIOError` (\ _ -> return Nothing)

showDeps :: D.Relations -> String
showDeps = show . pretty . PP

showDeps' :: D.Relations -> String
showDeps' xss = show $ mconcat $ intersperse (text "\n ") $
    [pretty (PP xs) <> text "," | xs <- xss ]

-- | From Darcs.Utils - set the working directory and run an IO operation.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path m =
    E.bracket
        (do oldwd <- getCurrentDirectory
            let newwd = oldwd </> path
            setCurrentDirectory' newwd
            return oldwd)
        (\oldwd -> setCurrentDirectory' oldwd {- `catchall` return () -})
        (\_oldwd -> m)

setCurrentDirectory' :: FilePath -> IO ()
setCurrentDirectory' dir =
    try (setCurrentDirectory dir) >>= either handle return
    where
      handle e@(IOError {ioe_type = NoSuchThing}) = throw $ e {ioe_description = ioe_description e ++ ": " ++ show dir}
      handle e@(IOError {ioe_type = InappropriateType}) = throw $ e {ioe_description = ioe_description e ++ ": " ++ show dir}
      handle e@(IOError {ioe_type = typ}) = throw $ e {ioe_description = ioe_description e ++ " unexpected ioe_type: " ++ show typ}

{-
catchall :: IO a -> IO a -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)

-- catchNonSignal is a drop-in replacement for Control.Exception.catch, which allows
-- us to catch anything but a signal.  Useful for situations where we want
-- don't want to inhibit ctrl-C.

catchNonSignal :: IO a -> (E.SomeException -> IO a) -> IO a
catchNonSignal comp handler = catch comp handler'
    where handler' se =
           case fromException se :: Maybe SignalException of
             Nothing -> handler se
             Just _ -> E.throw se

newtype SignalException = SignalException Signal deriving (Show, Typeable)

instance Exception SignalException where
   toException e = SomeException e
   fromException (SomeException e) = cast e
-}

-- | Get directory contents minus dot files.
getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir =
    getDirectoryContents dir >>= return . filter (not . dotFile)
    where
      dotFile "." = True
      dotFile ".." = True
      dotFile _ = False

setMapMaybe :: ({-Ord a,-} Ord b) => (a -> Maybe b) -> Set a -> Set b
setMapMaybe p = Set.fromList . mapMaybe p . toList

zipMaps :: Ord k => (k -> Maybe a -> Maybe b -> Maybe c) -> Map k a -> Map k b -> Map k c
zipMaps f m n =
    Map.foldrWithKey h (Map.foldrWithKey g Map.empty m) n
    where
      g k a r = case f k (Just a) (Map.lookup k n) of
                  Just c -> Map.insert k c r              -- Both m and n have entries for k
                  Nothing -> r                            -- Only m has an entry for k
      h k b r = case Map.lookup k m of
                  Nothing -> case f k Nothing (Just b) of
                               Just c -> Map.insert k c r -- Only n has an entry for k
                               Nothing -> r
                  Just _ -> r

foldEmpty :: r -> ([a] -> r) -> [a] -> r
foldEmpty r _ [] = r
foldEmpty _ f l = f l

-- | If the current value of view x is Nothing, replace it with f.
maybeL :: Lens' a (Maybe b) -> Maybe b -> a -> a
maybeL l mb x = over l (maybe mb Just) x

indent :: [Char] -> String -> String
indent prefix s = unlines (List.map (prefix ++) (List.lines s))

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

read' :: Read a => (String -> a) -> String -> a
read' f s = fromMaybe (f s) (maybeRead s)

-- modifyM :: (Monad m, MonadTrans t, MonadState a (t m)) => (a -> m a) -> t m ()
-- modifyM f = get >>= lift . f >>= put

-- modifyM :: (Monad m, MonadTrans t, MonadState a (t m)) => (a -> m a) -> t m ()
modifyM :: MonadState a m => (a -> m a) -> m ()
modifyM f = get >>= f >>= put

-- read' :: Read a => String -> a
-- read' s = trace ("read " ++ show s) (read s)

-- | Version of 'Distribution.Verbosity.intToVerbosity' that first
-- clamps its argument to the acceptable range (0-3).
intToVerbosity' :: Int -> Verbosity
intToVerbosity' n = fromJust (intToVerbosity (max 0 (min 3 n)))

listElemLens :: (a -> Bool) -> Lens' [a] (Maybe a)
listElemLens p =
    lens lensGet lensPut
    where
      lensGet xs =
          case span (not . p) xs of
            (_, x : _) -> Just x
            _ -> Nothing
      lensPut xs Nothing  =
          case span (not . p) xs of
            (before, _ : after) -> before ++ after
            _ -> xs
      lensPut xs (Just x) =
          case span (not . p) xs of
            (before, _ : after) -> before ++ (x : after)
            _ -> xs ++ [x]

maybeLens :: a -> Lens' a b -> Lens' (Maybe a) b
maybeLens def l =
    lens (\ x -> (fromMaybe def x) ^. l)
         (\ b a -> case (a, b) of
                     (_, Nothing) -> Just (l .~ a $ def)
                     (_, Just b') -> Just (l .~ a $ b'))

fromEmpty :: Set a -> Set a -> Set a
fromEmpty d s | Set.null s = d
fromEmpty _ s = s

fromSingleton :: a -> ([a] -> a) -> Set a -> a
fromSingleton e multiple s =
    case toList s of
      [x] -> x
      [] -> e
      xs -> multiple xs

instance Pretty (PP PackageIdentifier) where
    pretty (PP p) = pretty (PP (pkgName p)) <> text "-" <> pretty (PP (pkgVersion p))

instance Pretty (PP PackageName) where
    pretty (PP p) = text (unPackageName p)

-- | Set @b@ if it currently isNothing and the argument isJust, that is
--  1. Nothing happens if the argument isNothing
--  2. Nothing happens if the current value isJust
(.?=) :: Monad m => Lens' a (Maybe b) -> Maybe b -> StateT a m ()
l .?= mx = use l >>= assign l . maybe mx Just

-- | This should probably be used in a lot of places.
escapeDebianWildcards :: String -> String
escapeDebianWildcards (c : more) | elem c "[]" = '\\' : c : escapeDebianWildcards more
escapeDebianWildcards (c : more) = c : escapeDebianWildcards more
escapeDebianWildcards "" = ""
