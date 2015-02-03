{-# LANGUAGE CPP #-}
module Debian.Debianize.Options
    ( options
    , compileArgs
    , compileEnvironmentArgs
    , compileCommandlineArgs
    , putEnvironmentArgs
    , withEnvironmentArgs
    ) where

import Control.Category ((.))
import Control.Monad.State (StateT)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (isDigit, ord)
import Data.Lens.Lazy (focus, Lens)
import Debian.Debianize.DebInfo (DebInfo, flags, binaryDebDescription)
import qualified  Debian.Debianize.DebInfo as D
import Debian.Debianize.Goodies (doExecutable)
import Debian.Debianize.InputCabalPackageDescription (flagOptions, Flags)
import Debian.Debianize.Monad (CabalT, DebianT)
import Debian.Debianize.Prelude ((%=), (+++=), (++=), (+=), maybeRead, (~=))
import qualified Debian.Debianize.Atoms as A
import qualified Debian.Debianize.BinaryDebDescription as B
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Orphans ()
import Debian.Policy (parseMaintainer, parseStandardsVersion, SourceFormat(Quilt3, Native3))
import Debian.Relation (BinPkgName(..), Relation(..), Relations, SrcPkgName(..))
import Debian.Relation.String (parseRelations)
import Debian.Version (parseDebianVersion)
import Distribution.Package (PackageName(..))
import Prelude hiding ((.), lines, log, null, readFile, sum)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(RequireOrder), getOpt', OptDescr(..))
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>), splitFileName)
import System.IO.Error (tryIOError)
import System.Posix.Env (setEnv)
import Text.Regex.TDFA ((=~))

-- | Apply a list of command line arguments to the monadic state.
compileArgs :: MonadIO m => [String] -> CabalT m ()
compileArgs args =
    case getOpt' RequireOrder options args of
      (os, [], [], []) -> sequence_ os
      (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                    ", Unrecognized: " ++ show unk ++
                                    ", Non-Options: " ++ show non)

-- | Get a list of arguments from the CABALDEBIAN environment variable
-- and apply them to the monadic state.
compileEnvironmentArgs :: MonadIO m => CabalT m ()
compileEnvironmentArgs = withEnvironmentArgs compileArgs

-- | Get the list of command line arguments and apply them to the
-- monadic state.
compileCommandlineArgs :: MonadIO m => CabalT m ()
compileCommandlineArgs = liftIO getArgs >>= compileArgs

-- | Read a value out of the CABALDEBIAN environment variable which is
-- the result of applying show to a [String].
withEnvironmentArgs :: MonadIO m => ([String] -> CabalT m a) -> CabalT m a
withEnvironmentArgs f =
    liftIO (tryIOError (getEnv "CABALDEBIAN")) >>= either (\ _ -> f []) (maybe (f []) f . maybeRead)

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True

-- | Options that modify other atoms.
options :: MonadIO m => [OptDescr (CabalT m ())]
options =
    [ Option "" ["executable"] (ReqArg (\ path -> executableOption path (\ bin e -> doExecutable bin e)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             (unlines [ "Create an individual binary package to hold this executable.  Other executables "
                      , " and data files are gathered into a single utils package named 'haskell-packagename-utils'."]),
      Option "" ["default-package"] (ReqArg (\ name -> (D.utilsPackageNameBase . A.debInfo) ~= Just name) "NAME")
             (unlines [ "Set the name of the catch-all package that receives all the files not included in a library package or "
                      , " some other executable package.  By default this is 'haskell-packagename-utils'."]),
      Option "" ["disable-haddock"] (NoArg ((D.noDocumentationLibrary . A.debInfo) ~= True))
             (unlines [ "Don't generate API documentation packages, usually named"
                      , "libghc-packagename-doc.  Use this if your build is crashing due to a"
                      , "haddock bug."]),
      Option "" ["missing-dependency"] (ReqArg (\ name -> (D.missingDependencies . A.debInfo) += (BinPkgName name)) "DEB")
             (unlines [ "This is the counterpart to --disable-haddock.  It prevents a package"
                      , "from being added to the build dependencies.  This is necessary, for example,"
                      , "when a dependency package was built with the --disable-haddock option, because"
                      , "normally cabal-debian assumes that the -doc package exists and adds it as a"
                      , "build dependency."]),
      Option "" ["debian-name-base"] (ReqArg (\ name -> (D.overrideDebianNameBase . A.debInfo) ~= (Just (DebBase name))) "NAME")
             (unlines [ "Use this name for the base of the debian binary packages - the string between 'libghc-'"
                      , " and '-dev'.  Normally this is derived from the hackage package name."]),
      Option "" ["source-package-name"] (ReqArg (\ name -> (D.sourcePackageName . A.debInfo) ~= (Just (SrcPkgName name))) "NAME")
             (unlines [ "Use this name for the debian source package, the name in the Source field at the top of the"
                      , "debian control file, and also at the very beginning of the debian/changelog file.  By default"
                      , "this is haskell-<cabalname>, where the cabal package name is downcased."]),
      Option "" ["source-section"] (ReqArg (\ name -> (S.section . D.control . A.debInfo) ~= Just (read name)) "NAME")
             "Set the Section: field of the Debian source package.",
      Option "" ["disable-library-profiling"] (NoArg ((D.noProfilingLibrary . A.debInfo) ~= True))
             (unlines [ "Don't generate profiling (-prof) library packages.  This has been used in one case"
                      , "where the package code triggered a compiler bug."]),
      Option "" ["maintainer"] (ReqArg (\ s -> either (error ("Invalid maintainer string: " ++ show s)) (((D.maintainerOption . A.debInfo) ~=) . Just) (parseMaintainer s)) "Maintainer Name <email addr>")
             (unlines [ "Supply a value for the Maintainer field.  Final value is computed from several inputs."]),
      Option "" ["uploader"] (ReqArg (\ s -> either (error ("Invalid uploader string: " ++ show s)) (\ x -> (D.uploadersOption . A.debInfo) %= (\ l -> l ++ [x])) (parseMaintainer s)) "Uploader Name <email addr>")
             (unlines [ "Add one entry to the uploader list"]),
      Option "" ["standards-version"] (ReqArg (\ sv -> (S.standardsVersion . D.control . A.debInfo) ~= Just (parseStandardsVersion sv)) "VERSION")
             "Claim compatibility to this version of the Debian policy (i.e. the value of the Standards-Version field)",
      Option "" ["build-dep"]
                 (ReqArg (\ name ->
                              case parseRelations name of
                                Left err -> error ("cabal-debian option --build-dep " ++ show name ++ ": " ++ show err)
                                Right rss -> (S.buildDepends . D.control . A.debInfo) %= (++ rss)) "Debian package relations")
                 (unlines [ "Add a dependency relation to the Build-Depends: field for this source package, e.g."
                          , ""
                          , "     --build-dep libglib2.0-dev"
                          , "     --build-dep 'libglib2.0-dev >= 2.2'" ]),
      Option "" ["build-dep-indep"]
                 (ReqArg (\ name ->
                              case parseRelations name of
                                Left err -> error ("cabal-debian option --build-dep-indep " ++ show name ++ ": " ++ show err)
                                Right rss -> (S.buildDependsIndep . D.control . A.debInfo) %= (++ rss)) "Debian binary package name")
                 (unlines [ "Similar to --build-dep, but the dependencies are added to Build-Depends-Indep, e.g.:"
                          , ""
                          , "    --build-dep-indep perl" ]),
      Option "" ["dev-dep"] (ReqArg (\ name -> (D.extraDevDeps . A.debInfo) %= (++ [[Rel (BinPkgName name) Nothing Nothing]])) "Debian binary package name")
             (unlines [ "Add an entry to the Depends: field of the -dev package, e.g."
                      , "'--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep."]),
      Option "" ["depends"]
             (ReqArg (addDep (\b -> B.depends . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             (unlines [ "Generalized --dev-dep - specify pairs A:B of debian binary package names, each"
                      , "A gets a Depends: B.  Note that B can have debian style version relations"]),
      Option "" ["conflicts"]
             (ReqArg (addDep (\b -> B.conflicts . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Conflicts field.",
      Option "" ["replaces"]
             (ReqArg (addDep (\b -> B.replaces . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Replaces field.",
      Option "" ["provides"]
             (ReqArg (addDep (\b -> B.provides . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Provides field.",
      Option "" ["recommends"]
             (ReqArg (addDep (\b -> B.recommends . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Recommends field.",
      Option "" ["suggests"]
             (ReqArg (addDep (\b -> B.suggests . B.relations . binaryDebDescription b)) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Suggests field.",
      Option "" ["map-dep"] (ReqArg (\ pair -> case break (== '=') pair of
                                                 (cab, (_ : deb)) -> (D.extraLibMap . A.debInfo) +++= (cab, rels deb)
                                                 (_, "") -> error "usage: --map-dep CABALNAME=RELATIONS") "CABALNAME=RELATIONS")
             (unlines [ "Specify what debian package name corresponds with a name that appears in"
                      , "the Extra-Library field of a cabal file, e.g. --map-dep cryptopp=libcrypto-dev."
                      , "I think this information is present somewhere in the packaging system, but"
                      , "I'm not sure of the details."]),
      Option "" ["deb-version"] (ReqArg (\ version -> (D.debVersion . A.debInfo) ~= Just (parseDebianVersion version)) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (\ rev -> (D.revision . A.debInfo) ~= Just rev) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"]
             (ReqArg (\ pair -> case break (== '=') pair of
                                  (_, (_ : ['0'])) -> return ()
                                  (cab, (_ : [d])) | isDigit d -> A.epochMap ++= (PackageName cab, ord d - ord '0')
                                  _ -> error "usage: --epoch-map CABALNAME=DIGIT") "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["exec-map"] (ReqArg (\ s -> case break (== '=') s of
                                               (cab, (_ : deb)) -> (D.execMap . A.debInfo) ++= (cab, rels deb)
                                               _ -> error "usage: --exec-map EXECNAME=RELATIONS") "EXECNAME=RELATIONS")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-prof-version-deps"] (NoArg ((D.omitProfVersionDeps . A.debInfo) ~= True))
             "Do not put the version dependencies on the prof packages that we put on the dev packages.",
      Option "" ["omit-lt-deps"] (NoArg ((D.omitLTDeps . A.debInfo) ~= True))
             (unlines [ "Remove all less-than dependencies from the generated control file.  Less-than"
                      , "dependencies are less useful and more troublesome for debian packages than cabal,"
                      , "because you can't install multiple versions of a given debian package.  For more"
                      , "google 'cabal hell'."]),
      Option "" ["quilt"] (NoArg ((D.sourceFormat . A.debInfo) ~= Just Quilt3))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "" ["native"] (NoArg ((D.sourceFormat . A.debInfo) ~= Just Native3))
             "The package has an no upstream tarball, write '3.0 (native)' into source/format.",
      Option "" ["official"] (NoArg ((D.official . A.debInfo) ~= True))
             "This packaging is created of the official Debian Haskell Group",
      Option "" ["builddir"] (ReqArg (\ s -> (D.buildDir . A.debInfo) ~= Just (s </> "build")) "PATH")
             (unlines [ "Subdirectory where cabal does its build, dist/build by default, dist-ghc when"
                      , "run by haskell-devscripts.  The build subdirectory is added to match the"
                      , "behavior of the --builddir option in the Setup script."])
    ] ++ map liftOpt flagOptions

liftOpt :: Monad m => OptDescr (StateT Flags m ()) -> OptDescr (StateT A.Atoms m ())
liftOpt (Option chrs strs desc doc) = Option chrs strs (liftDesc desc) doc

liftDesc :: Monad m => ArgDescr (StateT Flags m ()) -> ArgDescr (StateT A.Atoms m ())
liftDesc (NoArg x) = NoArg (focus (flags . A.debInfo) x)
liftDesc (ReqArg f s) = ReqArg (\ p -> focus (flags . A.debInfo) (f p)) s
liftDesc (OptArg f s) = OptArg (\ mp -> focus (flags . A.debInfo) (f mp)) s

anyrel :: BinPkgName -> Relation
anyrel x = Rel x Nothing Nothing

-- | Process a --executable command line argument
executableOption :: String -> (BinPkgName -> D.InstallFile -> a) -> a
executableOption arg f =
    case span (/= ':') arg of
      (sp, md) ->
          let (sd, name) = splitFileName sp in
          f (BinPkgName name)
            (D.InstallFile { D.execName = name
                           , D.destName = name
                           , D.sourceDir = case sd of "./" -> Nothing; _ -> Just sd
                           , D.destDir = case md of (':' : dd) -> Just dd; _ -> Nothing })

addDep' :: Monad m => (BinPkgName -> Lens DebInfo Relations) -> String -> DebianT m ()
addDep' lns arg = mapM_ (\ (b, rel) -> lns b %= (++ [[rel]])) (parseDeps arg)

addDep :: Monad m => (BinPkgName -> Lens DebInfo Relations) -> String -> CabalT m ()
addDep lns arg = mapM_ (\ (b, rel) -> (lns b . A.debInfo) %= (++ [[rel]])) (parseDeps arg)

parseDeps :: String -> [(BinPkgName, Relation)]
parseDeps arg =
    map pair (split arg)
    where
      split s =
          case s =~ "^[ \t,]*([^,]+)[ \t,]*" :: (String, String, String, [String]) of
            (_, _, tl, [hd]) -> hd : split tl
            (_, _, "", _) -> []
            _ -> error $ "Invalid dependency: " ++ show s
      pair s =
          case s =~ "^[ \t:]*([^ \t:]+)[ \t]*:[ \t]*(.+)[ \t]*" :: (String, String, String, [String]) of
            (_, _, _, [x, y]) -> (BinPkgName x, anyrel (BinPkgName y))
            _ -> error $ "Invalid dependency: " ++ show s

rels :: String -> Relations
rels s =
    case parseRelations s of
      Right relss -> relss
      _ -> error $ "Parse error in debian relations: " ++ show s
