{-# LANGUAGE CPP #-}
module Debian.Debianize.Options
    ( options
    , compileArgs
    , compileEnvironmentArgs
    , compileCommandlineArgs
    , putEnvironmentArgs
    , withEnvironmentArgs
    ) where

import Control.Monad.State (get, put)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toLower, toUpper, isDigit, ord)
import Data.Lens.Lazy (Lens)
import Data.Set (singleton, insert, delete)
import Debian.Debianize.Goodies (doExecutable)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude (read', maybeRead, (+=), (~=), (%=), (++=), (+++=))
import Debian.Debianize.Types
    (verbosity, dryRun, debAction, noDocumentationLibrary, noProfilingLibrary,
     missingDependencies, sourcePackageName, overrideDebianNameBase, cabalFlagAssignments, maintainer, buildDir, omitProfVersionDeps, omitLTDeps,
     sourceFormat, buildDepends, buildDependsIndep, extraDevDeps, depends, conflicts, replaces, provides,
     recommends, suggests, extraLibMap, debVersion, revision, epochMap, execMap, utilsPackageNameBase,
     standardsVersion, official)
import Debian.Debianize.Types.Atoms (Atoms, EnvSet(..), InstallFile(..), DebAction(..), setBuildEnv, compilerFlavors)
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Orphans ()
import Debian.Policy (SourceFormat(Quilt3, Native3), parseMaintainer, parseStandardsVersion)
import Debian.Relation (BinPkgName(..), SrcPkgName(..), Relations, Relation(..))
import Debian.Relation.String (parseRelations)
import Debian.Version (parseDebianVersion)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Package (PackageName(..))
import Prelude hiding (readFile, lines, null, log, sum)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..), ArgOrder(RequireOrder), getOpt')
import System.Environment (getArgs, getEnv)
import System.FilePath ((</>), splitFileName)
import System.IO.Error (tryIOError)
import System.Posix.Env (setEnv)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

compileArgs :: MonadIO m => [String] -> DebT m ()
compileArgs args =
    case getOpt' RequireOrder options args of
      (os, [], [], []) -> sequence_ os
      (_, non, unk, errs) -> error ("Errors: " ++ show errs ++
                                    ", Unrecognized: " ++ show unk ++
                                    ", Non-Options: " ++ show non)

compileEnvironmentArgs :: MonadIO m => DebT m ()
compileEnvironmentArgs = withEnvironmentArgs compileArgs

compileCommandlineArgs :: MonadIO m => DebT m ()
compileCommandlineArgs = liftIO getArgs >>= compileArgs

-- | Read a value out of the CABALDEBIAN environment variable which is
-- the result of applying show to a [String].
withEnvironmentArgs :: MonadIO m => ([String] -> DebT m a) -> DebT m a
withEnvironmentArgs f =
    liftIO (tryIOError (getEnv "CABALDEBIAN")) >>= either (\ _ -> f []) (maybe (f []) f . maybeRead)

-- | Insert a value for CABALDEBIAN into the environment that the
-- withEnvironment* functions above will find and use.  E.g.
-- putEnvironmentFlags ["--dry-run", "--validate"] (debianize defaultFlags)
putEnvironmentArgs :: [String] -> IO ()
putEnvironmentArgs fs = setEnv "CABALDEBIAN" (show fs) True

-- | Options that modify other atoms.
options :: MonadIO m => [OptDescr (DebT m ())]
options =
    [ Option "v" ["verbose"] (ReqArg (\ s -> verbosity ~= (read' (\ s' -> error $ "verbose: " ++ show s') s)) "n")
             "Change the amount of progress messages generated",
      Option "n" ["dry-run", "compare"] (NoArg (dryRun ~= True))
             "Just compare the existing debianization to the one we would generate.",
      Option "h?" ["help"] (NoArg (debAction ~= Usage))
             "Show this help text",
      Option "" ["executable"] (ReqArg (\ path -> executableOption path (\ bin e -> doExecutable bin e)) "SOURCEPATH or SOURCEPATH:DESTDIR")
             (unlines [ "Create an individual binary package to hold this executable.  Other executables "
                      , " and data files are gathered into a single utils package named 'haskell-packagename-utils'."]),
      Option "" ["default-package"] (ReqArg (\ name -> utilsPackageNameBase ~= Just name) "NAME")
             (unlines [ "Set the name of the catch-all package that receives all the files not included in a library package or "
                      , " some other executable package.  By default this is 'haskell-packagename-utils'."]),
      Option "" ["disable-haddock"] (NoArg (noDocumentationLibrary ~= True))
             (unlines [ "Don't generate API documentation packages, usually named"
                      , "libghc-packagename-doc.  Use this if your build is crashing due to a"
                      , "haddock bug."]),
      Option "" ["missing-dependency"] (ReqArg (\ name -> missingDependencies += (BinPkgName name)) "DEB")
             (unlines [ "This is the counterpart to --disable-haddock.  It prevents a package"
                      , "from being added to the build dependencies.  This is necessary, for example,"
                      , "when a dependency package was built with the --disable-haddock option, because"
                      , "normally cabal-debian assumes that the -doc package exists and adds it as a"
                      , "build dependency."]),
      Option "" ["debian-name-base"] (ReqArg (\ name -> overrideDebianNameBase ~= (Just (DebBase name))) "NAME")
             (unlines [ "Use this name for the base of the debian binary packages - the string between 'libghc-'"
                      , " and '-dev'.  Normally this is derived from the hackage package name."]),
      Option "" ["source-package-name"] (ReqArg (\ name -> sourcePackageName ~= (Just (SrcPkgName name))) "NAME")
             (unlines [ "Use this name for the debian source package, the name in the Source field at the top of the"
                      , "debian control file, and also at the very beginning of the debian/changelog file.  By default"
                      , "this is haskell-<cabalname>, where the cabal package name is downcased."]),
      Option "" ["disable-library-profiling"] (NoArg (noProfilingLibrary ~= True))
             (unlines [ "Don't generate profiling (-prof) library packages.  This has been used in one case"
                      , "where the package code triggered a compiler bug."]),
      Option "" ["maintainer"] (ReqArg (\ maint -> either (error ("Invalid maintainer string: " ++ show maint)) ((maintainer ~=) . Just) (parseMaintainer maint)) "Maintainer Name <email addr>")
             (unlines [ "Override the Maintainer name and email given in $DEBEMAIL or $EMAIL or $DEBFULLNAME or $FULLNAME"]),
      Option "" ["standards-version"] (ReqArg (\ sv -> standardsVersion ~= Just (parseStandardsVersion sv)) "VERSION")
             "Claim compatibility to this version of the Debian policy (i.e. the value of the Standards-Version field)",
      Option "" ["build-dep"]
                 (ReqArg (\ name ->
                              case parseRelations name of
                                Left err -> error ("cabal-debian option --build-dep " ++ show name ++ ": " ++ show err)
                                Right rss -> buildDepends %= (++ rss)) "Debian package relations")
                 (unlines [ "Add a dependency relation to the Build-Depends: field for this source package, e.g."
                          , ""
                          , "     --build-dep libglib2.0-dev"
                          , "     --build-dep 'libglib2.0-dev >= 2.2'" ]),
      Option "" ["build-dep-indep"]
                 (ReqArg (\ name ->
                              case parseRelations name of
                                Left err -> error ("cabal-debian option --build-dep-indep " ++ show name ++ ": " ++ show err)
                                Right rss -> buildDependsIndep %= (++ rss)) "Debian binary package name")
                 (unlines [ "Similar to --build-dep, but the dependencies are added to Build-Depends-Indep, e.g.:"
                          , ""
                          , "    --build-dep-indep perl" ]),
      Option "" ["dev-dep"] (ReqArg (\ name -> extraDevDeps %= (++ [[Rel (BinPkgName name) Nothing Nothing]])) "Debian binary package name")
             (unlines [ "Add an entry to the Depends: field of the -dev package, e.g."
                      , "'--dev-dep libncurses5-dev'.  It might be good if this implied --build-dep."]),
      Option "" ["depends"]
             (ReqArg (addDep depends) "deb:deb,deb:deb,...")
             (unlines [ "Generalized --dev-dep - specify pairs A:B of debian binary package names, each"
                      , "A gets a Depends: B.  Note that B can have debian style version relations"]),
      Option "" ["conflicts"]
             (ReqArg (addDep conflicts) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Conflicts field.",
      Option "" ["replaces"]
             (ReqArg (addDep replaces) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Replaces field.",
      Option "" ["provides"]
             (ReqArg (addDep provides) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Provides field.",
      Option "" ["recommends"]
             (ReqArg (addDep recommends) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Recommends field.",
      Option "" ["suggests"]
             (ReqArg (addDep suggests) "deb:deb,deb:deb,...")
             "Like --depends, modifies the Suggests field.",
      Option "" ["map-dep"] (ReqArg (\ pair -> case break (== '=') pair of
                                                 (cab, (_ : deb)) -> extraLibMap +++= (cab, rels deb)
                                                 (_, "") -> error "usage: --map-dep CABALNAME=RELATIONS") "CABALNAME=RELATIONS")
             (unlines [ "Specify what debian package name corresponds with a name that appears in"
                      , "the Extra-Library field of a cabal file, e.g. --map-dep cryptopp=libcrypto-dev."
                      , "I think this information is present somewhere in the packaging system, but"
                      , "I'm not sure of the details."]),
      Option "" ["deb-version"] (ReqArg (\ version -> debVersion ~= Just (parseDebianVersion version)) "VERSION")
             "Specify the version number for the debian package.  This will pin the version and should be considered dangerous.",
      Option "" ["revision"] (ReqArg (\ rev -> revision ~= Just rev) "REVISION")
             "Add this string to the cabal version to get the debian version number.  By default this is '-1~hackage1'.  Debian policy says this must either be empty (--revision '') or begin with a dash.",
      Option "" ["epoch-map"]
             (ReqArg (\ pair -> case break (== '=') pair of
                                  (_, (_ : ['0'])) -> return ()
                                  (cab, (_ : [d])) | isDigit d -> epochMap ++= (PackageName cab, ord d - ord '0')
                                  _ -> error "usage: --epoch-map CABALNAME=DIGIT") "CABALNAME=DIGIT")
             "Specify a mapping from the cabal package name to a digit to use as the debian package epoch number, e.g. --epoch-map HTTP=1",
      Option "" ["exec-map"] (ReqArg (\ s -> case break (== '=') s of
                                               (cab, (_ : deb)) -> execMap ++= (cab, rels deb)
                                               _ -> error "usage: --exec-map EXECNAME=RELATIONS") "EXECNAME=RELATIONS")
             "Specify a mapping from the name appearing in the Build-Tool field of the cabal file to a debian binary package name, e.g. --exec-map trhsx=haskell-hsx-utils",
      Option "" ["omit-prof-version-deps"] (NoArg (omitProfVersionDeps ~= True))
             "Do not put the version dependencies on the prof packages that we put on the dev packages.",
      Option "" ["omit-lt-deps"] (NoArg (omitLTDeps ~= True))
             (unlines [ "Remove all less-than dependencies from the generated control file.  Less-than"
                      , "dependencies are less useful and more troublesome for debian packages than cabal,"
                      , "because you can't install multiple versions of a given debian package.  For more"
                      , "google 'cabal hell'."]),
      Option "" ["quilt"] (NoArg (sourceFormat ~= Just Quilt3))
             "The package has an upstream tarball, write '3.0 (quilt)' into source/format.",
      Option "" ["native"] (NoArg (sourceFormat ~= Just Native3))
             "The package has an no upstream tarball, write '3.0 (native)' into source/format.",
      Option "" ["official"] (NoArg (official ~= True))
             "This packaging is created of the official Debian Haskell Group",
      Option "" ["builddir"] (ReqArg (\ s -> buildDir ~= singleton (s </> "build")) "PATH")
             (unlines [ "Subdirectory where cabal does its build, dist/build by default, dist-ghc when"
                      , "run by haskell-devscripts.  The build subdirectory is added to match the"
                      , "behavior of the --builddir option in the Setup script."]),

      let f :: MonadIO m => String -> DebT m ()
          f s = get >>= setBuildEnv (EnvSet {cleanOS = s </> "clean", dependOS = s </> "depend", buildOS = s </> "build"}) >>= put in
      Option "" ["buildenvdir"] (ReqArg f "PATH")
             (unlines [ "Directory containing the build environment for which the debianization will"
                      , "be generated.  This determines which compiler will be available, which in turn"
                      , "determines which basic libraries can be provided by the compiler.  This can be"
                      , "set to /, but it must be set."]),
      Option "" ["ghc"] (NoArg (compilerFlavors %= (insert GHC))) "Generate packages for GHC - same as --with-compiler GHC",
      Option "" ["no-ghc"] (NoArg (compilerFlavors %= (delete GHC))) "Do not generate packages for GHC - same as --without-compiler GHC",
#if MIN_VERSION_Cabal(1,21,0)
      Option "" ["ghcjs"] (NoArg (compilerFlavors %= (insert GHCJS))) "Generate packages for GHCJS - same as --with-compiler GHCJS",
      Option "" ["no-ghcjs"] (NoArg (compilerFlavors %= (delete GHCJS))) "Do not generate packages for GHCJS - same as --without-compiler GHCJS",
#endif
      Option "" ["hugs"] (NoArg (compilerFlavors %= (insert Hugs))) "Generate packages for Hugs - same as --with-compiler GHC",
      Option "" ["no-hugs"] (NoArg (compilerFlavors %= (delete Hugs))) "Do not generate packages for Hugs - same as --without-compiler Hugs",
      Option "" ["with-compiler"] (ReqArg (\ s -> maybe (error $ "Invalid compiler id: " ++ show s)
                                                        (\ hc -> compilerFlavors %= insert hc)
                                                        (readMaybe (map toUpper s) :: Maybe CompilerFlavor)) "COMPILER")
             (unlines [ "Generate packages for this CompilerFlavor" ]),
      Option "" ["without-compiler"] (ReqArg (\ s -> maybe (error $ "Invalid compiler id: " ++ show s)
                                                           (\ hc -> compilerFlavors %= delete hc)
                                                           (readMaybe (map toUpper s) :: Maybe CompilerFlavor)) "COMPILER")
             (unlines [ "Do not generate packages for this CompilerFlavors, e.g. GHC" ]),
      Option "f" ["flags"] (ReqArg (\ fs -> mapM_ (cabalFlagAssignments +=) (flagList fs)) "FLAGS")
             (unlines [ "Flags to pass to the finalizePackageDescription function in"
                      , "Distribution.PackageDescription.Configuration when loading the cabal file."]),

      Option "" ["debianize"] (NoArg (debAction ~= Debianize))
             "Deprecated - formerly used to get what is now the normal benavior.",
      Option "" ["substvar"] (ReqArg (\ name -> debAction ~= (SubstVar (read' (\ s -> error $ "substvar: " ++ show s) name))) "Doc, Prof, or Dev")
             (unlines [ "With this option no debianization is generated.  Instead, the list"
                      , "of dependencies required for the dev, prof or doc package (depending"
                      , "on the argument) is printed to standard output.  These can be added"
                      , "to the appropriate substvars file.  (This is an option whose use case"
                      , "is lost in the mists of time.)"])
    ]

anyrel :: BinPkgName -> Relation
anyrel x = Rel x Nothing Nothing

-- | Process a --executable command line argument
executableOption :: String -> (BinPkgName -> InstallFile -> a) -> a
executableOption arg f =
    case span (/= ':') arg of
      (sp, md) ->
          let (sd, name) = splitFileName sp in
          f (BinPkgName name)
            (InstallFile { execName = name
                         , destName = name
                         , sourceDir = case sd of "./" -> Nothing; _ -> Just sd
                         , destDir = case md of (':' : dd) -> Just dd; _ -> Nothing })

addDep :: Monad m => (BinPkgName -> Lens Atoms Relations) -> String -> DebT m ()
addDep lns arg = mapM_ (\ (b, rel) -> lns b %= (++ [[rel]])) (parseDeps arg)

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

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

rels :: String -> Relations
rels s =
    case parseRelations s of
      Right relss -> relss
      _ -> error $ "Parse error in debian relations: " ++ show s
