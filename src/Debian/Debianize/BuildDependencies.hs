-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.BuildDependencies
    ( debianBuildDeps
    , debianBuildDepsIndep
    ) where


#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Lens
import Control.Monad.State (MonadState(get))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Char (isSpace, toLower)
import Data.Function (on)
import Data.List as List (filter, groupBy, intercalate, map, minimumBy, nub, sortBy)
import Data.Map as Map (lookup, Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set as Set (empty, fold, fromList, map, member, Set, singleton, toList, union)
import Debian.Debianize.Prelude
import Debian.Debianize.BasicInfo (buildEnv, compilerFlavor, EnvSet(dependOS))
import Debian.Debianize.Bundled (builtIn)
import qualified Debian.Debianize.DebInfo as D
import Debian.Debianize.DebianName (mkPkgName, mkPkgName')
import Debian.Debianize.Monad as Monad (CabalInfo, CabalT)
import qualified Debian.Debianize.BinaryDebDescription as B
import qualified Debian.Debianize.CabalInfo as A
import qualified Debian.Debianize.SourceDebDescription as S
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.GHC (compilerPackageName)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(..), checkVersionReq, Relation(..), Relations)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Version (DebianVersion, parseDebianVersion')
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Package (Dependency(..), PackageIdentifier(pkgName, pkgVersion), PackageName)
import Distribution.PackageDescription as Cabal (BuildInfo(..), BuildInfo(buildTools, extraLibs, pkgconfigDepends), Library(..), Executable(..), TestSuite(..))
import Distribution.PackageDescription (PackageDescription)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(library, executables, testSuites))
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency(..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency(..))
#endif
import Distribution.Version (anyVersion, asVersionIntervals, earlierVersion, foldVersionRange', fromVersionIntervals, intersectVersionRanges, isNoVersion, laterVersion, orEarlierVersion, orLaterVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
import Distribution.Version.Invert (invertVersionRange)
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.Directory (findExecutable)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

-- | Naive conversion of Cabal build dependencies to Debian
-- dependencies will usually result in a self dependency, due to the
-- fact that a Cabal executable often depends on the associated
-- library to build.  Due to the fact that Debian build dependencies
-- are global to the package, this results in unwanted self
-- dependencies, which usually need to be filtered out.
-- Unfortunately, some Debian packages actually do depend on an
-- earlier version of themselves to build (e.g. most compilers.)  So a
-- command line option is probably necessary.
--
-- selfDependency :: PackageIdentifier -> Dependency_ -> Bool
-- selfDependency pkgId (BuildDepends (Dependency name _)) = name == pkgName pkgId
-- selfDependency _ _ = False

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Monad m => [BuildInfo] -> CabalT m [Dependency_]
allBuildDepends buildInfos =
    allBuildDepends'
      (mergeCabalDependencies $ concatMap Cabal.targetBuildDepends buildInfos)
      (mergeCabalDependencies $ mapMaybe convertLegacy $ concatMap buildTools buildInfos)
      (mergeCabalDependencies $ mapMaybe convertPkgconfig $  concatMap pkgconfigDepends buildInfos)
      (concatMap extraLibs buildInfos) >>=
    return {- . List.filter (not . selfDependency (Cabal.package pkgDesc)) -}
    where
#if MIN_VERSION_Cabal(2,0,0)
      convertLegacy :: LegacyExeDependency -> Maybe Dependency
      convertLegacy = const Nothing
      convertPkgconfig :: PkgconfigDependency -> Maybe Dependency
      convertPkgconfig = const Nothing
#else
      convertLegacy = Just
      convertPkgconfig = Just
#endif
      allBuildDepends' :: Monad m => [Dependency] -> [Dependency] -> [Dependency] -> [String] -> CabalT m [Dependency_]
      allBuildDepends' buildDepends' buildTools' pkgconfigDepends' extraLibs' =
          do atoms <- get
             return $ nub $ List.map BuildDepends buildDepends' ++
                            List.map BuildTools buildTools' ++
                            List.map PkgConfigDepends pkgconfigDepends' ++
                            [ExtraLibs (fixDeps atoms extraLibs')]

      fixDeps :: CabalInfo -> [String] -> Relations
      fixDeps atoms xs =
          concatMap (\ cab -> fromMaybe [[D.Rel (D.BinPkgName ("lib" ++ List.map toLower cab ++ "-dev")) Nothing Nothing]]
                                        (Map.lookup cab (view (A.debInfo . D.extraLibMap) atoms))) xs

-- | Take the intersection of all the dependencies on a given package name
mergeCabalDependencies :: [Dependency] -> [Dependency]
mergeCabalDependencies =
    List.map (foldl1 (\ (Dependency name range1) (Dependency _ range2) -> Dependency name (intersectVersionRanges range1 range2))) . groupBy ((==) `on` dependencyPackage) . sortBy (compare `on` dependencyPackage)
    where
      dependencyPackage (Dependency x _) = x

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: (MonadIO m, Functor m) => PackageDescription -> CabalT m D.Relations
debianBuildDeps pkgDesc =
    do hflavor <- use (A.debInfo . D.flags . compilerFlavor)
       let hcs = singleton hflavor -- vestigial
       let hcTypePairsLibs =
               fold union empty $
                  Set.map (\ hc' -> Set.map (hc',) $ hcPackageTypesLibs hc') hcs
       let hcTypePairsBins =
               fold union empty $
                  Set.map (\ hc' -> Set.map (hc',) $ hcPackageTypesBins hc') hcs
       let hcTypePairsTests =
               fold union empty $
                  Set.map (\ hc' -> Set.map (hc',) $ hcPackageTypesTests hc') hcs

       libDeps <- allBuildDepends (maybe [] (return . libBuildInfo) (Cabal.library pkgDesc))
       binDeps <- allBuildDepends (List.map buildInfo (Cabal.executables pkgDesc))
       testDeps <- allBuildDepends (List.map testBuildInfo (Cabal.testSuites pkgDesc))

       liftIO (putStrLn ("library dependencies: " ++ show libDeps))
       liftIO (putStrLn ("executable dependencies: " ++ show binDeps))
       liftIO (putStrLn (intercalate "\n  " ("executables:" :  fmap show (Cabal.executables pkgDesc))))
       liftIO (putStrLn ("test suite dependencies: " ++ show testDeps))

       testsStatus <- use (A.debInfo . D.testsStatus)

       cDeps <- nub . concat . concat <$> sequence
            [ mapM (buildDependencies hcTypePairsLibs) libDeps
            , mapM (buildDependencies hcTypePairsBins) binDeps
            , mapM (buildDependencies hcTypePairsTests) (if testsStatus /= D.TestsDisable then testDeps else [])
            ]

       bDeps <- use (A.debInfo . D.control . S.buildDepends)
       prof <- not <$> use (A.debInfo . D.noProfilingLibrary)
       official <- use (A.debInfo . D.official)
       compat <- use (A.debInfo . D.compat)
       let ghcdev = compilerPackageName hflavor B.Development
       let ghcrel = if member GHC hcs then maybe [] ((: []) . anyrel') ghcdev else []
       let ghcprof = compilerPackageName hflavor B.Profiling
       let ghcrelprof = if prof then maybe [] ((: []) . anyrel') ghcprof else []
       let xs = nub $ [maybe [] (\ n -> [D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion' (show n)))) Nothing]) compat,
                       [D.Rel (D.BinPkgName "haskell-devscripts-minimal") Nothing Nothing,
                        D.Rel (D.BinPkgName "haskell-devscripts") (Just $ D.GRE $ parseDebianVersion' $ if official then "0.9" else "0.8" :: String) Nothing],
                       anyrel "cdbs"] ++
                      (ghcrel ++ ghcrelprof) ++
{-
#if MIN_VERSION_Cabal(1,22,0)
                      (if member GHCJS (Set.map _hcFlavor hcs) then [anyrel (compilerPackageName hflavor B.Development)] else []) ++
#endif
-}
                       bDeps ++
                       cDeps
       filterMissing xs
    where
      hcPackageTypesLibs :: CompilerFlavor -> Set B.PackageType
      hcPackageTypesLibs GHC = fromList [B.Development, B.Profiling]
#if MIN_VERSION_Cabal(1,22,0)
      hcPackageTypesLibs GHCJS = fromList [B.Development]
#endif
      hcPackageTypesLibs hc = error $ "Unsupported compiler flavor: " ++ show hc

      -- No point in installing profiling packages for the dependencies
      -- of binaries and test suites
      hcPackageTypesBins :: CompilerFlavor -> Set B.PackageType
      hcPackageTypesBins _ = singleton B.Development

      hcPackageTypesTests :: CompilerFlavor -> Set B.PackageType
      hcPackageTypesTests _ = singleton B.Development

-- | Collect the dependencies required to build any packages that have
-- architecture "all".
debianBuildDepsIndep :: (MonadIO m, Functor m) => PackageDescription -> CabalT m D.Relations
debianBuildDepsIndep pkgDesc =
    do hc <- use (A.debInfo . D.flags . compilerFlavor)
       let hcs = singleton hc -- vestigial
       doc <- not <$> use (A.debInfo . D.noDocumentationLibrary)
       bDeps <- use (A.debInfo . D.control . S.buildDependsIndep)
       libDeps <- allBuildDepends (maybe [] (return . libBuildInfo) (Cabal.library pkgDesc))
       cDeps <- mapM docDependencies libDeps
       let ghcdoc = compilerPackageName hc B.Documentation
       let hcdocdep = if doc && member GHC hcs then maybe [] ((: []) . anyrel') ghcdoc else []
       let xs = nub $ if doc && isJust (Cabal.library pkgDesc)
                      then hcdocdep ++ bDeps ++ concat cDeps
                      else []
       filterMissing xs

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have use to all the cross references.
docDependencies :: (MonadIO m, Functor m) => Dependency_ -> CabalT m D.Relations
docDependencies (BuildDepends (Dependency name ranges)) =
    do hc <- use (A.debInfo . D.flags . compilerFlavor)
       let hcs = singleton hc -- vestigial
       omitProfDeps <- use (A.debInfo . D.omitProfVersionDeps)
       concat <$> mapM (\ hc' -> dependencies hc' B.Documentation name ranges omitProfDeps) (toList hcs)
docDependencies _ = return []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: (MonadIO m, Functor m) => Set (CompilerFlavor, B.PackageType) -> Dependency_ -> CabalT m D.Relations
buildDependencies hcTypePairs (BuildDepends (Dependency name ranges)) =
    use (A.debInfo . D.omitProfVersionDeps) >>= \ omitProfDeps ->
    concat <$> mapM (\ (hc, typ) -> dependencies hc typ name ranges omitProfDeps) (toList hcTypePairs)
buildDependencies _ dep@(ExtraLibs _) =
    do mp <- use (A.debInfo . D.execMap)
       return $ concat $ adapt mp dep
buildDependencies _ dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          do mp <- get >>= return . view (A.debInfo . D.execMap)
             return $ concat $ adapt mp dep
      Nothing ->
          return []

adapt :: Map.Map String Relations -> Dependency_ -> [Relations]
adapt mp (PkgConfigDepends (Dependency pkg _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt mp (BuildTools (Dependency pkg _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency pkg _)) = [[[D.Rel (D.BinPkgName (unPackageName pkg)) Nothing Nothing]]]

-- There are three reasons this may not work, or may work
-- incorrectly: (1) the build environment may be a different
-- distribution than the parent environment (the environment the
-- autobuilder was run from), so the packages in that
-- environment might have different names, (2) the package
-- we are looking for may not be installed in the parent
-- environment, and (3) the apt-file executable is not installed.
aptFile :: String -> [Relations] -- Maybe would probably be more correct
aptFile pkg = unsafePerformIO $
    findExecutable "apt-file" >>= aptFile'
  where
    aptFile' Nothing = error "The apt-file executable could not be found."
    aptFile' (Just aptfile) = do
        ret <- readProcessWithExitCode aptfile ["-l", "search", pkg ++ ".pc"] ""
        return $ case ret of
                  (ExitSuccess, out, _) ->
                      case takeWhile (not . isSpace) out of
                        "" -> error $ "Unable to locate a debian package containing the build tool " ++ pkg ++
                                      ", try using --exec-map " ++ pkg ++ ":<debname> or execMap " ++ show pkg ++
                                      " [[Rel (BinPkgName \"<debname>\") Nothing Nothing]]"
                        s -> [[[D.Rel (D.BinPkgName s) Nothing Nothing]]]
                  _ -> []

anyrel :: String -> [D.Relation]
anyrel x = anyrel' (D.BinPkgName x)

anyrel' :: D.BinPkgName -> [D.Relation]
anyrel' x = [D.Rel x Nothing Nothing]

-- | Turn a cabal dependency into debian dependencies.  The result
-- needs to correspond to a single debian package to be installed,
-- so we will return just an OrRelation.
dependencies :: MonadIO m => CompilerFlavor -> B.PackageType -> PackageName -> VersionRange -> Bool -> CabalT m Relations
dependencies hc typ name cabalRange omitProfVersionDeps =
    do nameMap <- use A.debianNameMap
       -- Compute a list of alternative debian dependencies for
       -- satisfying a cabal dependency.  The only caveat is that
       -- we may need to distribute any "and" dependencies implied
       -- by a version range over these "or" dependences.
       let alts :: [(BinPkgName, VersionRange)]
           alts = case Map.lookup name nameMap of
                    -- If there are no splits for this package just
                    -- return the single dependency for the package.
                    Nothing -> [(mkPkgName hc name typ, cabalRange')]
                    -- If there are splits create a list of (debian package name, VersionRange) pairs
                    Just splits' -> List.map (\ (n, r) -> (mkPkgName' hc typ n, r)) (packageRangesFromVersionSplits splits')
       mapM convert alts >>= mapM (doBundled typ name hc) . convert' . canonical . Or . catMaybes
    where
      convert (dname, range) =
          case isNoVersion range''' of
            True -> return Nothing
            False ->
                foldVersionRange'
                          (return $ Rel' (D.Rel dname Nothing Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.EEQ dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.SGR dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.SLT dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.GRE dv)) Nothing))
                          (\ v -> debianVersion' name v >>= \ dv -> return $ Rel' (D.Rel dname (Just (D.LTE dv)) Nothing))
#if MIN_VERSION_Cabal(2,0,0)
                          (\ x y -> debianVersion' name x >>= \ dvx ->
                                    debianVersion' name y >>= \ dvy ->
                                    return $ And [Rel' (D.Rel dname (Just (D.GRE dvx)) Nothing),
                                                  Rel' (D.Rel dname (Just (D.SLT dvy)) Nothing)])
#endif
                          (\ x y -> debianVersion' name x >>= \ dvx ->
                                    debianVersion' name y >>= \ dvy ->
                                    return $ And [Rel' (D.Rel dname (Just (D.GRE dvx)) Nothing),
                                                  Rel' (D.Rel dname (Just (D.SLT dvy)) Nothing)])
                          (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ Or [x', y'])
                          (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ And [x', y'])
                          id
                          range''' >>= return . Just
          where
            -- Choose the simpler of the two
            range''' = canon (simpler range' range'')
            -- Unrestrict the range for versions that we know don't exist for this debian package
            range'' = canon (unionVersionRanges range' (invertVersionRange range))
            -- Restrict the range to the versions specified for this debian package
            range' = intersectVersionRanges cabalRange' range
            -- When we see a cabal equals dependency we need to turn it into
            -- a wildcard because the resulting debian version numbers have
            -- various suffixes added.
      cabalRange' | typ `elem` noVersionPackageType = anyVersion
                  | otherwise = foldVersionRange'
            anyVersion
            withinVersion  -- <- Here we are turning equals into wildcard
            laterVersion
            earlierVersion
            orLaterVersion
            orEarlierVersion
            (\ lb ub -> intersectVersionRanges (orLaterVersion lb) (earlierVersion ub))
#if MIN_VERSION_Cabal(2,0,0)
            (\ lb ub -> intersectVersionRanges (orLaterVersion lb) (earlierVersion ub))
#endif
            unionVersionRanges
            intersectVersionRanges
            id
            cabalRange
      noVersionPackageType = (if omitProfVersionDeps then [B.Profiling] else []) ++ [B.Documentation]
      simpler v1 v2 = minimumBy (compare `on` (length . asVersionIntervals)) [v1, v2]
      -- Simplify a VersionRange
      canon = fromVersionIntervals . toVersionIntervals

-- | If a package is bundled with the compiler we make the
-- compiler a substitute for that package.  If we were to
-- specify the virtual package (e.g. libghc-base-dev) we would
-- have to make sure not to specify a version number.
doBundled :: MonadIO m =>
             B.PackageType
          -> PackageName
          -> CompilerFlavor
          -> [D.Relation]
          -> CabalT m [D.Relation]
doBundled typ name hc rels = do
  let hcname = compilerPackageName hc typ
  mapM (doRel hcname) rels >>= return . concat
    where
      -- If a library is built into the compiler, this is the debian
      -- package name the compiler will conflict with.
      doRel :: Monad m => Maybe BinPkgName -> D.Relation -> CabalT m [D.Relation]
      doRel hcname rel@(D.Rel dname req _) = do
        let comp = maybe [] (\x -> [D.Rel x Nothing Nothing]) hcname
        -- gver <- use ghcVersion
        root <- use (A.debInfo . D.flags . buildEnv) >>= return . dependOS
        -- Look at what version of the package is provided by the compiler.
        atoms <- get
        -- What version of this package (if any) does the compiler provide?
        let relInfo = builtIn hc root
            pver = listToMaybe $ fmap (debianVersion'' atoms) (filter ((== name) . pkgName) relInfo)
        -- The name this library would have if it was in the compiler conflicts list.
        let naiveDebianName = mkPkgName hc name typ
        -- The compiler should appear in the build dependency
        -- if it provides a suitable version of the library,
        -- or if it conflicts with all versions of the
        -- library (which, if pver is Nothing, will certainly
        -- result in an error which needs to be corrected in
        -- the packaging.)
        let compilerDependency = if isJust pver && (checkVersionReq req pver || (dname == naiveDebianName && conflictsWithHC naiveDebianName)) then comp else []
        -- The library package can satisfy the dependency if
        -- the compiler doesn't provide a version, or if the
        -- compiler doesn't conflict with the package's
        -- debian name.
        let libraryDependency = if isNothing pver || dname /= naiveDebianName || not (conflictsWithHC naiveDebianName) then [rel] else []
        -- Is the version number in the library dependency newer than
        -- the compiler version?  If so it should appear to its left,
        -- otherwise to its right.
        return $ case req of
                   Just (D.SLT lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   Just (D.LTE lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   Just (D.EEQ lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   _ -> libraryDependency ++ compilerDependency
      -- FIXME: we are assuming here that ghc conflicts with all the
      -- library packages it provides but it no longer conflicts with
      -- libghc-cabal-dev.  We can now check these conflicts using the
      -- new functions in Bundled.
      conflictsWithHC (BinPkgName "libghc-cabal-dev") = False
      conflictsWithHC (BinPkgName "libghc-cabal-prof") = False
      conflictsWithHC (BinPkgName "libghc-cabal-doc") = False
      conflictsWithHC _ = True

{-
doBundled :: Monad m =>
             B.PackageType  -- Documentation, Profiling, Development...
          -> PackageName    -- Cabal package name
          -> [D.Relation]   -- Original set of debian dependencies
          -> CabalT m [D.Relation] -- Modified debian dependencies accounting for the packages the compiler provides
doBundled hc typ name rels =
    concat <$> mapM doRel rels
    where
      doRel :: Monad m => D.Relation -> CabalT m [D.Relation]
      doRel rel@(D.Rel dname req _) = do
        hc <- use
-}

-- Convert a cabal version to a debian version, adding an epoch number if requested
debianVersion' :: Monad m => PackageName -> Version -> CabalT m DebianVersion
debianVersion' name v =
    do atoms <- get
       return $ parseDebianVersion' (maybe "" (\ n -> show n ++ ":") (Map.lookup name (view A.epochMap atoms)) ++ showVersion v)

debianVersion'' :: CabalInfo -> PackageIdentifier -> DebianVersion
debianVersion'' atoms i = parseDebianVersion' (maybe "" (\ n -> show n ++ ":") (Map.lookup (pkgName i) (view A.epochMap atoms)) ++ showVersion (pkgVersion i))

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel' {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = List.map (List.map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel' rel) = And [Or [Rel' rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . List.map Or $ sequence $ List.map (concat . List.map unOr . unAnd . canonical) $ rels

filterMissing :: Monad m => [[Relation]] -> CabalT m [[Relation]]
filterMissing rels =
    get >>= \ atoms -> return $
    List.filter (/= []) (List.map (List.filter (\ (Rel name _ _) -> not (Set.member name (view (A.debInfo . D.missingDependencies) atoms)))) rels)
