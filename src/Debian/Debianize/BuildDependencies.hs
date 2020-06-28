-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, TupleSections #-}
module Debian.Debianize.BuildDependencies
    ( debianBuildDeps
    , debianBuildDepsIndep
    ) where


import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (MonadState(get))
import Control.Monad.Trans (MonadIO)
import Data.Char (isSpace, toLower)
import Data.Function (on)
import Data.List as List (filter, groupBy, map, minimumBy, nub, sortBy)
import Data.Map as Map (lookup, Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Set as Set (empty, fold, fromList, map, member, Set, singleton, toList, union)
import Debian.Debianize.Prelude
import Debian.Debianize.BasicInfo (compilerFlavor)
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
import Distribution.PackageDescription as Cabal (BuildInfo(..), BuildInfo(buildTools, extraLibs, pkgconfigDepends), Library(..), Executable(..), TestSuite(..), SetupBuildInfo(..), PackageDescription(setupBuildInfo))
import qualified Distribution.PackageDescription as Cabal (PackageDescription(library, executables, testSuites))
import Distribution.Pretty (prettyShow)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency(..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency(..))
import Distribution.Version (anyVersion, asVersionIntervals, fromVersionIntervals, intersectVersionRanges, invertVersionRange, isNoVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
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
#if MIN_VERSION_Cabal(3,0,0)
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion mempty
#else
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion
#endif

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Monad m => [BuildInfo] -> CabalT m [Dependency_]
allBuildDepends buildInfos =
    allBuildDepends'
      (mergeCabalDependencies $ concatMap Cabal.targetBuildDepends buildInfos)
      (mergeCabalDependencies $ mapMaybe convertLegacy $ concatMap buildTools buildInfos)
      (mergeCabalDependencies $ mapMaybe convertPkgconfig $  concatMap pkgconfigDepends buildInfos)
      (concatMap extraLibs buildInfos)
    where
      convertLegacy :: LegacyExeDependency -> Maybe Dependency
      convertLegacy = const Nothing
      convertPkgconfig :: PkgconfigDependency -> Maybe Dependency
      convertPkgconfig = const Nothing
      allBuildDepends' :: Monad m => [Dependency] -> [Dependency] -> [Dependency] -> [String] -> CabalT m [Dependency_]
      allBuildDepends' buildDepends' buildTools' pkgconfigDepends' extraLibs' =
          do atoms <- get
             return $ nub $ List.map BuildDepends buildDepends' ++
                            List.map BuildTools buildTools' ++
                            List.map PkgConfigDepends pkgconfigDepends' ++
                            [ExtraLibs (fixDeps atoms extraLibs')]

      fixDeps :: CabalInfo -> [String] -> Relations
      fixDeps atoms =
          concatMap (\ cab -> fromMaybe [[D.Rel (D.BinPkgName ("lib" ++ List.map toLower cab ++ "-dev")) Nothing Nothing]]
                                        (Map.lookup cab (view (A.debInfo . D.extraLibMap) atoms)))

setupBuildDepends :: SetupBuildInfo -> [Dependency_]
setupBuildDepends = List.map BuildDepends . setupDepends

-- | Take the intersection of all the dependencies on a given package name
mergeCabalDependencies :: [Dependency] -> [Dependency]
mergeCabalDependencies =
#if MIN_VERSION_Cabal(3,0,0)
    List.map (foldl1 (\ (Dependency name range1 _) (Dependency _ range2 _) -> Dependency name (intersectVersionRanges range1 range2) mempty)) . groupBy ((==) `on` dependencyPackage) . sortBy (compare `on` dependencyPackage)
#else
    List.map (foldl1 (\ (Dependency name range1) (Dependency _ range2) -> Dependency name (intersectVersionRanges range1 range2))) . groupBy ((==) `on` dependencyPackage) . sortBy (compare `on` dependencyPackage)
#endif
    where
#if MIN_VERSION_Cabal(3,0,0)
      dependencyPackage (Dependency x _ _) = x
#else
      dependencyPackage (Dependency x _) = x
#endif

-- The haskell-devscripts-minimal package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: (MonadIO m) => PackageDescription -> CabalT m D.Relations
debianBuildDeps pkgDesc =
    do hflavor <- use (A.debInfo . D.flags . compilerFlavor)
       prof <- not <$> use (A.debInfo . D.noProfilingLibrary)
       let hcPackageTypes :: CompilerFlavor -> Set B.PackageType
           hcPackageTypes GHC = fromList ([B.Development] <> if prof then [B.Profiling] else [])
           hcPackageTypes GHCJS = fromList [B.Development]
           hcPackageTypes hc = error $ "Unsupported compiler flavor: " ++ show hc

       let hcs = singleton hflavor -- vestigial
           hcTypePairs =
               fold union empty $
                  Set.map (\ hc' -> Set.map (hc',) $ hcPackageTypes hc') hcs
           setupDeps = concat . maybeToList . fmap setupBuildDepends . setupBuildInfo $ pkgDesc

       libDeps <- allBuildDepends (maybe [] (filter isBuildable . return . libBuildInfo) (Cabal.library pkgDesc))
       binDeps <- allBuildDepends (List.map buildInfo (filter isBuildable (Cabal.executables pkgDesc)))
       testDeps <- allBuildDepends (List.map testBuildInfo (filter isBuildable (Cabal.testSuites pkgDesc)))
       testsStatus <- use (A.debInfo . D.testsStatus)

       cDeps <- nub . concat . concat <$> sequence
            [ mapM (buildDependencies hcTypePairs) libDeps
            , mapM (buildDependencies hcTypePairs) binDeps
            , mapM (buildDependencies hcTypePairs) setupDeps
            , mapM (buildDependencies hcTypePairs) (if testsStatus /= D.TestsDisable then testDeps else [])
            ]

       bDeps <- use (A.debInfo . D.control . S.buildDepends)
       compat <- use (A.debInfo . D.compat)
       ghcdev <- liftIO $ compilerPackageName hflavor B.Development
       ghcprof <- liftIO $ compilerPackageName hflavor B.Profiling
       let ghcrel = if member GHC hcs then maybe [] ((: []) . anyrel') ghcdev else []
       let ghcrelprof = if prof then maybe [] ((: []) . anyrel') ghcprof else []
       let xs = nub $ [maybe [] (\ n -> [D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion' (show n)))) Nothing]) compat,
                       [D.Rel (D.BinPkgName "haskell-devscripts-minimal") Nothing Nothing,
                        D.Rel (D.BinPkgName "haskell-devscripts") (Just $ D.GRE $ parseDebianVersion' ("0.13" :: String)) Nothing],
                       anyrel "cdbs"] ++
                      (ghcrel ++ ghcrelprof) ++
                       bDeps ++
                       cDeps
       filterMissing xs
    where
      -- No point in installing profiling packages for the
      -- dependencies of binaries and test suites.  (I take it back,
      -- some executable builds fail if the profiling library isn't
      -- installed.)
#if 0
      hcPackageTypesBins :: CompilerFlavor -> Set B.PackageType
      hcPackageTypesBins GHC = singleton [B.Development, B.Profiling]

      hcPackageTypesTests :: CompilerFlavor -> Set B.PackageType
      hcPackageTypesTests GHC = singleton [B.Development, B.Profiling]
#endif

class IsBuildable e where
    isBuildable :: e -> Bool

instance IsBuildable Executable where
    isBuildable = buildable . buildInfo

instance IsBuildable BuildInfo where
    isBuildable = buildable

instance IsBuildable TestSuite where
    isBuildable = buildable . testBuildInfo

-- | Collect the dependencies required to build any packages that have
-- architecture "all".
debianBuildDepsIndep :: (MonadIO m) => PackageDescription -> CabalT m D.Relations
debianBuildDepsIndep pkgDesc =
    do hc <- use (A.debInfo . D.flags . compilerFlavor)
       let hcs = singleton hc -- vestigial
       doc <- not <$> use (A.debInfo . D.noDocumentationLibrary)
       bDeps <- use (A.debInfo . D.control . S.buildDependsIndep)
       libDeps <- allBuildDepends (maybe [] (return . libBuildInfo) (Cabal.library pkgDesc))
       cDeps <- mapM docDependencies libDeps
       ghcdoc <- liftIO $ compilerPackageName hc B.Documentation
       let hcdocdep = if doc && member GHC hcs then maybe [] ((: []) . anyrel') ghcdoc else []
       let xs = nub $ if doc && isJust (Cabal.library pkgDesc)
                      then hcdocdep ++ bDeps ++ concat cDeps
                      else []
       filterMissing xs

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have use to all the cross references.
docDependencies :: (MonadIO m) => Dependency_ -> CabalT m D.Relations
#if MIN_VERSION_Cabal(3,0,0)
docDependencies (BuildDepends (Dependency name ranges _)) =
#else
docDependencies (BuildDepends (Dependency name ranges)) =
#endif
    do hc <- use (A.debInfo . D.flags . compilerFlavor)
       let hcs = singleton hc -- vestigial
       omitProfDeps <- use (A.debInfo . D.omitProfVersionDeps)
       concat <$> mapM (\ hc' -> dependencies hc' B.Documentation name ranges omitProfDeps) (toList hcs)
docDependencies _ = return []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: (MonadIO m) => Set (CompilerFlavor, B.PackageType) -> Dependency_ -> CabalT m D.Relations
#if MIN_VERSION_Cabal(3,0,0)
buildDependencies hcTypePairs (BuildDepends (Dependency name ranges _)) =
    use (A.debInfo . D.omitProfVersionDeps) >>= \ omitProfDeps ->
    concat <$> mapM (\ (hc, typ) -> dependencies hc typ name ranges omitProfDeps) (toList hcTypePairs)
#else
buildDependencies hcTypePairs (BuildDepends (Dependency name ranges)) =
    use (A.debInfo . D.omitProfVersionDeps) >>= \ omitProfDeps ->
    concat <$> mapM (\ (hc, typ) -> dependencies hc typ name ranges omitProfDeps) (toList hcTypePairs)
#endif
buildDependencies _ dep@(ExtraLibs _) =
    do mp <- use (A.debInfo . D.execMap)
       return $ concat $ adapt mp dep
buildDependencies _ dep =
    case unboxDependency dep of
#if MIN_VERSION_Cabal(3,0,0)
      Just (Dependency _name _ranges _) ->
#else
      Just (Dependency _name _ranges) ->
#endif
          do mp <- view (A.debInfo . D.execMap) <$> get
             return $ concat $ adapt mp dep
      Nothing ->
          return []

adapt :: Map.Map String Relations -> Dependency_ -> [Relations]
#if MIN_VERSION_Cabal(3,0,0)
adapt mp (PkgConfigDepends (Dependency pkg _ _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt mp (BuildTools (Dependency pkg _ _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency pkg _ _)) = [[[D.Rel (D.BinPkgName (unPackageName pkg)) Nothing Nothing]]]
#else
adapt mp (PkgConfigDepends (Dependency pkg _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt mp (BuildTools (Dependency pkg _)) =
    maybe (aptFile (unPackageName pkg)) (: []) (Map.lookup (unPackageName pkg) mp)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency pkg _)) = [[[D.Rel (D.BinPkgName (unPackageName pkg)) Nothing Nothing]]]
#endif

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
                Just <$> (cataVersionRange rangeToRange . normaliseVersionRange) range'''
          where
            rangeToRange AnyVersionF                     = return $ Rel' (D.Rel dname Nothing Nothing)
            rangeToRange (ThisVersionF v)                = (debianVersion' name >=> \ dv -> return $ Rel' (D.Rel dname (Just (D.EEQ dv)) Nothing)) v
            rangeToRange (LaterVersionF v)               = (debianVersion' name >=> \ dv -> return $ Rel' (D.Rel dname (Just (D.SGR dv)) Nothing)) v
            rangeToRange (EarlierVersionF v)             = (debianVersion' name >=> \ dv -> return $ Rel' (D.Rel dname (Just (D.SLT dv)) Nothing)) v
            rangeToRange (OrLaterVersionF v)             = (debianVersion' name >=> \ dv -> return $ Rel' (D.Rel dname (Just (D.GRE dv)) Nothing)) v
            rangeToRange (OrEarlierVersionF v)           = (debianVersion' name >=> \ dv -> return $ Rel' (D.Rel dname (Just (D.LTE dv)) Nothing)) v
            rangeToRange (WildcardVersionF v)            = (\ x y -> debianVersion' name x >>= \ dvx ->
                                    debianVersion' name y >>= \ dvy ->
                                    return $ And [Rel' (D.Rel dname (Just (D.GRE dvx)) Nothing),
                                                  Rel' (D.Rel dname (Just (D.SLT dvy)) Nothing)]) v (wildcardUpperBound v)
            rangeToRange (MajorBoundVersionF v)          = (\ x y -> debianVersion' name x >>= \ dvx ->
                                    debianVersion' name y >>= \ dvy ->
                                    return $ And [Rel' (D.Rel dname (Just (D.GRE dvx)) Nothing),
                                                  Rel' (D.Rel dname (Just (D.SLT dvy)) Nothing)]) v (majorUpperBound v)
            rangeToRange (UnionVersionRangesF v1 v2)     = (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ Or [x', y']) v1 v2
            rangeToRange (IntersectVersionRangesF v1 v2) = (\ x y -> x >>= \ x' -> y >>= \ y' -> return $ And [x', y']) v1 v2
            rangeToRange (VersionRangeParensF v)         = v
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
                  | otherwise = (hyloVersionRange tweak projectVersionRange . normaliseVersionRange) cabalRange
      tweak (ThisVersionF v) = withinVersion v
      tweak vr = embedVersionRange vr
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
  hcname <- liftIO $ compilerPackageName hc typ
  concat <$> mapM (doRel hcname) rels
    where
      -- If a library is built into the compiler, this is the debian
      -- package name the compiler will conflict with.
      doRel :: MonadIO m => Maybe BinPkgName -> D.Relation -> CabalT m [D.Relation]
      doRel hcname rel@(D.Rel dname req _) = do
        let comp = maybe [] (\x -> [D.Rel x Nothing Nothing]) hcname
        -- gver <- use ghcVersion
        -- Look at what version of the package is provided by the compiler.
        atoms <- get
        -- What version of this package (if any) does the compiler provide?
        relInfo <- liftIO $ builtIn hc
        let pver = listToMaybe $ fmap (debianVersion'' atoms) (filter ((== name) . pkgName) relInfo)
        -- The name this library would have if it was in the compiler conflicts list.
        let naiveDebianName = mkPkgName hc name typ
        -- The compiler should appear in the build dependency
        -- if it provides a suitable version of the library,
        -- or if it conflicts with all versions of the
        -- library (which, if pver is Nothing, will certainly
        -- result in an error which needs to be corrected in
        -- the packaging.)
        let compilerDependency = if isJust pver && (checkVersionReq req pver || dname == naiveDebianName) then comp else []
        -- The library package can satisfy the dependency if
        -- the compiler doesn't provide a version, or if the
        -- compiler doesn't conflict with the package's
        -- debian name.
        let libraryDependency = if isNothing pver || dname /= naiveDebianName then [rel] else []
        -- Is the version number in the library dependency newer than
        -- the compiler version?  If so it should appear to its left,
        -- otherwise to its right.
        return $ case req of
                   Just (D.SLT lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   Just (D.LTE lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   Just (D.EEQ lver) | Just lver < pver -> compilerDependency ++ libraryDependency
                   _ -> libraryDependency ++ compilerDependency

-- Convert a cabal version to a debian version, adding an epoch number if requested
debianVersion' :: Monad m => PackageName -> Version -> CabalT m DebianVersion
debianVersion' name v =
    do atoms <- get
       return $ parseDebianVersion' (maybe "" (\ n -> show n ++ ":") (Map.lookup name (view A.epochMap atoms)) ++ prettyShow v)

debianVersion'' :: CabalInfo -> PackageIdentifier -> DebianVersion
debianVersion'' atoms i = parseDebianVersion' (maybe "" (\ n -> show n ++ ":") (Map.lookup (pkgName i) (view A.epochMap atoms)) ++ prettyShow (pkgVersion i))

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel' {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = List.map (List.map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel' rel) = And [Or [Rel' rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . List.map Or $ mapM (concatMap unOr . unAnd . canonical) rels

filterMissing :: Monad m => [[Relation]] -> CabalT m [[Relation]]
filterMissing rels =
    get >>= \ atoms -> return $
    List.filter (/= []) (List.map (List.filter (\ (Rel name _ _) -> not (Set.member name (view (A.debInfo . D.missingDependencies) atoms)))) rels)
