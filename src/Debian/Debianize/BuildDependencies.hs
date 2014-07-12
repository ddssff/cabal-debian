-- | Compute the debianization of a cabal package.
{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
module Debian.Debianize.BuildDependencies
    ( debianBuildDeps
    , debianBuildDepsIndep
    ) where

import Control.Monad.State (MonadState(get))
import Control.Monad.Trans (MonadIO)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Lens.Lazy (access, getL)
import Data.List as List (filter, map, minimumBy, nub)
import Data.Map as Map (lookup, Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set (member)
import Data.Version (showVersion, Version)
import Debian.Debianize.Bundled (builtIn)
import Debian.Debianize.DebianName (mkPkgName, mkPkgName')
import Debian.Debianize.Monad as Monad (Atoms, DebT)
import qualified Debian.Debianize.Types as T (buildDepends, buildDependsIndep, debianNameMap, epochMap, execMap, extraLibMap, missingDependencies, noDocumentationLibrary, noProfilingLibrary)
import Debian.Debianize.Types.Atoms (EnvSet(dependOS), compilerFlavor, buildEnv)
import qualified Debian.Debianize.Types.BinaryDebDescription as B (PackageType(Development, Documentation, Profiling))
import Debian.Debianize.VersionSplits (packageRangesFromVersionSplits)
import Debian.Orphans ()
import Debian.Relation (BinPkgName(..), Relation(..), Relations, checkVersionReq)
import qualified Debian.Relation as D (BinPkgName(BinPkgName), Relation(..), Relations, VersionReq(EEQ, GRE, LTE, SGR, SLT))
import Debian.Version (DebianVersion, parseDebianVersion)
#if MIN_VERSION_Cabal(1,21,0)
import Distribution.Compiler (CompilerFlavor(GHC, GHCJS))
#else
import Distribution.Compiler (CompilerFlavor(GHC))
#endif
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(PackageName))
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription as Cabal (allBuildInfo, BuildInfo(..), BuildInfo(buildTools, extraLibs, pkgconfigDepends), Executable(..))
import qualified Distribution.PackageDescription as Cabal (PackageDescription(buildDepends, executables, package))
import Distribution.Version (anyVersion, asVersionIntervals, earlierVersion, foldVersionRange', fromVersionIntervals, intersectVersionRanges, isNoVersion, laterVersion, orEarlierVersion, orLaterVersion, toVersionIntervals, unionVersionRanges, VersionRange, withinVersion)
import Distribution.Version.Invert (invertVersionRange)
import Prelude hiding (init, log, map, unlines, unlines, writeFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

data Dependency_
  = BuildDepends Dependency
  | BuildTools Dependency
  | PkgConfigDepends Dependency
  | ExtraLibs Relations
    deriving (Eq, Show)

-- | In cabal a self dependency probably means the library is needed
-- while building the executables.  In debian it would mean that the
-- package needs an earlier version of itself to build, so we use this
-- to filter such dependencies out.
selfDependency :: PackageIdentifier -> Dependency_ -> Bool
selfDependency pkgId (BuildDepends (Dependency name _)) = name == pkgName pkgId
selfDependency _ _ = False

unboxDependency :: Dependency_ -> Maybe Dependency
unboxDependency (BuildDepends d) = Just d
unboxDependency (BuildTools d) = Just d
unboxDependency (PkgConfigDepends d) = Just d
unboxDependency (ExtraLibs _) = Nothing -- Dependency (PackageName d) anyVersion

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends :: Monad m => [Dependency] -> [Dependency] -> [Dependency] -> [String] -> DebT m [Dependency_]
allBuildDepends buildDepends' buildTools' pkgconfigDepends' extraLibs' =
    do atoms <- get
       return $ nub $ List.map BuildDepends buildDepends' ++
                      List.map BuildTools buildTools' ++
                      List.map PkgConfigDepends pkgconfigDepends' ++
                      [ExtraLibs (fixDeps atoms extraLibs')]
    where
      fixDeps :: Atoms -> [String] -> Relations
      fixDeps atoms xs =
          concatMap (\ cab -> fromMaybe [[D.Rel (D.BinPkgName ("lib" ++ cab ++ "-dev")) Nothing Nothing]]
                                        (Map.lookup cab (getL T.extraLibMap atoms))) xs

-- The haskell-cdbs package contains the hlibrary.mk file with
-- the rules for building haskell packages.
debianBuildDeps :: MonadIO m => PackageDescription -> DebT m D.Relations
debianBuildDeps pkgDesc =
    do deb <- get
       cDeps <- cabalDeps
       let bDeps = getL T.buildDepends deb
           prof = not $ getL T.noProfilingLibrary deb
       cfl <- access compilerFlavor
       let xs = nub $ [[D.Rel (D.BinPkgName "debhelper") (Just (D.GRE (parseDebianVersion ("7.0" :: String)))) Nothing],
                       [D.Rel (D.BinPkgName "haskell-devscripts") (Just (D.GRE (parseDebianVersion ("0.8" :: String)))) Nothing],
                       anyrel "cdbs"] ++
                      (case cfl of
                        GHC -> [anyrel "ghc"] ++ if prof then [anyrel "ghc-prof"] else []
#if MIN_VERSION_Cabal(1,21,0)
                        GHCJS -> [anyrel "ghcjs"]
#endif
                        x -> error ("Unsupported compiler flavor: " ++ show x)) ++
                      bDeps ++
                      cDeps
       filterMissing xs
    where
      cabalDeps =
          do deps <- allBuildDepends
                          (Cabal.buildDepends pkgDesc ++ concatMap (Cabal.targetBuildDepends . Cabal.buildInfo) (Cabal.executables pkgDesc))
                          (concatMap buildTools . allBuildInfo $ pkgDesc)
                          (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc)
                          (concatMap extraLibs . allBuildInfo $ pkgDesc)
             mapM buildDependencies (List.filter (not . selfDependency (Cabal.package pkgDesc)) deps) >>= return . concat

debianBuildDepsIndep :: MonadIO m => PackageDescription -> DebT m D.Relations
debianBuildDepsIndep pkgDesc =
    do doc <- get >>= return . not . getL T.noDocumentationLibrary
       bDeps <- get >>= return . getL T.buildDependsIndep
       cDeps <- cabalDeps
       let xs = if doc
                then nub $ [anyrel "ghc-doc"] ++ bDeps ++ concat cDeps
                else []
       filterMissing xs
    where
      cabalDeps =
          do deps <- allBuildDepends
                           (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                           (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc)
             let deps' = List.filter (not . selfDependency (Cabal.package pkgDesc)) deps
             mapM docDependencies deps'
{-
      cabalDeps deb =
          concat . List.map (\ x -> evalDebM (docDependencies x) deb)
                     $ List.filter (not . selfDependency (Cabal.package pkgDesc))
                     $ evalDebM
                         (allBuildDepends
                           (Cabal.buildDepends pkgDesc) (concatMap buildTools . allBuildInfo $ pkgDesc)
                           (concatMap pkgconfigDepends . allBuildInfo $ pkgDesc) (concatMap extraLibs . allBuildInfo $ pkgDesc))
                         deb
-}

-- | The documentation dependencies for a package include the
-- documentation package for any libraries which are build
-- dependencies, so we have access to all the cross references.
docDependencies :: MonadIO m => Dependency_ -> DebT m D.Relations
docDependencies (BuildDepends (Dependency name ranges)) = dependencies B.Documentation name ranges
docDependencies _ = return []

-- | The Debian build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.  Also the packages associated with extra libraries.
buildDependencies :: MonadIO m => Dependency_ -> DebT m D.Relations
buildDependencies (BuildDepends (Dependency name ranges)) =
    do hc <- access compilerFlavor
       dev <- dependencies B.Development name ranges
       prof <- if hc == GHC then dependencies B.Profiling name ranges else return []
       return $ dev ++ prof
buildDependencies dep@(ExtraLibs _) =
    do mp <- get >>= return . getL T.execMap
       return $ concat $ adapt mp dep
buildDependencies dep =
    case unboxDependency dep of
      Just (Dependency _name _ranges) ->
          do mp <- get >>= return . getL T.execMap
             return $ concat $ adapt mp dep
      Nothing ->
          return []

adapt :: Map.Map String Relations -> Dependency_ -> [Relations]
adapt mp (PkgConfigDepends (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg mp)
adapt mp (BuildTools (Dependency (PackageName pkg) _)) =
    maybe (aptFile pkg) (: []) (Map.lookup pkg mp)
adapt _flags (ExtraLibs x) = [x]
adapt _flags (BuildDepends (Dependency (PackageName pkg) _)) = [[[D.Rel (D.BinPkgName pkg) Nothing Nothing]]]

-- There are two reasons this may not work, or may work
-- incorrectly: (1) the build environment may be a different
-- distribution than the parent environment (the environment the
-- autobuilder was run from), so the packages in that
-- environment might have different names, and (2) the package
-- we are looking for may not be installed in the parent
-- environment.
aptFile :: String -> [Relations] -- Maybe would probably be more correct
aptFile pkg =
    unsafePerformIO $
    do ret <- readProcessWithExitCode "apt-file" ["-l", "search", pkg ++ ".pc"] ""
       return $ case ret of
                  (ExitSuccess, out, _) ->
                      case takeWhile (not . isSpace) out of
                        "" -> error $ "Unable to locate a debian package containing the build tool " ++ pkg ++
                                      ", try using --exec-map " ++ pkg ++ "=<debname> or execMap " ++ show pkg ++
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
dependencies :: MonadIO m => B.PackageType -> PackageName -> VersionRange -> DebT m Relations
dependencies typ name cabalRange =
    do atoms <- get
       hc <- access compilerFlavor
       -- Compute a list of alternative debian dependencies for
       -- satisfying a cabal dependency.  The only caveat is that
       -- we may need to distribute any "and" dependencies implied
       -- by a version range over these "or" dependences.
       let alts :: [(BinPkgName, VersionRange)]
           alts = case Map.lookup name (getL T.debianNameMap atoms) of
                    -- If there are no splits for this package just
                    -- return the single dependency for the package.
                    Nothing -> [(mkPkgName hc name typ, cabalRange')]
                    -- If there are splits create a list of (debian package name, VersionRange) pairs
                    Just splits' -> List.map (\ (n, r) -> (mkPkgName' hc n typ, r)) (packageRangesFromVersionSplits splits')
       mapM convert alts >>= mapM (doBundled typ name) . convert' . canonical . Or . catMaybes
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
      cabalRange' =
          foldVersionRange'
            anyVersion
            withinVersion  -- <- Here we are turning equals into wildcard
            laterVersion
            earlierVersion
            orLaterVersion
            orEarlierVersion
            (\ lb ub -> intersectVersionRanges (orLaterVersion lb) (earlierVersion ub))
            unionVersionRanges
            intersectVersionRanges
            id
            cabalRange
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
          -> [D.Relation]
          -> DebT m [D.Relation]
doBundled typ name rels =
    mapM doRel rels >>= return . concat
    where
      -- If a library is built into the compiler, this is the debian
      -- package name the compiler will conflict with.
      comp = D.Rel (compilerPackageName typ) Nothing Nothing
      doRel :: MonadIO m => D.Relation -> DebT m [D.Relation]
      doRel rel@(D.Rel dname req _) = do
        -- gver <- access ghcVersion
        hc <- access compilerFlavor
        splits <- access T.debianNameMap
        root <- access buildEnv >>= return . dependOS
        -- Look at what version of the package is provided by the compiler.
        atoms <- get
        -- What version of this package (if any) does the compiler provide?
        let pver = maybe Nothing (Just . debianVersion'' atoms name) (builtIn splits hc root name)
        -- The name this library would have if it was in the compiler conflicts list.
        let naiveDebianName = mkPkgName hc name typ
        return $ -- The compiler should appear in the build dependency
                 -- if it provides a suitable version of the library,
                 -- or if it conflicts with all versions of the
                 -- library (which, if pver is Nothing, will certainly
                 -- result in an error which needs to be corrected in
                 -- the packaging.)
                 (if isJust pver && (checkVersionReq req pver || (dname == naiveDebianName && conflictsWithHC naiveDebianName)) then [comp] else []) ++
                 -- The library package can satisfy the dependency if
                 -- the compiler doesn't provide a version, or if the
                 -- compiler doesn't conflict with the package's
                 -- debian name.
                 (if isNothing pver || dname /= naiveDebianName || not (conflictsWithHC naiveDebianName) then [rel] else [])
      compilerPackageName B.Documentation = D.BinPkgName "ghc-doc"
      compilerPackageName B.Profiling = D.BinPkgName "ghc-prof"
      compilerPackageName B.Development = D.BinPkgName "ghc"
      compilerPackageName _ = D.BinPkgName "ghc" -- whatevs

      -- FIXME: we are assuming here that ghc conflicts with all the
      -- library packages it provides but it no longer conflicts with
      -- libghc-cabal-dev.  We can now check these conflicts using the
      -- new functions in Bundled.
      conflictsWithHC (BinPkgName "libghc-cabal-dev") = False
      conflictsWithHC (BinPkgName "libghc-cabal-prof") = False
      conflictsWithHC (BinPkgName "libghc-cabal-doc") = False
      conflictsWithHC _ = True

{-
doBundled :: MonadIO m =>
             B.PackageType  -- Documentation, Profiling, Development...
          -> PackageName    -- Cabal package name
          -> [D.Relation]   -- Original set of debian dependencies
          -> DebT m [D.Relation] -- Modified debian dependencies accounting for the packages the compiler provides
doBundled hc typ name rels =
    concat <$> mapM doRel rels
    where
      doRel :: MonadIO m => D.Relation -> DebT m [D.Relation]
      doRel rel@(D.Rel dname req _) = do
        hc <- access
-}

-- Convert a cabal version to a debian version, adding an epoch number if requested
debianVersion' :: Monad m => PackageName -> Version -> DebT m DebianVersion
debianVersion' name v =
    do atoms <- get
       return $ parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (getL T.epochMap atoms)) ++ showVersion v)

debianVersion'' :: Atoms -> PackageName -> Version -> DebianVersion
debianVersion'' atoms name v = parseDebianVersion (maybe "" (\ n -> show n ++ ":") (Map.lookup name (getL T.epochMap atoms)) ++ showVersion v)

data Rels a = And {unAnd :: [Rels a]} | Or {unOr :: [Rels a]} | Rel' {unRel :: a} deriving Show

convert' :: Rels a -> [[a]]
convert' = List.map (List.map unRel . unOr) . unAnd . canonical

-- | return and of ors of rel
canonical :: Rels a -> Rels a
canonical (Rel' rel) = And [Or [Rel' rel]]
canonical (And rels) = And $ concatMap (unAnd . canonical) rels
canonical (Or rels) = And . List.map Or $ sequence $ List.map (concat . List.map unOr . unAnd . canonical) $ rels

filterMissing :: Monad m => [[Relation]] -> DebT m [[Relation]]
filterMissing rels =
    get >>= \ atoms -> return $
    List.filter (/= []) (List.map (List.filter (\ (Rel name _ _) -> not (Set.member name (getL T.missingDependencies atoms)))) rels)
