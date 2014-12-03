-- | Code pulled out of cabal-debian that straightforwardly implements
-- parts of the Debian policy manual, or other bits of Linux standards.
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings #-}
module Debian.Policy
    ( -- * Paths
      databaseDirectory
    , apacheLogDirectory
    , apacheErrorLog
    , apacheAccessLog
    , serverLogDirectory
    , serverAppLog
    , serverAccessLog
    , errorLogBaseName
    , appLogBaseName
    , accessLogBaseName
    -- * Installed packages
    , debianPackageVersion
    , getDebhelperCompatLevel
    , StandardsVersion(..)
    , getDebianStandardsVersion
    , parseStandardsVersion
    -- * Package fields
    , SourceFormat(..)
    , readSourceFormat
    , PackagePriority(..)
    , readPriority
    , PackageArchitectures(..)
    , parsePackageArchitectures
    , Section(..)
    , readSection
    , Area(..)
    , parseUploaders
    , parseMaintainer
    , getDebianMaintainer
    , haskellMaintainer
    , License(..)
    , fromCabalLicense
    , toCabalLicense
    , readLicense
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Arrow (second)
import Control.Monad (mplus)
import Data.Char (toLower, isSpace)
import Data.List (groupBy, intercalate)
import Data.Generics (Data, Typeable)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, strip)
import Debian.Debianize.Prelude (read')
import Debian.Pretty (PP(..))
import Debian.Relation (BinPkgName)
import Debian.Version (DebianVersion, parseDebianVersion, version)
import qualified Distribution.License as Cabal
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (readProcess)
import Text.Parsec (parse)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..), address)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Text.Read (readMaybe)

databaseDirectory :: BinPkgName -> String
databaseDirectory x = "/srv" </> show (pPrint . PP $ x)

apacheLogDirectory :: BinPkgName -> String
apacheLogDirectory x =  "/var/log/apache2/" ++ show (pPrint . PP $ x)

apacheErrorLog :: BinPkgName -> String
apacheErrorLog x = apacheLogDirectory x </> errorLogBaseName

apacheAccessLog :: BinPkgName -> String
apacheAccessLog x = apacheLogDirectory x </> accessLogBaseName

serverLogDirectory :: BinPkgName -> String
serverLogDirectory x = "/var/log/" ++ show (pPrint . PP $ x)

serverAppLog :: BinPkgName -> String
serverAppLog x = serverLogDirectory x </> appLogBaseName

serverAccessLog :: BinPkgName -> String
serverAccessLog x = serverLogDirectory x </> accessLogBaseName

appLogBaseName :: String
appLogBaseName = "app.log"

errorLogBaseName :: String
errorLogBaseName = "error.log"

accessLogBaseName :: String
accessLogBaseName = "access.log"

debianPackageVersion :: String -> IO (Maybe DebianVersion)
debianPackageVersion name =
    readProcess "dpkg-query" ["--show", "--showformat=${version}", name] "" >>=
    return . parseDebianVersion'
    where
      -- This should maybe be the real parseDebianVersion
      parseDebianVersion' "" = Nothing
      parseDebianVersion' s = Just (parseDebianVersion s)

-- | The version number of the installed debhelper package is the
-- highest acceptable value for compat in a debian/control file.  If
-- the package doesn't explicitly set an (acceptable) compat value we
-- can use the value returned by this function, assuming debhelper is
-- installed.
getDebhelperCompatLevel :: IO (Maybe Int)
getDebhelperCompatLevel =
    debianPackageVersion "debhelper" >>= return . fmap (read . takeWhile (/= '.') . version)

data StandardsVersion = StandardsVersion Int Int Int (Maybe Int) deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty (PP StandardsVersion) where
    pPrint (PP (StandardsVersion a b c (Just d))) = text (show a) <> text "." <> text (show b) <> text "." <> text (show c) <> text "." <> text (show d)
    pPrint (PP (StandardsVersion a b c Nothing)) = text (show a) <> text "." <> text (show b) <> text "." <> text (show c)

-- | Assumes debian-policy is installed
getDebianStandardsVersion :: IO (Maybe StandardsVersion)
getDebianStandardsVersion = debianPackageVersion "debian-policy" >>= return . fmap (parseStandardsVersion . version)

parseStandardsVersion :: String -> StandardsVersion
parseStandardsVersion s =
    case filter (/= ".") (groupBy (\ a b -> (a == '.') == (b == '.')) s) of
      (a : b : c : d : _) -> StandardsVersion (read' (error . ("StandardsVersion" ++) . show) a)
                                              (read' (error . ("StandardsVersion" ++) . show) b)
                                              (read' (error . ("StandardsVersion" ++) . show) c)
                                              (Just (read' (error . ("StandardsVersion" ++) . show) d))
      (a : b : c : _) -> StandardsVersion (read' (error . ("StandardsVersion" ++) . show) a)
                                          (read' (error . ("StandardsVersion" ++) . show) b)
                                          (read' (error . ("StandardsVersion" ++) . show) c) Nothing
      _ -> error $ "Invalid Standards-Version string: " ++ show s

data SourceFormat
    = Native3
    | Quilt3
    deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty (PP SourceFormat) where
    pPrint (PP Quilt3) = text "3.0 (quilt)\n"
    pPrint (PP Native3) = text "3.0 (native)\n"

readSourceFormat :: Text -> Either Text SourceFormat
readSourceFormat s =
    case () of
      _ | strip s == "3.0 (native)" -> Right Native3
      _ | strip s == "3.0 (quilt)" -> Right Quilt3
      _ -> Left $ "Invalid debian/source/format: " <> pack (show (strip s))

data PackagePriority
    = Required
    | Important
    | Standard
    | Optional
    | Extra
    deriving (Eq, Ord, Read, Show, Data, Typeable)

readPriority :: String -> PackagePriority
readPriority s =
    case unpack (strip (pack s)) of
      "required" -> Required
      "important" -> Important
      "standard" -> Standard
      "optional" -> Optional
      "extra" -> Extra
      x -> error $ "Invalid priority string: " ++ show x

instance Pretty (PP PackagePriority) where
    pPrint = text . map toLower . show . unPP

-- | The architectures for which a binary deb can be built.
data PackageArchitectures
    = All            -- ^ The package is architecture independenct
    | Any            -- ^ The package can be built for any architecture
    | Names [String] -- ^ The list of suitable architectures
    deriving (Read, Eq, Ord, Show, Data, Typeable)

instance Pretty (PP PackageArchitectures) where
    pPrint (PP All) = text "all"
    pPrint (PP Any) = text "any"
    pPrint (PP (Names xs)) = text $ intercalate " " xs

parsePackageArchitectures :: String -> PackageArchitectures
parsePackageArchitectures "all" = All
parsePackageArchitectures "any" = Any
parsePackageArchitectures s = error $ "FIXME: parsePackageArchitectures " ++ show s

data Section
    = MainSection String -- Equivalent to AreaSection Main s?
    | AreaSection Area String
    deriving (Read, Eq, Ord, Show, Data, Typeable)

readSection :: String -> Section
readSection s =
    case break (== '/') s of
      ("contrib", '/' : b) -> AreaSection Contrib (tail b)
      ("non-free", '/' : b) -> AreaSection NonFree (tail b)
      ("main", '/' : b) -> AreaSection Main (tail b)
      (a, '/' : _) -> error $ "readSection - unknown area: " ++ show a
      (a, _) -> MainSection a

instance Pretty (PP Section) where
    pPrint (PP (MainSection sec)) = text sec
    pPrint (PP (AreaSection area sec)) = pPrint (PP area) <> text "/" <> text sec

-- Is this really all that is allowed here?  Doesn't Ubuntu have different areas?
data Area
    = Main
    | Contrib
    | NonFree
    deriving (Read, Eq, Ord, Show, Data, Typeable)

instance Pretty (PP Area) where
    pPrint (PP Main) = text "main"
    pPrint (PP Contrib) = text "contrib"
    pPrint (PP NonFree) = text "non-free"

{-
Create a debian maintainer field from the environment variables:

  DEBFULLNAME (preferred) or NAME
  DEBEMAIL (preferred) or EMAIL

More work could be done to match dch, but this is sufficient for
now. Here is what the man page for dch has to say:

 If the environment variable DEBFULLNAME is set, this will be used for
 the maintainer full name; if not, then NAME will be checked.  If the
 environment variable DEBEMAIL is set, this will be used for the email
 address.  If this variable has the form "name <email>", then the
 maintainer name will also be taken from here if neither DEBFULLNAME
 nor NAME is set.  If this variable is not set, the same test is
 performed on the environment variable EMAIL.  Next, if the full name
 has still not been determined, then use getpwuid(3) to determine the
 name from the pass‐word file.  If this fails, use the previous
 changelog entry.  For the email address, if it has not been set from
 DEBEMAIL or EMAIL, then look in /etc/mailname, then attempt to build
 it from the username and FQDN, otherwise use the email address in the
 previous changelog entry.  In other words, it’s a good idea to set
 DEBEMAIL and DEBFULLNAME when using this script.

-}
getDebianMaintainer :: IO (Maybe NameAddr)
getDebianMaintainer =
    do env <- map (second decodeString) `fmap` getEnvironment
       return $ do fullname <- lookup "DEBFULLNAME" env `mplus` lookup "NAME" env
                   email    <- lookup "DEBEMAIL" env `mplus` lookup "EMAIL" env
                   either (const Nothing) Just (parseMaintainer (fullname ++ " <" ++ email ++ ">"))

haskellMaintainer :: NameAddr
haskellMaintainer =
    NameAddr { nameAddr_name = Just "Debian Haskell Group"
             , nameAddr_addr = "pkg-haskell-maintainers@lists.alioth.debian.org"}

-- | Turn the uploaders field of a cabal package into a list of
-- RFC2822 NameAddr values.
parseUploaders :: String -> Either String [NameAddr]
parseUploaders x =
    either (Left . show) fixNameAddrs (parse address "" ("Names: " ++ map fixWhite x ++ ";"))
    -- either (\ e -> error ("Failure parsing uploader list: " ++ show x ++ " -> " ++ show e)) id $ 
    where
      fixWhite c = if isSpace c then ' ' else c
      -- We absoletely need a name.
      fixNameAddrs :: [NameAddr] -> Either String [NameAddr]
      fixNameAddrs xs = case mapMaybe fixNameAddr xs of
                          [] -> Left ("No valid debian maintainers in " ++ show x)
                          xs' -> Right xs'
      fixNameAddr :: NameAddr -> Maybe NameAddr
      fixNameAddr y =
          case nameAddr_name y of
            Nothing -> Nothing
            _ -> Just y

-- | Parse a string containing a single NameAddr value.
parseMaintainer :: String -> Either String NameAddr
parseMaintainer x =
    case parseUploaders x of
      Left s -> Left s
      Right [y] -> Right y
      Right [] -> Left $ "Missing maintainer: " ++ show x
      Right ys -> Left $ "Too many maintainers: " ++ show ys

-- | Official Debian license types as described in
-- <https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/#license-specification>.
data License
    = Public_Domain	-- ^ No license required for any purpose; the work is not subject to copyright in any jurisdiction.
    | Apache		-- ^ Apache license 1.0, 2.0.
    | Artistic		-- ^ Artistic license 1.0, 2.0.
    | BSD_2_Clause	-- ^ Berkeley software distribution license, 2-clause version.
    | BSD_3_Clause	-- ^ Berkeley software distribution license, 3-clause version.
    | BSD_4_Clause	-- ^ Berkeley software distribution license, 4-clause version.
    | ISC		-- ^ Internet Software Consortium, sometimes also known as the OpenBSD License.
    | CC_BY		-- ^ Creative Commons Attribution license 1.0, 2.0, 2.5, 3.0.
    | CC_BY_SA		-- ^ Creative Commons Attribution Share Alike license 1.0, 2.0, 2.5, 3.0.
    | CC_BY_ND		-- ^ Creative Commons Attribution No Derivatives license 1.0, 2.0, 2.5, 3.0.
    | CC_BY_NC		-- ^ Creative Commons Attribution Non-Commercial license 1.0, 2.0, 2.5, 3.0.
    | CC_BY_NC_SA	-- ^ Creative Commons Attribution Non-Commercial Share Alike license 1.0, 2.0, 2.5, 3.0.
    | CC_BY_NC_ND	-- ^ Creative Commons Attribution Non-Commercial No Derivatives license 1.0, 2.0, 2.5, 3.0.
    | CC0		-- ^ Creative Commons Zero 1.0 Universal. Omit "Universal" from the license version when forming the short name.
    | CDDL		-- ^ Common Development and Distribution License 1.0.
    | CPL		-- ^ IBM Common Public License.
    | EFL		-- ^ The Eiffel Forum License 1.0, 2.0.
    | Expat		-- ^ The Expat license.
    | GPL		-- ^ GNU General Public License 1.0, 2.0, 3.0.
    | LGPL		-- ^ GNU Lesser General Public License 2.1, 3.0, or GNU Library General Public License 2.0.
    | GFDL		-- ^ GNU Free Documentation License 1.0, 1.1, 1.2, or 1.3. Use GFDL-NIV instead if there are no Front-Cover or Back-Cover Texts or Invariant Sections.
    | GFDL_NIV		-- ^ GNU Free Documentation License, with no Front-Cover or Back-Cover Texts or Invariant Sections. Use the same version numbers as GFDL.
    | LPPL		-- ^ LaTeX Project Public License 1.0, 1.1, 1.2, 1.3c.
    | MPL		-- ^ Mozilla Public License 1.1.
    | Perl		-- ^ erl license (use "GPL-1+ or Artistic-1" instead)
    | Python		-- ^ Python license 2.0.
    | QPL		-- ^ Q Public License 1.0.
    | W3C		-- ^ W3C Software License For more information, consult the W3C Intellectual Rights FAQ.
    | Zlib		-- ^ zlib/libpng license.
    | Zope		-- ^ Zope Public License 1.0, 1.1, 2.0, 2.1.
    | OtherLicense String
			-- ^ A license name associated with the subsequent text of the License: field or in
			-- a Files paragraph of the same debian/copyright file, or in a License: paragraph.
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- We need a license parse function that converts these strings back
-- into License values.
instance Pretty License where
    pPrint Public_Domain = text "public-domain"
    pPrint Apache = text "Apache"
    pPrint Artistic = text "Artistic"
    pPrint BSD_2_Clause = text "BSD2"
    pPrint BSD_3_Clause = text "BSD3"
    pPrint BSD_4_Clause = text "BSD4"
    pPrint ISC = text "ISC"
    pPrint CC_BY = text "CC-BY"
    pPrint CC_BY_SA = text "CC-BY-SA"
    pPrint CC_BY_ND = text "CC-BY-ND"
    pPrint CC_BY_NC = text "CC-BY-NC"
    pPrint CC_BY_NC_SA = text "CC-BY-NC-SA"
    pPrint CC_BY_NC_ND = text "CC-BY-NC-ND"
    pPrint CC0 = text "CC0"
    pPrint CDDL = text "CDDL"
    pPrint CPL = text "CPL"
    pPrint EFL = text "EFL"
    pPrint Expat = text "Expat"
    pPrint GPL = text "GPL"
    pPrint LGPL = text "LGPL"
    pPrint GFDL = text "GFDL"
    pPrint GFDL_NIV = text "GFDL-NIV"
    pPrint LPPL = text "LPPL"
    pPrint MPL = text "MPL"
    pPrint Perl = text "Perl"
    pPrint Python = text "Python"
    pPrint QPL = text "QPL"
    pPrint W3C = text "W3C"
    pPrint Zlib = text "Zlib"
    pPrint Zope = text "Zope"
    pPrint (OtherLicense s) = text s

fromCabalLicense :: Cabal.License -> License
fromCabalLicense x =
    case x of
      Cabal.GPL mver -> GPL -- FIXME - what about the version number?  same below
      Cabal.AGPL mver -> OtherLicense (show x)
      Cabal.LGPL mver -> LGPL
      Cabal.BSD2 -> BSD_2_Clause
      Cabal.BSD3 -> BSD_3_Clause
      Cabal.BSD4 -> BSD_4_Clause
      Cabal.MIT -> OtherLicense (show x)
      Cabal.MPL ver -> MPL
      Cabal.Apache mver -> Apache
      Cabal.PublicDomain -> Public_Domain
      Cabal.AllRightsReserved -> OtherLicense "AllRightsReserved"
      Cabal.UnspecifiedLicense -> OtherLicense (show x)
      Cabal.OtherLicense -> OtherLicense (show x)
      Cabal.UnknownLicense s -> OtherLicense (show x)

toCabalLicense :: License -> Cabal.License
toCabalLicense x =
    -- This needs to be finished
    case x of
      BSD_2_Clause -> Cabal.BSD2
      BSD_3_Clause -> Cabal.BSD3
      BSD_4_Clause -> Cabal.BSD4
      OtherLicense s -> Cabal.UnknownLicense s
      _ -> Cabal.UnknownLicense (show x)

invalidLicense :: Text -> License
invalidLicense = OtherLicense . unpack

-- | I think we need an actual parser for license names.
readLicense :: Text -> License
readLicense t = let s = unpack (strip t) in fromMaybe (invalidLicense t) (readMaybe s)
