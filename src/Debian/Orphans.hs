{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, StandaloneDeriving, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Orphans where

import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.List (isPrefixOf, intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Version (Version(..), showVersion)
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Control (Field'(..))
import Debian.Pretty (Pretty(pretty), text, cat, empty)
import Debian.Relation (Relation(..), VersionReq(..), ArchitectureReq(..),
                        BinPkgName(..), SrcPkgName(..))
import Debian.Version (DebianVersion)
import Distribution.Compiler (CompilerId(..))
#if !MIN_VERSION_Cabal(1,18,0)
import Distribution.Compiler (CompilerFlavor(..))
#endif
import Distribution.License (License(..))
import Distribution.PackageDescription (PackageDescription(package), Executable(..))
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Version (VersionRange(..), foldVersionRange')
import Language.Haskell.Extension (Extension(..), KnownExtension(..), Language(..))
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))

deriving instance Typeable Compiler
deriving instance Typeable CompilerId

#if !MIN_VERSION_Cabal(1,18,0)
deriving instance Typeable CompilerFlavor
deriving instance Typeable Language
deriving instance Typeable Extension
deriving instance Typeable KnownExtension
#endif

deriving instance Data Compiler
deriving instance Data CompilerId

#if !MIN_VERSION_Cabal(1,18,0)
deriving instance Data Extension
deriving instance Data KnownExtension
deriving instance Data Language
deriving instance Data CompilerFlavor
#endif

deriving instance Ord Language
deriving instance Ord KnownExtension
deriving instance Ord Extension
deriving instance Eq Compiler
deriving instance Ord Compiler
deriving instance Ord NameAddr
deriving instance Ord License

instance Ord Executable where
    compare = compare `on` exeName

instance Ord PackageDescription where
    compare = compare `on` package

{-
instance Show (Control' String) where
    show _ = "<control file>"

instance Show ChangeLog where
    show _ = "<log entry>"
-}

deriving instance Read ArchitectureReq
deriving instance Read BinPkgName
deriving instance Read ChangeLog
deriving instance Read ChangeLogEntry
deriving instance Read Relation
deriving instance Read SrcPkgName
deriving instance Read VersionReq

deriving instance Show ChangeLog
deriving instance Show ChangeLogEntry

dropPrefix :: String -> String -> Maybe String
dropPrefix p s = if isPrefixOf p s then Just (drop (length p) s) else Nothing

deriving instance Data ArchitectureReq
deriving instance Data BinPkgName
deriving instance Data ChangeLog
deriving instance Data ChangeLogEntry
-- deriving instance Data NameAddr
deriving instance Data Relation
deriving instance Data SrcPkgName
deriving instance Data VersionReq

deriving instance Typeable ArchitectureReq
deriving instance Typeable BinPkgName
deriving instance Typeable ChangeLog
deriving instance Typeable ChangeLogEntry
-- deriving instance Typeable NameAddr
deriving instance Typeable Relation
deriving instance Typeable SrcPkgName
deriving instance Typeable VersionReq

deriving instance Ord ChangeLog
deriving instance Ord ChangeLogEntry

{-
instance Pretty SrcPkgName where
    pretty (SrcPkgName x) = pretty x

instance Pretty BinPkgName where
    pretty (BinPkgName x) = pretty x
-}

#if !MIN_VERSION_Cabal(1,18,0)
deriving instance Typeable License
deriving instance Data Version
deriving instance Data License
#endif

-- Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.
instance Pretty License where
    pretty (GPL _) = text "GPL"
    pretty (LGPL _) = text "LGPL"
    pretty BSD3 = text "BSD"
    pretty BSD4 = text "BSD-like"
    pretty PublicDomain = text "Public Domain"
    pretty AllRightsReserved = text "Proprietary"
    pretty OtherLicense = text "Non-distributable"
    pretty MIT = text "MIT"
    pretty (UnknownLicense _) = text "Unknown"
    pretty x = pretty (show x)

deriving instance Data NameAddr
deriving instance Typeable NameAddr
deriving instance Read NameAddr

-- This Pretty instance gives a string used to create a valid
-- changelog entry, it *must* have a name followed by an email address
-- in angle brackets.
instance Pretty NameAddr where
    pretty x = pretty (fromMaybe (nameAddr_addr x) (nameAddr_name x) ++ " <" ++ nameAddr_addr x ++ ">")
    -- pretty x = text (maybe (nameAddr_addr x) (\ n -> n ++ " <" ++ nameAddr_addr x ++ ">") (nameAddr_name x))

instance Pretty [NameAddr] where
    pretty = cat . intersperse (text ", ") . map pretty

instance Pretty (Maybe NameAddr) where
    pretty Nothing = empty
    pretty (Just x) = pretty x

deriving instance Show (Field' String)

instance Pretty VersionRange where
    pretty range =
        foldVersionRange'
          (text "*")
          (\ v -> text "=" <> pretty v)
          (\ v -> text ">" <> pretty v)
          (\ v -> text "<" <> pretty v)
          (\ v -> text ">=" <> pretty v)
          (\ v -> text "<=" <> pretty v)
          (\ x _ -> text "=" <> pretty x <> text ".*") -- not exactly right
          (\ x y -> text "(" <> x <> text " || " <> y <> text ")")
          (\ x y -> text "(" <> x <> text " && " <> y <> text ")")
          (\ x -> text "(" <> x <> text ")")
          range

instance Pretty Version where
    pretty = pretty . showVersion

instance Pretty DebianVersion where
    pretty = pretty . show

instance Pretty (Maybe SrcPkgName) where
    pretty Nothing = empty
    pretty (Just x) = pretty x

instance Pretty Bool where
    pretty = pretty . show
