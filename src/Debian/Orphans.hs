{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, StandaloneDeriving, CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Debian.Orphans where

import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.List (intersperse, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Pretty (PP(PP, unPP))
import Debian.Relation (ArchitectureReq(..), Relation(..), VersionReq(..))
import Distribution.Compiler (CompilerId(..))
import Distribution.Compiler (AbiTag(..))
import Distribution.License (License(..))
import Distribution.PackageDescription (Executable(..), PackageDescription(package))
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Version (cataVersionRange, normaliseVersionRange, VersionRange(..), VersionRangeF(..))
import Distribution.Version (Version)
import Language.Haskell.Extension (Language(..))
import Network.URI (URI)
#if MIN_VERSION_hsemail(2,0,0)
import Text.Parsec.Rfc2822 (NameAddr(..))
#else
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
#endif
import Text.PrettyPrint.HughesPJ (Doc)
import Text.PrettyPrint.HughesPJClass (hcat, text)
import Distribution.Pretty (Pretty(pretty))

deriving instance Typeable Compiler
deriving instance Typeable CompilerId

deriving instance Typeable AbiTag
deriving instance Data AbiTag
deriving instance Ord AbiTag

deriving instance Data Compiler
deriving instance Data CompilerId

deriving instance Ord Language
deriving instance Ord Compiler
deriving instance Ord NameAddr
deriving instance Ord License

instance Ord Executable where
    compare = compare `on` exeName

instance Ord PackageDescription where
    compare = compare `on` package

dropPrefix :: String -> String -> Maybe String
dropPrefix p s = if isPrefixOf p s then Just (drop (length p) s) else Nothing

deriving instance Data ArchitectureReq
deriving instance Data ChangeLog
deriving instance Data ChangeLogEntry
deriving instance Data Relation
deriving instance Data VersionReq

deriving instance Typeable ArchitectureReq
deriving instance Typeable ChangeLog
deriving instance Typeable ChangeLogEntry
deriving instance Typeable Relation
deriving instance Typeable VersionReq

deriving instance Ord ChangeLog
deriving instance Ord ChangeLogEntry

-- Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.
instance Pretty (PP License) where
    pretty (PP (GPL _)) = text "GPL"
    pretty (PP (LGPL _)) = text "LGPL"
    pretty (PP BSD3) = text "BSD"
    pretty (PP BSD4) = text "BSD-like"
    pretty (PP PublicDomain) = text "Public Domain"
    pretty (PP AllRightsReserved) = text "Proprietary"
    pretty (PP OtherLicense) = text "Non-distributable"
    pretty (PP MIT) = text "MIT"
    pretty (PP (UnknownLicense _)) = text "Unknown"
    pretty (PP x) = text (show x)

deriving instance Data NameAddr
deriving instance Typeable NameAddr
deriving instance Read NameAddr

-- This Pretty instance gives a string used to create a valid
-- changelog entry, it *must* have a name followed by an email address
-- in angle brackets.
instance Pretty (PP NameAddr) where
    pretty (PP x) = text (fromMaybe (nameAddr_addr x) (nameAddr_name x) ++ " <" ++ nameAddr_addr x ++ ">")
    -- pretty x = text (maybe (nameAddr_addr x) (\ n -> n ++ " <" ++ nameAddr_addr x ++ ">") (nameAddr_name x))

instance Pretty (PP [NameAddr]) where
    pretty = hcat . intersperse (text ", ") . map (pretty . PP) . unPP

instance Pretty (PP VersionRange) where
    pretty (PP range) = (cataVersionRange prettyRange . normaliseVersionRange) range

prettyRange :: VersionRangeF Text.PrettyPrint.HughesPJ.Doc -> Text.PrettyPrint.HughesPJ.Doc
prettyRange AnyVersionF                     = (text "*")
prettyRange (ThisVersionF v)                = text "=" <> pretty (PP v)
prettyRange (LaterVersionF v)               = text ">" <> pretty (PP v)
prettyRange (EarlierVersionF v)             = text "<" <> pretty (PP v)
prettyRange (OrLaterVersionF v)             = text ">=" <> pretty (PP v)
prettyRange (OrEarlierVersionF v)           = text "<=" <> pretty (PP v)
prettyRange (WildcardVersionF v)            = text "=" <> pretty (PP v) <> text ".*" -- not exactly right
prettyRange (MajorBoundVersionF v)          = text " >= " <> pretty (PP v) -- maybe this will do?
prettyRange (UnionVersionRangesF v1 v2)     = text "(" <> v1 <> text " || " <> v2 <> text ")"
prettyRange (IntersectVersionRangesF v1 v2) = text "(" <> v1 <> text " && " <> v2 <> text ")"
prettyRange (VersionRangeParensF v)         = text "(" <> v <> text ")"

instance Pretty (PP Version) where
    pretty = text . prettyShow . unPP

instance Pretty (PP URI) where
    pretty = text . show . unPP
