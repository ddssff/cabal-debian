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
import Debian.Pretty (PP(PP, unPP))
import Debian.Relation (Relation(..), VersionReq(..), ArchitectureReq(..))
import Distribution.Compiler (CompilerId(..))
#if !MIN_VERSION_Cabal(1,18,0)
import Distribution.Compiler (CompilerFlavor(..))
#endif
import Distribution.License (License(..))
import Distribution.PackageDescription (PackageDescription(package), Executable(..))
import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Version (VersionRange(..), foldVersionRange')
import Language.Haskell.Extension (Language(..))
import Network.URI (URI)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text, hcat)

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
#if !MIN_VERSION_Cabal(1,21,1)
deriving instance Ord KnownExtension
deriving instance Ord Extension
#endif
deriving instance Eq Compiler
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

#if !MIN_VERSION_Cabal(1,18,0)
deriving instance Typeable License
deriving instance Data Version
deriving instance Data License
#endif

-- Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.
instance Pretty (PP License) where
    pPrint (PP (GPL _)) = text "GPL"
    pPrint (PP (LGPL _)) = text "LGPL"
    pPrint (PP BSD3) = text "BSD"
    pPrint (PP BSD4) = text "BSD-like"
    pPrint (PP PublicDomain) = text "Public Domain"
    pPrint (PP AllRightsReserved) = text "Proprietary"
    pPrint (PP OtherLicense) = text "Non-distributable"
    pPrint (PP MIT) = text "MIT"
    pPrint (PP (UnknownLicense _)) = text "Unknown"
    pPrint (PP x) = text (show x)

deriving instance Data NameAddr
deriving instance Typeable NameAddr
deriving instance Read NameAddr

-- This Pretty instance gives a string used to create a valid
-- changelog entry, it *must* have a name followed by an email address
-- in angle brackets.
instance Pretty (PP NameAddr) where
    pPrint (PP x) = text (fromMaybe (nameAddr_addr x) (nameAddr_name x) ++ " <" ++ nameAddr_addr x ++ ">")
    -- pPrint x = text (maybe (nameAddr_addr x) (\ n -> n ++ " <" ++ nameAddr_addr x ++ ">") (nameAddr_name x))

instance Pretty (PP [NameAddr]) where
    pPrint = hcat . intersperse (text ", ") . map (pPrint . PP) . unPP

instance Pretty (PP VersionRange) where
    pPrint (PP range) =
        foldVersionRange'
          (text "*")
          (\ v -> text "=" <> pPrint (PP v))
          (\ v -> text ">" <> pPrint (PP v))
          (\ v -> text "<" <> pPrint (PP v))
          (\ v -> text ">=" <> pPrint (PP v))
          (\ v -> text "<=" <> pPrint (PP v))
          (\ x _ -> text "=" <> pPrint (PP x) <> text ".*") -- not exactly right
          (\ x y -> text "(" <> x <> text " || " <> y <> text ")")
          (\ x y -> text "(" <> x <> text " && " <> y <> text ")")
          (\ x -> text "(" <> x <> text ")")
          range

instance Pretty (PP Version) where
    pPrint = text . showVersion . unPP

instance Pretty (PP URI) where
    pPrint = text . show . unPP
