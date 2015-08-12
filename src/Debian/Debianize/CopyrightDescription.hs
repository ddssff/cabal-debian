-- | <https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/>
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TupleSections, LambdaCase #-}
module Debian.Debianize.CopyrightDescription
    ( CopyrightDescription(..)
    , FilesOrLicenseDescription(..)
    -- * Lenses
    , format
    , upstreamName
    , upstreamContact
    , upstreamSource
    , disclaimer
    , summaryComment
    , summaryLicense
    , summaryCopyright
    , filesAndLicenses
    , filesPattern
    , filesCopyright
    , filesLicense
    , filesLicenseText
    , filesComment
    , license
    , licenseText
    , comment
    -- * Builders
    , readCopyrightDescription
    , parseCopyrightDescription
    , defaultCopyrightDescription
    ) where

import Data.Char (isSpace)
import Data.Default (Default(def))
import Data.Either (lefts, rights)
import Data.Generics (Data, Typeable)
import Control.Lens.TH (makeLenses)
import Data.List as List (dropWhileEnd, partition)
import Data.Maybe.Extended (isJust, catMaybes, fromJust, fromMaybe, listToMaybe, nothingIf)
import Data.Monoid ((<>), mempty)
import Data.Text as Text (Text, pack, strip, unpack, null, lines, unlines, dropWhileEnd)
import Debian.Control (Field'(Field), fieldValue, Paragraph'(Paragraph), Control'(Control, unControl), parseControl)
import Debian.Debianize.Prelude (readFileMaybe)
import Debian.Orphans ()
import Debian.Policy (License(..), readLicense, fromCabalLicense)
import Debian.Pretty (prettyText, ppText)
import Debug.Trace
import qualified Distribution.License as Cabal (License(UnknownLicense))
import qualified Distribution.Package as Cabal
#if MIN_VERSION_Cabal(1,20,0)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFiles, copyright, license, package, maintainer))
#else
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, copyright, license, package, maintainer))
#endif
import Network.URI (URI, parseURI)
import Prelude hiding (init, init, log, log, unlines, readFile)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

unPackageName :: Cabal.PackageName -> String
unPackageName (Cabal.PackageName x) = x

-- | Description of the machine readable debian/copyright file.  A
-- special case is used to represeent the old style free format file -
-- if the value is equal to newCopyrightDescription except for the
-- field _summaryComment, the text in _summaryComment is the copyright
-- file.
data CopyrightDescription
    = CopyrightDescription
      { _format :: URI
      , _upstreamName :: Maybe Text
      , _upstreamContact :: Maybe Text
      , _upstreamSource :: Maybe Text
      , _disclaimer :: Maybe Text
      , _summaryComment :: Maybe Text
      , _summaryLicense :: Maybe (License, Maybe Text)
      , _summaryCopyright :: Maybe Text
      , _filesAndLicenses :: [FilesOrLicenseDescription]
      } deriving (Eq, Ord, Show, Data, Typeable)

data FilesOrLicenseDescription
    = FilesDescription
      { _filesPattern :: FilePath
      , _filesCopyright :: Text
      , _filesLicense :: License
      , _filesLicenseText :: Maybe Text
      , _filesComment :: Maybe Text
      }
    | LicenseDescription
      { _license :: License
      , _licenseText :: Maybe Text
      , _comment :: Maybe Text
      } deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty CopyrightDescription where
    -- Special case encodes free format debian/copyright file
    pPrint x@(CopyrightDescription {_summaryComment = Just t}) | x {_summaryComment = Nothing} == def = text (List.dropWhileEnd isSpace (unpack t) <> "\n")
    pPrint x = pPrint . toControlFile $ x

instance Default CopyrightDescription where
    def = CopyrightDescription
          { _format = fromJust $ parseURI "http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/"
          , _upstreamName = Nothing
          , _upstreamContact = Nothing
          , _upstreamSource = Nothing
          , _disclaimer = Nothing
          , _summaryComment = Nothing
          , _summaryLicense = Nothing
          , _summaryCopyright = Nothing
          , _filesAndLicenses = [] }

-- | Read a 'CopyrightDescription' from the text one might obtain from
-- a @debian/copyright@ file.
readCopyrightDescription :: Text -> CopyrightDescription
readCopyrightDescription t =
    case parseControl "debian/copyright" t of
      Left _e -> def { _summaryComment = Just t }
      Right ctl -> case parseCopyrightDescription (unControl ctl) of
                     Just cpy -> cpy
                     Nothing -> def { _summaryComment = Just t }

-- | Try to parse a structured copyright file
parseCopyrightDescription :: [Paragraph' Text] -> Maybe CopyrightDescription
parseCopyrightDescription (hd : tl) =
    let (muri :: Either (Paragraph' Text) URI) = maybe (Left hd) Right (maybe Nothing (parseURI . unpack) (fieldValue "Format" hd)) in
    case (muri, map parseFilesOrLicense tl) of
      (Right uri, fnls) | all (either (const False) (const True)) fnls ->
          Just $ CopyrightDescription
                   { _format = uri
                   , _upstreamName = fieldValue "Upstream-Name" hd
                   , _upstreamContact = fieldValue "Upstream-Contact" hd
                   , _upstreamSource = fieldValue "Source" hd
                   , _disclaimer = fieldValue "Disclaimer" hd
                   , _summaryComment = fieldValue "Comment" hd
                   , _summaryLicense = fmap readLicenseField (fieldValue "License" hd)
                   , _summaryCopyright = Nothing -- fieldValue "Copyright" hd
                   , _filesAndLicenses = rights fnls
                   }
      (_, fnls) -> trace ("Not a parsable copyright file: " ++ show (lefts [muri] ++ lefts fnls)) Nothing
parseCopyrightDescription [] = Nothing

readLicenseField :: Text -> (License, Maybe Text)
readLicenseField v
    | length lns > 1
    = (readLicense firstLine, Just otherLines)
    | otherwise
    = (readLicense v, Nothing)
  where
    lns = Text.lines v
    firstLine = head lns
    otherLines = Text.unlines (tail lns)

parseFilesOrLicense :: Paragraph' Text -> Either (Paragraph' Text) (FilesOrLicenseDescription)
parseFilesOrLicense p =
    case (fieldValue "Files" p, fieldValue "Copyright" p, fieldValue "License" p) of
      (Just files,
       Just copyright,
       Just license) ->
          let (l,t) = readLicenseField license
          in Right $ FilesDescription
                    { _filesPattern = unpack files
                    , _filesCopyright = copyright
                    , _filesLicense = l
                    , _filesLicenseText = t
                    , _filesComment = fieldValue "Comment" p }
      (Nothing,
       Nothing,
       Just license) ->
          let (l,t) = readLicenseField license
          in Right $ LicenseDescription
                    { _license = l
                    , _licenseText = t
                    , _comment = fieldValue "Comment" p }
      _ -> Left p

toControlFile :: CopyrightDescription -> Control' Text
toControlFile d =
    Control
    ( Paragraph
      ( [ Field ("Format", (" " <> ppText (_format d))) ] ++
        maybe [] (\x -> [Field ("Upstream-Name", " " <> x)]) (_upstreamName d) ++
        maybe [] (\x -> [Field ("Upstream-Contact", " " <> x)]) (_upstreamContact d) ++
        maybe [] (\x -> [Field ("Source", " " <> x)]) (_upstreamSource d) ++
        maybe [] (\x -> [Field ("Disclaimer", " " <> x)]) (_disclaimer d) ++
        maybe [] (\(x,t) -> [toLicenseField x t]) (_summaryLicense d) ++
        maybe [] (\x -> [Field ("Copyright", " " <> x)]) (_summaryCopyright d) ++
        maybe [] (\x -> [Field ("Comment", " " <> x)]) (_summaryComment d)) :
      map toParagraph (_filesAndLicenses d) )

toParagraph :: FilesOrLicenseDescription -> Paragraph' Text
toParagraph fd@FilesDescription {} =
    Paragraph $
      [ Field ("Files", " " <> pack (_filesPattern fd))
      , Field ("Copyright", " " <> _filesCopyright fd)
      , toLicenseField (_filesLicense fd) (_filesLicenseText fd)
      ] ++
      maybe [] (\ t -> [Field ("Comment", " " <> t)]) (_filesComment fd)
toParagraph ld@LicenseDescription {} =
    Paragraph $
      [ toLicenseField (_license ld) (_licenseText ld)
      ] ++
      maybe [] (\ t -> [Field ("Comment", " " <> t)]) (_comment ld)

toLicenseField :: License -> Maybe Text -> Field' Text
toLicenseField l t =
    Field ("License", " " <> prettyText l <> maybe mempty (Text.pack "\n" <>) t)


sourceDefaultFilesDescription :: Maybe Text -> License -> FilesOrLicenseDescription
sourceDefaultFilesDescription copyrt license =
  FilesDescription {
    _filesPattern = "*"
  , _filesCopyright = fromMaybe "(No copyright field in cabal file)" copyrt
  , _filesLicense = license
  , _filesLicenseText = mempty
  , _filesComment = mempty
  }



debianDefaultFilesDescription :: License -> FilesOrLicenseDescription
debianDefaultFilesDescription license =
  FilesDescription {
    _filesPattern = "debian/*"
  , _filesCopyright = "held by the contributors mentioned in debian/changelog"
  , _filesLicense = license
  , _filesLicenseText = mempty
  , _filesComment = mempty
  }

defaultLicenseDescriptions ::
    License -> [(FilePath, Maybe Text)] -> [FilesOrLicenseDescription]
defaultLicenseDescriptions license = \case
    []         -> []
    [(_, txt)] -> [LicenseDescription license txt Nothing]
    pairs      -> map mkLicenseDescription pairs
  where
    mkLicenseDescription (path, txt) =
      LicenseDescription {
          _license = fromCabalLicense (Cabal.UnknownLicense path)
        , _licenseText = txt
        , _comment = mempty
        }

-- | Infer a 'CopyrightDescription' from a Cabal package description.
-- This will try to read any copyright files listed in the cabal
-- configuration.  Inputs include the license field from the cabal
-- file, the contents of the license files mentioned there, and the
-- provided @copyright0@ value.
defaultCopyrightDescription :: Cabal.PackageDescription -> IO CopyrightDescription
defaultCopyrightDescription pkgDesc = do
#if MIN_VERSION_Cabal(1,20,0)
  let (debianCopyrightPath, otherLicensePaths) = partition (== "debian/copyright") (Cabal.licenseFiles pkgDesc)
#else
  let (debianCopyrightPath, otherLicensePaths) = partition (== "debian/copyright") [Cabal.licenseFile pkgDesc]
#endif
      license = fromCabalLicense $ Cabal.license pkgDesc
      pkgname = unPackageName . Cabal.pkgName . Cabal.package $ pkgDesc
      maintainer = Cabal.maintainer $ pkgDesc
  -- This is an @Nothing@ unless debian/copyright is (for some
  -- reason) mentioned in the cabal file.
  debianCopyrightText <- mapM readFileMaybe debianCopyrightPath >>= return . listToMaybe . catMaybes
  licenseCommentPairs <- mapM readFileMaybe otherLicensePaths >>= return . filter (isJust . snd) . zip otherLicensePaths
  return $ case debianCopyrightText of
    Just t ->
        def { _summaryComment = Just t }
    Nothing ->
        -- All we have is the name of the license
        let copyrt = fmap dots $ nothingIf (Text.null . strip) (pack (Cabal.copyright pkgDesc)) in
        def { _filesAndLicenses =
                  [ sourceDefaultFilesDescription copyrt license,
                    debianDefaultFilesDescription license ] ++
                  defaultLicenseDescriptions license licenseCommentPairs
            , _upstreamName = Just . pack $ pkgname
            , _upstreamSource = Just . pack $ "https://hackage.haskell.org/package/" ++ pkgname
            , _upstreamContact = nothingIf Text.null (pack maintainer)
            }

{-
  -- We don't really have a way to associate licenses with
  -- file patterns, so we will just cover some simple cases,
  -- a single license, no license, etc.
  -- It is possible we might interpret the license file path
  -- as a license name, so I hang on to it here.
  return $ cabalToCopyrightDescription pkgDesc licenseComments (maybe def readCopyrightDescription debianCopyrightText)
    where
      cabalToCopyrightDescription :: Cabal.PackageDescription -> [Maybe Text] -> CopyrightDescription -> CopyrightDescription
      cabalToCopyrightDescription pkgDesc licenseComments copyright0 =
          let copyrt = fmap dots $ nothingIf (Text.null . strip) (pack (Cabal.copyright pkgDesc))
              license = Cabal.license pkgDesc in
          copyright0 { _filesAndLicenses =
                           map (\ comment ->
                                    FilesDescription
                                    { _filesPattern = "*"
                                    , _filesCopyright = fromMaybe (pack "(No copyright field in cabal file)") copyrt
                                    , _filesLicense = fromCabalLicense license
                                    , _filesComment = comment }) licenseComments }
-}

-- | Replace empty lines with single dots
dots :: Text -> Text
dots = Text.unlines . map (\ line -> if Text.null line then "." else line) . map (Text.dropWhileEnd isSpace) . Text.lines

$(makeLenses ''CopyrightDescription)
$(makeLenses ''FilesOrLicenseDescription)
