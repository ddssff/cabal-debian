-- | <https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/>
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
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
    , filesComment
    , license
    , comment
    -- * Builders
    , readCopyrightDescription
    , parseCopyrightDescription
    , defaultCopyrightDescription
    ) where

import Data.Char (isSpace)
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.Lens.Template (makeLenses)
import Data.List as List (dropWhileEnd, partition)
import Data.Maybe (isJust, catMaybes, fromJust, fromMaybe, listToMaybe)
import Data.Monoid ((<>), mempty)
import Data.Text as Text (Text, pack, strip, unpack, null, lines, unlines, dropWhileEnd)
import Debian.Control (Field'(Field), lookupP, Paragraph'(Paragraph), Control'(Control, unControl), parseControl)
import Debian.Debianize.Prelude (readFileMaybe)
import Debian.Orphans ()
import Debian.Policy (License(..), readLicense, fromCabalLicense)
import Debian.Pretty (PP(PP, unPP), display', ppDisplay', ppPrint)
import qualified Distribution.License as Cabal (License(UnknownLicense))
#if MIN_VERSION_Cabal(1,20,0)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFiles, copyright, license))
#else
import qualified Distribution.PackageDescription as Cabal (PackageDescription(licenseFile, copyright, license))
#endif
import Network.URI (URI, parseURI)
import Prelude hiding (init, init, log, log, unlines, readFile)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

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
      , _summaryLicense :: Maybe License
      , _summaryCopyright :: Maybe Text
      , _filesAndLicenses :: [FilesOrLicenseDescription]
      } deriving (Eq, Ord, Show, Data, Typeable)

data FilesOrLicenseDescription
    = FilesDescription
      { _filesPattern :: FilePath
      , _filesCopyright :: Text
      , _filesLicense :: License
      , _filesComment :: Maybe Text
      }
    | LicenseDescription
      { _license :: License
      , _comment :: Maybe Text
      } deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty (PP CopyrightDescription) where
    -- Special case encodes free format debian/copyright file
    pPrint (PP x@(CopyrightDescription {_summaryComment = Just t})) | x {_summaryComment = Nothing} == def = text (List.dropWhileEnd isSpace (unpack t) <> "\n")
    pPrint x = ppPrint . toControlFile . unPP $ x

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
    let (muri :: Maybe URI) = maybe Nothing (\ (Field (_, t)) -> parseURI . unpack $ t) (lookupP "Format" hd) in
    case (muri, map parseFilesOrLicense tl) of
      (Just uri, fnls) | all isJust fnls ->
          Just $ CopyrightDescription
                   { _format = uri
                   , _upstreamName = fmap (\ (Field (_, x)) -> x) $ lookupP "Upstream-Name" hd
                   , _upstreamContact = fmap (\ (Field (_, x)) -> x) $ lookupP "Upstream-Contact" hd
                   , _upstreamSource = fmap (\ (Field (_, x)) -> x) $ lookupP "Source" hd
                   , _disclaimer = fmap (\ (Field (_, x)) -> x) $ lookupP "Disclaimer" hd
                   , _summaryComment = fmap (\ (Field (_, x)) -> x) $ lookupP "Comment" hd
                   , _summaryLicense = fmap (\ (Field (_, x)) -> readLicense x) $ lookupP "License" hd
                   , _summaryCopyright = Nothing -- fmap (\ (Field (_, x)) -> x) $ lookupP "Copyright" hd
                   , _filesAndLicenses = catMaybes fnls
                   }
      _ -> Nothing
parseCopyrightDescription [] = Nothing

parseFilesOrLicense :: Paragraph' Text -> Maybe (FilesOrLicenseDescription)
parseFilesOrLicense p =
    case (lookupP "Files" p, lookupP "Copyright" p, lookupP "License" p) of
      (Just (Field (_, files)),
       Just (Field (_, copyright)),
       Just (Field (_, license))) ->
          Just $ FilesDescription
                 { _filesPattern = unpack files
                 , _filesCopyright = copyright
                 , _filesLicense = readLicense license
                 , _filesComment = maybe Nothing (\ (Field (_, comment)) -> Just comment) (lookupP "Comment" p) }
      (Nothing,
       Nothing,
       Just (Field (_, license))) ->
          Just $ LicenseDescription
                 { _license = readLicense license
                 , _comment = maybe Nothing (\ (Field (_, comment)) -> Just comment) (lookupP "Comment" p) }
      _ -> Nothing

toControlFile :: CopyrightDescription -> Control' Text
toControlFile d =
    Control
    ( Paragraph
      ( [ Field ("Format", (" " <> ppDisplay' (_format d))) ] ++
        maybe [] (\x -> [Field ("Upstream-Name", " " <> x)]) (_upstreamName d) ++
        maybe [] (\x -> [Field ("Upstream-Contact", " " <> x)]) (_upstreamContact d) ++
        maybe [] (\x -> [Field ("Source", " " <> x)]) (_upstreamSource d) ++
        maybe [] (\x -> [Field ("Disclaimer", " " <> x)]) (_disclaimer d) ++
        maybe [] (\x -> [Field ("License", " " <> display' x)]) (_summaryLicense d) ++
        maybe [] (\x -> [Field ("Copyright", " " <> x)]) (_summaryCopyright d) ++
        maybe [] (\x -> [Field ("Comment", " " <> x)]) (_summaryComment d)) :
      map toParagraph (_filesAndLicenses d) )

toParagraph :: FilesOrLicenseDescription -> Paragraph' Text
toParagraph fd@FilesDescription {} =
    Paragraph $
      [ Field ("Files", " " <> pack (_filesPattern fd))
      , Field ("Copyright", " " <> _filesCopyright fd)
      , Field ("License", " " <> display' (_filesLicense fd)) ] ++
      maybe [] (\ t -> [Field ("Comment", " " <> t)]) (_filesComment fd)
toParagraph ld@LicenseDescription {} =
    Paragraph $
      [ Field ("License", " " <> display' (_license ld)) ] ++
      maybe [] (\ t -> [Field ("Comment", " " <> t)]) (_comment ld)

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
                  [ FilesDescription
                    { _filesPattern = "*"
                    , _filesCopyright = fromMaybe (pack "(No copyright field in cabal file)") copyrt
                    , _filesLicense = license
                    , _filesComment = mempty }
                  , FilesDescription
                    { _filesPattern = "*/debian"
                    , _filesCopyright = "held by the contributors mentioned in debian/changelog"
                    , _filesLicense = license
                    , _filesComment = mempty } ] ++
                  case licenseCommentPairs of
                    [] -> []
                    [(_, comment)] ->
                        [ LicenseDescription
                          { _license = license
                          , _comment = comment } ]
                    _ -> map (\ (path, comment) -> LicenseDescription
                                                   { _license = fromCabalLicense (Cabal.UnknownLicense path)
                                                   , _comment = comment }) licenseCommentPairs }
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

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

-- | Replace empty lines with single dots
dots :: Text -> Text
dots = Text.unlines . map (\ line -> if Text.null line then "." else line) . map (Text.dropWhileEnd isSpace) . Text.lines

$(makeLenses [''CopyrightDescription, ''FilesOrLicenseDescription])
