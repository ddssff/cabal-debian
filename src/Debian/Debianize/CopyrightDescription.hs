-- | <https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/>
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Debian.Debianize.CopyrightDescription
    ( CopyrightDescription(..)
    , FilesOrLicenseDescription(..)
    , format
    , upstreamName
    , upstreamContact
    , source
    , disclaimer
    , summaryComment
    , summaryLicense
    , summaryCopyright
    , filesAndLicenses
    , filesPattern
    , filesCopyright
    , filesLicense
    , filesComment
    , Debian.Debianize.CopyrightDescription.license
    , comment
    , newCopyrightDescription
    , readCopyrightDescription
    , parseCopyrightDescription
    , defaultCopyrightDescription
    ) where

import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Generics (Data, Typeable)
import Data.Lens.Template (makeLenses)
import Data.List (dropWhileEnd)
import Data.Maybe (isJust, catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack, strip, unpack, null)
import Debian.Control (Field'(Field), lookupP, Paragraph'(Paragraph), Control'(Control, unControl), parseControl)
import Debian.Debianize.Prelude (readFileMaybe)
import Debian.Orphans ()
import Debian.Policy (License(..), readLicense, fromCabalLicense)
import Debian.Pretty (PP(PP, unPP), display', ppDisplay', ppPrint)
import Distribution.PackageDescription as Cabal (PackageDescription(licenseFiles, copyright, license))
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
      , _source :: Maybe Text
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
    pPrint (PP x@(CopyrightDescription {_summaryComment = Just t})) | x {_summaryComment = Nothing} == newCopyrightDescription = text (dropWhileEnd isSpace (unpack t) <> "\n")
    pPrint x = ppPrint . toControlFile . unPP $ x

newCopyrightDescription :: CopyrightDescription
newCopyrightDescription =
    CopyrightDescription
    { _format = fromJust $ parseURI "http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/"
    , _upstreamName = Nothing
    , _upstreamContact = Nothing
    , _source = Nothing
    , _disclaimer = Nothing
    , _summaryComment = Nothing
    , _summaryLicense = Nothing
    , _summaryCopyright = Nothing
    , _filesAndLicenses = [] }

-- | Try to read a CopyrightDescription from a file
readCopyrightDescription :: Text -> CopyrightDescription
readCopyrightDescription t =
    case parseControl "debian/copyright" t of
      Left _e -> newCopyrightDescription { _summaryComment = Just t }
      Right ctl -> case parseCopyrightDescription (unControl ctl) of
                     Just cpy -> cpy
                     Nothing -> newCopyrightDescription { _summaryComment = Just t }

parseCopyrightDescription :: [Paragraph' Text] -> Maybe CopyrightDescription
parseCopyrightDescription (hd : tl) =
    let (muri :: Maybe URI) = maybe Nothing (\ (Field (_, t)) -> parseURI . unpack $ t) (lookupP "Format" hd) in
    case (muri, map parseFilesOrLicense tl) of
      (Just uri, fnls) | all isJust fnls ->
          Just $ CopyrightDescription
                   { _format = uri
                   , _upstreamName = fmap (\ (Field (_, x)) -> x) $ lookupP "Upstream-Name" hd
                   , _upstreamContact = fmap (\ (Field (_, x)) -> x) $ lookupP "Upstream-Contact" hd
                   , _source = fmap (\ (Field (_, x)) -> x) $ lookupP "Source" hd
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
        maybe [] (\x -> [Field ("Source", " " <> x)]) (_source d) ++
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

defaultCopyrightDescription :: CopyrightDescription -> PackageDescription -> IO CopyrightDescription
defaultCopyrightDescription copyright0 pkgDesc = do
  licenseFiles <- mapM (\ path -> liftIO (readFileMaybe path) >>= \ text -> return (path, text))
                       (Cabal.licenseFiles pkgDesc)
  -- It is possible we might interpret the license file path
  -- as a license name, so I hang on to it here.
  let licenseFiles' = mapMaybe (\ (path, text) -> maybe Nothing (\ t -> Just (path, t)) text) licenseFiles
  return $ cabalToCopyrightDescription pkgDesc licenseFiles' copyright0

cabalToCopyrightDescription :: PackageDescription -> [(FilePath, Text)] -> CopyrightDescription -> CopyrightDescription
cabalToCopyrightDescription pkgDesc licenseFiles cdesc =
    let triples = zip3 (repeat (nothingIf (Text.null . strip) (pack (Cabal.copyright pkgDesc))))
                       (repeat (Cabal.license pkgDesc))
                       (case licenseFiles of
                          [] -> [Nothing]
                          xs -> map (Just. snd) xs)
        fnls = map (\ (copyrt, license, comment) ->
                         FilesDescription
                                {_filesPattern = "*"
                                , _filesCopyright = fromMaybe (pack "(No copyright field in cabal file)") copyrt
                                , _filesLicense = fromCabalLicense license
                                , _filesComment = comment }) triples in
     cdesc { _filesAndLicenses = fnls }

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x

$(makeLenses [''CopyrightDescription, ''FilesOrLicenseDescription])
