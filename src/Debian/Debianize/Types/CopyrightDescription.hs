-- | <https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/>
{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Debian.Debianize.Types.CopyrightDescription
    ( CopyrightDescription(..)
    , FilesDescription(..)
    , LicenseDescription(..)
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
    , license
    , comment
    , newCopyrightDescription
    , inputCopyrightDescription
    , parseCopyrightDescription
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Generics (Data, Typeable)
import Data.Lens.Template (makeLenses)
import Data.Maybe (isJust, catMaybes, fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (readFile)
import Debian.Control (Field'(Field), lookupP, Paragraph'(Paragraph), Control'(Control, unControl), parseControl)
import Debian.Orphans ()
import Debian.Pretty (PP(unPP), ppDisplay', ppPrint)
import Network.URI (URI, parseURI)
import Prelude hiding (init, init, log, log, unlines, readFile)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

data CopyrightDescription
    = CopyrightDescription
      { _format :: URI
      , _upstreamName :: Maybe Text
      , _upstreamContact :: Maybe Text
      , _source :: Maybe Text
      , _disclaimer :: Maybe Text
      , _summaryComment :: Maybe Text
      , _summaryLicense :: Maybe Text
      , _summaryCopyright :: Maybe Text
      , _filesAndLicenses :: [Either FilesDescription LicenseDescription]
      } deriving (Eq, Ord, Show, Data, Typeable)

data FilesDescription
    = FilesDescription
      { _filesPattern :: FilePath
      , _filesCopyright :: Text
      , _filesLicense :: Text
      , _filesComment :: Maybe Text
      } deriving (Eq, Ord, Show, Data, Typeable)

data LicenseDescription
    = LicenseDescription
      { _license :: Text
      , _comment :: Maybe Text
      } deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty (PP CopyrightDescription) where
    pPrint = ppPrint . toControlFile . unPP

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

inputCopyrightDescription :: MonadIO m => FilePath -> m (Either CopyrightDescription Text)
inputCopyrightDescription path =
    do t <- liftIO $ readFile path
       return $ either (\ _e -> Right t) (maybe (Right t) Left . parseCopyrightDescription . unControl) (parseControl "debian/copyright" t)

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
                   , _summaryLicense = fmap (\ (Field (_, x)) -> x) $ lookupP "License" hd
                   , _summaryCopyright = fmap (\ (Field (_, x)) -> x) $ lookupP "Copyright" hd
                   , _filesAndLicenses = catMaybes fnls
                   }
      _ -> Nothing
parseCopyrightDescription [] = Nothing

parseFilesOrLicense :: Paragraph' Text -> Maybe (Either FilesDescription LicenseDescription)
parseFilesOrLicense p =
    case (lookupP "Files" p, lookupP "Copyright" p, lookupP "License" p) of
      (Just (Field (_, files)),
       Just (Field (_, copyright)),
       Just (Field (_, license))) ->
          Just $ Left $ FilesDescription
                 { _filesPattern = unpack files
                 , _filesCopyright = copyright
                 , _filesLicense = license
                 , _filesComment = maybe Nothing (\ (Field (_, comment)) -> Just comment) (lookupP "Comment" p) }
      (Nothing,
       Nothing,
       Just (Field (_, license))) ->
          Just $ Right $ LicenseDescription
                 { _license = license
                 , _comment = maybe Nothing (\ (Field (_, comment)) -> Just comment) (lookupP "Comment" p) }
      _ -> Nothing

toControlFile :: CopyrightDescription -> Control' Text
toControlFile d =
    Control
    ( Paragraph
      ( [ Field ("Format", (" " <> ppDisplay' (_format d))) ] ++
        concat [ maybe [] ((: []) . Field . ("Upstream-Name",) . (" " <>) . ppDisplay') (_upstreamName d)
               , maybe [] ((: []) . Field . ("Upstream-Contact",) . (" " <>) . ppDisplay') (_upstreamContact d)
               , maybe [] ((: []) . Field . ("Source",) . (" " <>) . ppDisplay') (_source d)
               , maybe [] ((: []) . Field . ("Disclaimer",) . (" " <>) . ppDisplay') (_disclaimer d)
               , maybe [] ((: []) . Field . ("Comment",) . (" " <>) . ppDisplay') (_summaryComment d)
               , maybe [] ((: []) . Field . ("License",) . (" " <>) . ppDisplay') (_summaryLicense d)
               , maybe [] ((: []) . Field . ("Copyright",) . (" " <>) . ppDisplay') (_summaryCopyright d) ]) :
      map toParagraph (_filesAndLicenses d) )

toParagraph :: Either FilesDescription LicenseDescription -> Paragraph' Text
toParagraph (Left fd) =
    Paragraph $
      [ Field ("Files", pack (_filesPattern fd))
      , Field ("Copyright", _filesCopyright fd)
      , Field ("License", _filesLicense fd)
      ] ++
      maybe [] ((: []) . Field . ("Comment",) . ppDisplay') (_filesComment fd)
toParagraph (Right ld) = undefined

$(makeLenses [''CopyrightDescription, ''FilesDescription, ''LicenseDescription])
