module Debian.Debianize.Changelog
    ( filterEntries
    , dropFutureEntries
    , findChangelogEntry
    , mergeChangelogEntries
    ) where

import Debian.Changes (ChangeLog(..), ChangeLogEntry(..))
import Debian.Version (DebianVersion)
import Prelude hiding (log)

-- | Apply a filter to the version numbers of the changelog entries.
filterEntries :: (DebianVersion -> Bool) -> ChangeLog -> ChangeLog
filterEntries p (ChangeLog entries) = ChangeLog $ filter (p . logVersion) $ entries

-- | Filter out versions newer than the given one.
dropFutureEntries :: DebianVersion -> ChangeLog -> ChangeLog
dropFutureEntries ver log = filterEntries (<= ver) log

-- | Find the log entry with the given version.
findChangelogEntry :: DebianVersion -> ChangeLog -> Maybe ChangeLogEntry
findChangelogEntry ver log =
    case filterEntries (== ver) log of
      ChangeLog [] -> Nothing
      ChangeLog [x] -> Just x
      _ -> error $ "Multiple version " ++ show ver ++ " changelog entries"

mergeChangelogEntries :: ChangeLogEntry -> ChangeLogEntry -> ChangeLogEntry
mergeChangelogEntries old new =
    old { logComments = logComments old ++ logComments new
        , logDate = logDate new }
