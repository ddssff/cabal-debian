{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Distribution.Version.Invert
    ( invertVersionRange
    , invertVersionIntervals
    ) where

#if MIN_VERSION_Cabal(1,24,0)
import Distribution.Version (invertVersionRange, invertVersionIntervals)
#else
import Distribution.Version (Version(Version, versionBranch, versionTags), VersionRange, fromVersionIntervals, asVersionIntervals, mkVersionIntervals,
                             LowerBound(LowerBound), UpperBound(UpperBound, NoUpperBound), Bound(InclusiveBound, ExclusiveBound))

-- | This function belongs in Cabal, see http://hackage.haskell.org/trac/hackage/ticket/935.
invertVersionRange :: VersionRange -> VersionRange
invertVersionRange = fromVersionIntervals . maybe (error "invertVersionRange") id . mkVersionIntervals . invertVersionIntervals . asVersionIntervals

invertVersionIntervals :: [(LowerBound, UpperBound)] -> [(LowerBound, UpperBound)]
invertVersionIntervals xs =
    case xs of
      [] -> [(lb0, NoUpperBound)]
      ((LowerBound (Version {versionBranch = [0], versionTags = []}) InclusiveBound, ub) : more) ->
          invertVersionIntervals' ub more
      ((lb, ub) : more) ->
          (lb0, invertLowerBound lb) : invertVersionIntervals' ub more
    where
      invertVersionIntervals' :: UpperBound -> [(LowerBound, UpperBound)] -> [(LowerBound, UpperBound)]
      invertVersionIntervals' NoUpperBound [] = []
      invertVersionIntervals' ub0 [] = [(invertUpperBound ub0, NoUpperBound)]
      invertVersionIntervals' ub0 [(lb, NoUpperBound)] = [(invertUpperBound ub0, invertLowerBound lb)]
      invertVersionIntervals' ub0 ((lb, ub1) : more) = (invertUpperBound ub0, invertLowerBound lb) : invertVersionIntervals' ub1 more

      invertLowerBound :: LowerBound -> UpperBound
      invertLowerBound (LowerBound v b) = UpperBound v (invertBound b)

      invertUpperBound :: UpperBound -> LowerBound
      invertUpperBound (UpperBound v b) = LowerBound v (invertBound b)
      invertUpperBound NoUpperBound = error "NoUpperBound: unexpected"

      invertBound :: Bound -> Bound
      invertBound ExclusiveBound = InclusiveBound
      invertBound InclusiveBound = ExclusiveBound

      lb0 :: LowerBound
      lb0 = LowerBound (Version {versionBranch = [0], versionTags = []}) InclusiveBound
#endif
