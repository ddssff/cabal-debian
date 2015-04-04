module Data.Maybe.Extended (
  module Data.Maybe,
  nothingIf
  ) where

import Data.Maybe

nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x
