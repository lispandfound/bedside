-- | Southern-hemisphere seasons and what Bert the tree looks like in
-- each of them.
module Bedside.Season
  ( Season (..),
    BertVariant (..),
    seasonOfMonth,
    bertVariant,
  )
where

data Season = Summer | Autumn | Winter | Spring
  deriving (Eq, Show)

data BertVariant = Bloom | Leafless
  deriving (Eq, Show)

-- | Month number (1-12) to season, southern hemisphere.
seasonOfMonth :: Int -> Season
seasonOfMonth m
  | m == 12 || m <= 2 = Summer
  | m <= 5 = Autumn
  | m <= 8 = Winter
  | otherwise = Spring

bertVariant :: Season -> BertVariant
bertVariant Autumn = Leafless
bertVariant Winter = Leafless
bertVariant Spring = Bloom
bertVariant Summer = Bloom
