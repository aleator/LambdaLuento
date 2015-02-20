{-#LANGUAGE NoMonomorphismRestriction#-}
module Tally where
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

---------------
-- Tallying

newtype Tally = Tally [Count] deriving (Eq,Show,Ord)
instance Monoid Tally where
    mempty  = Tally []
    mappend (Tally as) (Tally bs) = Tally (zipM as bs)
instance Aeson.ToJSON Tally where
    toJSON (Tally as) = Aeson.toJSON as

newtype Count = Count Int deriving (Eq,Show,Ord)
instance Monoid Count where
    mempty  = Count 0
    mappend (Count a) (Count b) = Count (a+b)
instance Aeson.ToJSON Count where
    toJSON (Count n) = Aeson.toJSON n

one  = Count 1
zero = Count 0

sumRow = zipWith mappend

zipM [] [] = []
zipM [] (x:xs) = x:zipM [] xs
zipM (x:xs) [] = x:zipM xs []
zipM (x:xs) (y:ys) = x<>y:zipM xs ys

type ScoreCard = M.Map T.Text (M.Map BS.ByteString Tally)

toTally :: [Bool] -> Tally
toTally = Tally . map (\x -> if x then one else zero)

insert :: T.Text -> BS.ByteString -> Tally -> ScoreCard -> ScoreCard
insert qname cookie tally sc = M.insertWith' (<>) qname new sc
   where 
    new = M.singleton cookie tally
 
tally :: T.Text -> ScoreCard -> Tally
tally qname scorecard = mconcat $  M.elems 
    $ (M.findWithDefault mempty qname scorecard)

-- Calculate relative percentages of tally
relative :: Tally -> [Int]
relative (Tally ts) = let
    Count total = mconcat ts
    in map (\(Count i) -> round (100*fromIntegral i / fromIntegral total)) ts


---------------
