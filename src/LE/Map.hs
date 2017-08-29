module LE.Map where

import Protolude

import qualified Data.Map as M
import qualified Data.Sequence as Q

import LE.Types

type SeqMap k v = M.Map k (Q.Seq v)
type MapView k v = ((k, Q.Seq v), SeqMap k v)

deleteFindBidAskSpread :: OrderBook -> (Maybe Bid, Maybe Ask, OrderBook)
deleteFindBidAskSpread book@OrderBook{..} =
  let (mBid, deletedBids) = deleteFindMax _book_bids
      (mAsk, deletedAsks) = deleteFindMin _book_asks
      newBook = book {
        _book_bids = deletedBids,
        _book_asks = deletedAsks
        }
  in (mBid, mAsk, newBook)

deleteFindMin = deleteFindFirstSeqMap M.minViewWithKey
deleteFindMax = deleteFindFirstSeqMap M.maxViewWithKey
deleteFindEq k x = deleteFindSeqMap (==x) (viewMap k)

deleteFindSeqMap :: (Ord k) => (v -> Bool) -> (SeqMap k v -> Maybe (MapView k v)) -> SeqMap k v -> (Maybe v, SeqMap k v)
deleteFindSeqMap pred view map = fromMaybe (Nothing, map) $ do
  ((k, vs), deletedMap) <- view map
  case Q.findIndexL pred vs of
    Nothing -> Nothing
    Just idx ->
      let vs' = Q.deleteAt idx vs
          v = Q.index vs idx
      in Just (Just v, M.insert k vs' deletedMap)

viewMap :: Ord k => k -> M.Map k v -> Maybe ((k, v), M.Map k v)
viewMap k map = do
  v <- M.lookup k map
  return ((k, v), M.delete k map)

deleteFindFirstSeqMap :: (Ord k) => (SeqMap k v -> Maybe (MapView k v)) -> SeqMap k v -> (Maybe v, SeqMap k v)
deleteFindFirstSeqMap = deleteFindSeqMap (const True)

price :: LimitOrder -> Double
price LimitOrder{..} = ((/) `on` fromIntegral) _lorder_fromAmount _lorder_toAmount
