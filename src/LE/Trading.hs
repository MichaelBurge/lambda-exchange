module LE.Trading where

import Protolude

import Control.Monad.Loops
import Control.Concurrent.STM

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Tagged

import LE.Types
import LE.Map

type ConsistencyCheck = Exchange -> STM Bool

allCurrencies :: S.Set Currency
allCurrencies = S.fromList [
  "USD",
  "BTC",
  "ETH"
  ]

cancelBid :: Bid -> OrderBook -> OrderBook
cancelBid bid book@OrderBook{..} =
  let k = price $ unTagged bid
      (_, deletedBids) = deleteFindEq k bid _book_bids
  in book { _book_bids = deletedBids }

cancelAsk :: Ask -> OrderBook -> OrderBook
cancelAsk ask book@OrderBook{..} =
  let k = price $ unTagged ask
      (_, deletedAsks) = deleteFindEq k ask _book_asks
  in book { _book_asks = deletedAsks }

lowestAsk :: OrderBook -> (Maybe Ask, OrderBook)
lowestAsk book@OrderBook{..} =
  let (mAsk, deletedAsks) = deleteFindMin _book_asks
      newBook = book { _book_asks = deletedAsks }
  in (mAsk, newBook)

highestBid :: OrderBook -> (Maybe Bid, OrderBook)
highestBid book@OrderBook{..} =
  let (mBid, deletedBids) = deleteFindMax _book_bids
      newBook = book { _book_bids = deletedBids }
  in (mBid, newBook)
     
fillBid :: Bid -> OrderBook -> ([Trade], OrderBook)
fillBid bid book = case matchBid bid book of
  (Nothing, trades, book) ->
    (trades, book)
  (Just bidRemainder, trades, book) ->
    (trades, unsafe_addBid bidRemainder book)

fillAsk :: Ask -> OrderBook -> ([Trade], OrderBook)
fillAsk ask book = case matchAsk ask book of
  (Nothing, trades, book) ->
    (trades, book)
  (Just askRemainder, trades, book) ->
    (trades, unsafe_addAsk askRemainder book)

tryFillMBid :: MBid -> Balances -> OrderBook -> (Maybe MBid, [Trade], OrderBook)
tryFillMBid mbid bals book@OrderBook{..} =
  case M.lookup _book_toCurrency bals of
    Nothing -> (Nothing, [], book)
    Just toBalance ->
      -- A Market order is a non-persistable limit order with the user bidding his entire balance.
      let (Tagged MarketOrder{..}) = mbid
          bid = Tagged $ LimitOrder {
            _lorder_user = _morder_user,
            _lorder_fromAmount = _morder_amount,
            _lorder_toAmount = toBalance
            }
      in case matchBid bid book of
        (Nothing, trades, book) ->
          (Nothing, trades, book)
        (Just bid, trades, book) ->
          (Just $ bidToMbid bid, trades, book)

tryFillMAsk :: MAsk -> Balances -> OrderBook -> (Maybe MAsk, [Trade], OrderBook)
tryFillMAsk mask bals book@OrderBook{..} =
  case M.lookup _book_fromCurrency bals of
    Nothing -> (Nothing, [], book)
    Just x | x < 1 -> (Nothing, [], book)
    Just fromBalance ->
      let (Tagged MarketOrder{..}) = mask
          ask = Tagged $ LimitOrder {
            _lorder_user = _morder_user,
            _lorder_fromAmount = fromBalance,
            _lorder_toAmount = _morder_amount
            }
      in case matchAsk ask book of
        (Nothing, trades, book) ->
          (Nothing, trades, book)
        (Just ask, trades, book) ->
          (Just $ askToMask ask, trades, book)
          
             
bidToMbid :: Bid -> MBid
bidToMbid (Tagged LimitOrder{..}) = Tagged $ MarketOrder _lorder_user _lorder_fromAmount

askToMask :: Ask -> MAsk
askToMask (Tagged LimitOrder{..}) = Tagged $ MarketOrder _lorder_user _lorder_toAmount
  
matchBid :: Bid -> OrderBook -> (Maybe Bid, [Trade], OrderBook)
matchBid bid book =
  let pair = _book_pair book
      loop :: (Bid, [Trade], OrderBook) -> (Maybe Bid, [Trade], OrderBook)
      loop x@(bid, trades, book) =
        case lowestAsk book of
          -- Case 1: The order book has no asks
          (Nothing, _) -> (Just bid, [], book)
          (Just lowestAsk, deletedBook) ->
            case mergeBid pair bid lowestAsk of
              -- Case 2: The bid was unable to be matched
              (Just bid, Just _, Nothing) -> (Just bid, trades, book)
              -- Case 3: The bid was partially matched; repeat the loop
              (Just bidRemainder, Nothing, Just trade) ->
                loop (bidRemainder, trade:trades, deletedBook)
              -- Case 4: The ask was partially matched; terminate the loop.
              (Nothing, Just askRemainder, Just trade) ->
                (Nothing, trade:trades, unsafe_addAsk askRemainder deletedBook)
              -- Case 5: The bid and ask exactly canceled each other out
              (Nothing, Nothing, Just trade) ->
                (Nothing, trade:trades, deletedBook)
              -- Case 6: Impossible cases
              x -> panic $ "fillBid: Unexpected case: " <> show x
  in loop (bid, [], book)

matchAsk :: Ask -> OrderBook -> (Maybe Ask, [Trade], OrderBook)
matchAsk ask book =
  let pair = _book_pair book
      loop :: (Ask, [Trade], OrderBook) -> (Maybe Ask, [Trade], OrderBook)
      loop x@(ask, trades, book) =
        case highestBid book of
          -- Case 1: The order book has no bids
          (Nothing, _) -> (Just ask, [], book)
          (Just highestBid, deletedBook) ->
            case mergeBid pair highestBid ask of
              -- Case 2: The ask was unable to be matched
              (Just _, Just _, Nothing) -> (Just ask, trades, book)
              -- Case 3: The ask was partially matched; repeat the loop
              (Nothing, Just askRemainder, Just trade) ->
                loop (askRemainder, trade:trades, deletedBook)
              -- Case 4: The bid was partially matched; terminate the loop.
              (Just bidRemainder, Nothing, Just trade) ->
                (Nothing, trade:trades, unsafe_addBid bidRemainder deletedBook)
              -- Case 5: The bid and ask exactly canceled each other out
              (Nothing, Nothing, Just trade) -> (Nothing, trade:trades, deletedBook)
              -- Case 6: Impossible cases
              x -> panic $ "fillBid: Unexpected case: " <> show x
  in loop (ask, [], book)

mergeBid :: CurrencyPair -> Bid -> Ask -> (Maybe Bid, Maybe Ask, Maybe Trade)
mergeBid (fromCurrency, toCurrency) bid ask =
  let bidOrder = unTagged bid
      askOrder = unTagged ask
      n1 = _lorder_fromAmount bidOrder
      d1 = _lorder_toAmount bidOrder
      n2 = negate $ _lorder_fromAmount askOrder
      d2 = _lorder_toAmount askOrder
      buyer = _lorder_user bidOrder
      seller = _lorder_user askOrder
      fi = fromIntegral
      -- If seller rounds down, price would be below his limit.
      sellerPrice = ceiling (fi n2 / fi d2)
      -- If buyer rounds up, price would be above his limit.
      buyerPrice = floor (fi n1 / fi d1)

      unitPrice = buyerPrice
      numUnits = min n1 n2
      toAmount = unitPrice * numUnits
      fromTransfer = DoubleEntry {
        _de_fromAccount = seller,
        _de_toAccount   = buyer,
        _de_amount      = numUnits,
        _de_currency    = fromCurrency
        }
      toTransfer = DoubleEntry {
        _de_fromAccount = buyer,
        _de_toAccount   = seller,
        _de_amount      = toAmount,
        _de_currency    = toCurrency
        }
      trade = Trade fromTransfer toTransfer
      (mNewBid, mNewAsk) = case n1 `compare` n2 of
        -- Case 1: Buyer is done; seller still has inventory
        LT -> let newAsk = Tagged $ LimitOrder {
                    _lorder_user       = seller,
                    _lorder_fromAmount = n2 - numUnits,
                    _lorder_toAmount   = sellerPrice
                    }
              in (Nothing, Just newAsk)
        -- Case 2: Seller is out; buyer needs more
        GT -> let newBid = Tagged $ LimitOrder {
                    _lorder_user       = buyer,
                    _lorder_fromAmount = n1 - numUnits,
                    _lorder_toAmount   = buyerPrice
                    }
              in (Just newBid, Nothing)
        -- Case 3: Buyer and seller exactly traded
        EQ -> (Nothing, Nothing)
  in if buyerPrice >= sellerPrice
     -- Bid has crossed the ask, so we can generate a trade.
     then (mNewBid, mNewAsk, Just trade)
     -- Bid is less than ask, so they can't be merged.
     else (Just bid, Just ask, Nothing)

unsafe_addBid :: Bid -> OrderBook -> OrderBook
unsafe_addBid bid book@OrderBook{..} =
  let k = price $ unTagged bid
      bids = case M.lookup k _book_bids of
        Nothing -> Q.empty
        Just bids -> bids
      newBids = bids Q.|> bid
  in book { _book_bids = M.insert k newBids _book_bids }

unsafe_addAsk :: Ask -> OrderBook -> OrderBook
unsafe_addAsk ask book@OrderBook{..} =
  let k = price $ unTagged ask
      asks = case M.lookup k _book_asks of
        Nothing -> Q.empty
        Just asks -> asks
      newAsks = asks Q.|> ask
  in book { _book_asks = M.insert k newAsks _book_asks }

isBid :: LimitOrder -> Bool
isBid order =  _lorder_fromAmount order > 0

rollupBalances :: Foldable f => f (UserId, Currency, Amount) -> M.Map UserId Balances
rollupBalances = undefined

destructBalances :: M.Map UserId Balances -> [(UserId, Currency, Amount)]
destructBalances = undefined

userExternalBalances :: Exchange -> STM (M.Map UserId Balances)
userExternalBalances Exchange{..} = do
  externals <- readTVar _exchange_external
  let mkDelta (Tagged SingleEntry{..}) =
        (_se_account, _se_currency, _se_amount)
  return $ rollupBalances $ map mkDelta externals

userInternalBalances :: Exchange -> STM (M.Map UserId Balances)
userInternalBalances Exchange{..} = do
  trades <- readTVar _exchange_trades
  let mkDeltasDe DoubleEntry{..} =
        [(_de_fromAccount, _de_currency, -_de_amount),
         (_de_toAccount, _de_currency, _de_amount)]
  let mkDeltas Trade{..} =
        concatMap mkDeltasDe [_trade_from, _trade_to]
  return $ rollupBalances $ concatMap mkDeltas trades

userHeldBalances :: Exchange -> STM (M.Map UserId Balances)
userHeldBalances Exchange{..} = do
  book@OrderBook{..} <- readTVar _exchange_book
  let mkDelta order@LimitOrder{..} =
        if isBid order
        then (_lorder_user, _book_fromCurrency, _lorder_fromAmount)
        else (_lorder_user, _book_toCurrency, _lorder_toAmount)
  return $ rollupBalances $ map mkDelta $ toList book
  

userBalances :: Exchange -> STM (M.Map UserId Balances)
userBalances exchange = do
  externals <- userExternalBalances exchange
  internals <- userInternalBalances exchange
  helds     <- userHeldBalances exchange
  let deltas =
        destructBalances externals <>
        destructBalances internals <>
        map (\(a,b,c) -> (a,b,-c)) (destructBalances helds)
  return $ rollupBalances deltas

bookBalances :: Exchange -> STM Balances
bookBalances exchange = do
  balances <- userHeldBalances exchange
  let deltas = map (\(a,b,c) -> (b,c)) $ destructBalances balances
  return $ M.fromListWith (+) deltas

userBookBalances :: Exchange -> UserId -> STM Balances
userBookBalances exchange user = do
  bals <- userBalances exchange
  return $ case M.lookup user bals of
    Nothing -> M.empty
    Just userBals -> userBals
