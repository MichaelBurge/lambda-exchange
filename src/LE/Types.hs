module LE.Types where

import Protolude

import Control.Concurrent.STM.TVar
import Data.Tagged

import qualified Data.Sequence as Q
import qualified Data.Map as M

type UserId = Integer

type Currency = Text
curr_usd = "USD" :: Currency
curr_btc = "BTC" :: Currency
curr_eth = "ETH" :: Currency

type CurrencyPair = (Currency, Currency)
type Amount = Integer
type Price = Double

data LimitOrder = LimitOrder {
  _lorder_user         :: UserId,
  _lorder_fromAmount   :: Amount,
  _lorder_toAmount     :: Amount
  } deriving (Eq, Show, Generic)

data TBid
data TAsk

type BidT a = Tagged TBid a
type AskT a = Tagged TAsk a
type Bid = BidT LimitOrder
type Ask = AskT LimitOrder

data MarketOrder = MarketOrder {
  _morder_user   :: UserId,
  _morder_amount :: Amount
  } deriving (Eq, Show, Generic)

type MBid = BidT MarketOrder
type MAsk = AskT MarketOrder

data OrderBookF a = OrderBook {
  _book_fromCurrency :: Currency,
  _book_toCurrency   :: Currency,
  _book_bids         :: M.Map Price (Q.Seq (BidT a)),
  _book_asks         :: M.Map Price (Q.Seq (AskT a))
  } deriving (Eq, Show, Functor, Traversable, Foldable)
type OrderBook = OrderBookF LimitOrder

newBook :: CurrencyPair -> OrderBook
newBook (from, to) = OrderBook from to M.empty M.empty

data SingleEntry = SingleEntry {
  _se_account  :: UserId,
  _se_currency :: Currency,
  _se_amount   :: Amount
  } deriving (Eq, Show, Generic)

data DoubleEntry = DoubleEntry {
  _de_fromAccount :: UserId,
  _de_toAccount   :: UserId,
  _de_currency    :: Currency,
  _de_amount      :: Amount
  } deriving (Eq, Show, Generic)

data TradeF a = Trade {
  _trade_from :: a,
  _trade_to   :: a
  } deriving (Eq, Show, Functor, Traversable, Foldable, Generic)
type Trade = TradeF DoubleEntry
              
type Balances = M.Map Currency Amount

data External

type ExternalTransfer = Tagged External SingleEntry

data Exchange = Exchange {
  _exchange_external :: TVar (Q.Seq External),
  _exchange_book     :: TVar OrderBook,
  _exchange_trades   :: TVar (Q.Seq Trade)
  }


_book_pair :: OrderBookF a -> CurrencyPair
_book_pair OrderBook{..} = (_book_fromCurrency, _book_toCurrency)
