module LE.Networking where

import Protolude

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import GHC.Generics

import LE.Types
import LE.Trading

le_port = 2345

type HandlerT a = (?req :: Request, ?respond :: (Response -> IO ResponseReceived), ?state :: Exchange) => a -> IO ResponseReceived
type Handler = HandlerT ()

data Request_ListOrders = Request_ListOrders {
  _reqListOrders_user :: Maybe UserId
  } deriving (Eq, Show, Generic)

data Request_CancelOrder = Request_CancelBid {
  _reqCancelOrder_bid :: Bid
  } | Request_CancelAsk {
  _reqCancelOrder_ask :: Ask
  } deriving (Eq, Show, Generic)

data Request_AddOrder = Request_AddBid {
  _reqAddOrder_bid :: Bid
  } | Request_AddAsk {
  _reqAddOrder_ask :: Ask
  } | Request_AddMBid {
  _reqAddOrder_mbid :: MBid
  } | Request_AddMAsk {
  _reqAddOrder_mask :: MAsk
  } deriving (Eq, Show, Generic)

data Response_ListOrders = Response_ListOrders {
  _resListOrders_bids :: [Bid],
  _resListOrders_asks :: [Ask]
  } deriving (Eq, Show, Generic)

data Response_CancelOrder = Response_CancelOrder deriving (Eq, Show, Generic)

serverMain :: IO ()
serverMain = do
  state <- initialize
  run le_port $ \req respond ->
    let ?req = req
        ?respond = respond
        ?state = state
    in do
      print req
      case (pathInfo req, requestMethod req) of
        ("listOrders" : _, "GET") -> api_listOrders ()
        _ -> respond (W.responseLBS status404 [] "Unknown path")

initialize :: IO Exchange
initialize = undefined

api_listOrders :: Handler
api_listOrders = undefined

