module LE.Checks where

import Protolude

import Control.Monad.Loops
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Prelude(error)

import qualified Data.Map as M
import qualified Data.Set as S

import LE.Types
import LE.Trading

allCurrencies :: S.Set Currency
allCurrencies = S.fromList [
  "USD",
  "BTC",
  "ETH"
  ]

consistency_noNegativeBalances :: ConsistencyCheck
consistency_noNegativeBalances = \exchange -> do
  bals <- userBalances exchange
  let checkUser (userId, balances) =
        flip all (M.toList balances) $ \(currency, balance) ->
        if balance >= 0
        then True
        else error $ "Negative balance for " <> show (userId, currency, balance)
  return $ all checkUser $ M.toList bals

consistency_ordersBackedByAccount :: ConsistencyCheck
consistency_ordersBackedByAccount = \exchange -> do
  usersBals <- userBalances exchange

  let checkUserBalance :: Balances -> (Currency, Amount) -> Bool
      checkUserBalance userBals (currency, bookAmount) =
        case M.lookup currency userBals of
          Nothing -> False
          Just userAmount -> userAmount >= bookAmount

  let checkUser :: (UserId, Balances) -> STM Bool
      checkUser (user, userBals) = do
        bookBals <- userBookBalances exchange user
        let currenciesPending = M.toList bookBals
        return $ all (checkUserBalance userBals) currenciesPending
  allM checkUser $ M.toList usersBals

consistency_allCurrenciesExist :: ConsistencyCheck
consistency_allCurrenciesExist = \exchange -> do
  usersBals <- userBalances exchange
  bookBals <- bookBalances exchange
  let valid currency = currency `elem` allCurrencies
      checkBals bals = all valid $ M.keys bals
      usersCheck = all checkBals usersBals
      booksCheck = all valid $ M.keys bookBals
  return $ usersCheck && booksCheck

consistency_noSelfTrades :: ConsistencyCheck
consistency_noSelfTrades = \exchange -> do
  trades <- readTVar $ _exchange_trades exchange
  return $ all checkTrade trades
  where
    checkDe DoubleEntry{..} = _de_fromAccount /= _de_toAccount
    checkTrade Trade{..} = checkDe _trade_from && checkDe _trade_to
  

installSanityChecks :: Exchange -> IO ()
installSanityChecks exchange = do
  atomically $ mapM_ installCheck [
    (consistency_noNegativeBalances, "No negative balances"),
    (consistency_ordersBackedByAccount, "Orders must be backed by account"),
    (consistency_allCurrenciesExist, "Non-existent currency"),
    (consistency_noSelfTrades, "Users cannot trade with themselves")
    ]
  where
    installCheck (check, message) = alwaysSucceeds $ do
      b <- check exchange
      if b
        then return ()
        else error message
