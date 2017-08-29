module LE.Checks where


consistency_noNegativeBalances :: ConsistencyCheck
consistency_noNegativeBalances = \exchange -> do
  bals <- userBalances exchange
  let checkUser (userId, balances) =
        flip all (M.toList balances) $ \(currency, balance) ->
        balance >= 0
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
    checkTrade Trade{..} = _de_user _trade_from /= _de_user _trade_to
  

installSanityChecks :: Exchange -> IO ()
installSanityChecks exchange =
  atomically $ mapM_ installCheck [
    consistency_noNegativeBalances,
    consistency_ordersBackedByAccount,
    consistency_allCurrenciesExist,
    consistency_noSelfTrades
  ]
  where
    installCheck check = always $ check exchange
Returns the highest bid, lowest ask, and the book with them removed.

