##############################
##### main function of backtest

backtest <- function(strategy, start, end, data, rf.rate, w0, transact.cost=1e-3) {
  start = as.Date(start)
  end = as.Date(end)
  date = index(data)
  bt.date = (1:length(date))[date >= start & date <= end]
  bt.length = length(bt.date)
  bt.hist.len = bt.date[1] - 1
  
  bt.data =lapply(bt.date, function(x) data[(x - bt.hist.len + 1):(x - 1),])
  signals = rep(0, bt.length)
  day.result = NULL
  for (i in 1:bt.length) {
    print(date[bt.date[i]])
    day.result = strategy(bt.data[[i]], w0, day.result)
    signals[i] = day.result[['asset.alloc']]
  }
  
  bt.test.risky = as.vector(data[bt.date]) / as.vector(data[bt.date - 1]) - 1
  bt.test.rf = as.vector(rf.rate[bt.date])
  bt.day.ret = signals * bt.test.risky + (1 - signals) * bt.test.rf
  ret.list = list(backtest.date = date[date >= start & date <= end], num.days=bt.length,
                  daily.return=bt.day.ret, initial.wealth = w0, weights=signals,
                  transaction.cost = transact.cost)
  return(ret.list)
}

simple.summary.strat <- function(bt.result) {
  ret = bt.result$daily.return
  w0 = bt.result$initial.wealth
  
  transact.pay = diff(c(0, bt.results$weights)) * be.result$transaction.cost
  total.weal = cumprod(c(w0, ret + 1))
  perf = tail(total.weal, 1) / w0 - 1
  anul.ret = mean(ret)
  anul.vol = sd(ret)
  sr = anul.ret / anul.vol
  mdd = 0
  for (i in 2:length(total.weal))
    mdd = max(mdd, 1 - total.weal[i] / max(total.weal[1:i]))
  
  plot(bt.result$daily.return, type = 'l')
  plot(total.weal, type = 'l')
  stats.vec = c(anul.ret, anul.vol, sr, mdd)
  names(stats.vec) <- c('annualized.return', 'annualized.volatily', 'annualized.Sharpe.ratio',
                        'max.drawdown')
  print(stats.vec)
  
  stat.list = list(backtest.date = bt.result$backtest.date, num.days=bt.result$num.days,
                   initial.wealth = w0,  performance = perf,
                   daily.return = ret, total.wealth = total.weal,
                   annualized.return = anul.ret, annualized.volatility = anul.vol, 
                   annualized.Sharpe.ratio = sr, max.drawdown = mdd)
  return(stat.list)
}