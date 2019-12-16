##############################
##### main function of backtest

backtest <- function(strategy, start, end, data, rf.rate, w0, transact.cost=1e-3) {
  start = as.Date(start)
  end = as.Date(end)
  date = index(data)
  bt.date = (1:length(date))[date >= start & date <= end]
  bt.length = length(bt.date)
  bt.hist.len = bt.date[1] - 1
  
  bt.data =sapply(bt.date, function(x) coredata(data[(x - bt.hist.len + 1):(x - 1),]))
  signals = rep(0, bt.length)
  day.result = NULL
  for (i in 1:bt.length) {
    print(date[bt.date[i]])
    day.result = strategy(bt.data[,i], w0, day.result)
    signals[i] = day.result[['asset.alloc']]
  }
  
  ret.list = list(backtest.date = date[date >= start & date <= end], weights=signals, 
                  risky=coredata(data[bt.date]), rf=coredata(rf.rate[bt.date]),
                  num.days=bt.length, initial.wealth = w0, transaction.cost = transact.cost)
  return(ret.list)
}

simple.summary.strat <- function(bt.result) {
  ws = bt.result$weights
  rk = bt.result$risky
  rf = bt.result$rf
  rf = 100 * cumprod(rf + 1)
  
  wealth = rep(0, length(rk))
  wealth[1] = bt.result$initial.wealth
  a = floor(ws[1] * wealth[1] / rk[1])
  b = floor((1 - ws[1]) * wealth[1] / rf[1])
  for (i in 2:bt.result$num.days) {
    wealth[i] = wealth[i - 1] + a * (rk[i] - rk[i - 1]) + b * (rf[i] - rf[i - 1])
    a = floor(ws[i] * wealth[i] / rk[i])
    b = floor((1 - ws[i]) * wealth[i] / rf[i])
  }
  
  ret = diff(wealth) / wealth[1:(length(wealth) - 1)]
  cum.ret = sapply(wealth, function (x) x / bt.result$initial.wealth - 1)
  
  ret.mean = mean(ret)
  vol = sd(ret)
  sr = ret.mean / vol
  maxdd = 0
  for (i in 2:length(wealth))
    maxdd = max(maxdd, 1 - wealth[i] / max(wealth[1:i]))
  
  stat.vec = c(ret.mean, vol, sr, maxdd)
  names(stat.vec) <- c('return', 'volatility', 'Sharpe ratio', 'max dd')
  plot(wealth, type = 'l', main = 'wealth')
  plot(ret, type = 'l', main = 'return')
  print(stat.vec)
  ret.list = list(performance.indicator = stat.vec,
                  daily.return = ret, cumulative.return = cum.ret, wealth = wealth)
  return(ret.list)
}