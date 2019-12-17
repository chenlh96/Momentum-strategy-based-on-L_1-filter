##############################
##### main function of backtest
wd = 'D:\\Codes\\Code Repositories\\filter'
source(paste(wd, 'data.R', sep = '\\'))

backtest.hf <- function(strategy, data, k, on, lot.size, w0, start=NULL, end=NULL) {
  if (is.null(start) & is.null(end))
    bt.data = aggregate.tick(data$price, k = k, on = on)
  else
    bt.data = aggregate.tick(data$price[paste(start, end, sep = '/')], k = k, on = on)
  bt.data = split(bt.data, 'day')
  bt.date = unique(as.Date(index(data)))
  n.days = length(bt.data)
  
  bt.pos = lapply(bt.data, function(x) rep(0, length(x)))
  
  am.range = c('09:30:00', '12:00:00')
  pm.range = c('13:00:00', '16:00:00')
  
  curr.vars = NULL
  for (i in 1:n.days) {
    curr.data = bt.data[[i]]
    curr.date = bt.date[[i]]
    print(curr.date)
    time.idx = index(curr.data)
    curr.am = as.POSIXlt(paste(curr.date, am.range))
    curr.pm = as.POSIXlt(paste(curr.date, pm.range))
    
    n.data = length(curr.data)
    trade.idx = (1:n.data)[(time.idx >= curr.am[1] & time.idx <= curr.am[2]) | (time.idx >= curr.pm[1] & time.idx <= curr.pm[2])]
    
    # curr.vars = NULL
    # for (j in 1:length(trade.idx)) {
    #   curr.idx = trade.idx[j]
    #   curr.vars = strategy(curr.data[1:j], curr.vars)
    #   bt.pos[[i]][j] = curr.vars$position
    # }
    
    curr.vars = strategy(curr.data, trade.idx, curr.vars)
    bt.pos[[i]] = curr.vars$position
  }
  sum.list = list(price=bt.data, pos=bt.pos, date=bt.date, lot.size=lot.size, w0 = w0)
  return(sum.list)
}

hf.summary.strat <- function(bt.result) {
  bt.price = bt.result$price
  bt.pos = bt.result$pos
  bt.date = bt.result$date
  n.days = length(bt.price)
  profit.list = list()
  profit.vec = rep(0, n.days)
  names(profit.vec) <- bt.date
  w0 = bt.result$w0
  
  for (i in 1:n.days) {
    pos = bt.pos[[i]]
    if (all(pos == 0))
      next
    
    price = as.vector(bt.price[[i]])
    pos.idx = which(pos != 0)
    prev.pos = pos[pos.idx[1]]
    prev.price = price[pos.idx[1]]
    profit.list[[i]] = rep(0, length(pos.idx))
    
    for (j in 2:length(pos.idx)) {
      curr.pos = pos[pos.idx[j]]
      curr.price = price[pos.idx[j]]
      
      if (curr.pos < 0 & prev.pos > 0) {
        profit = curr.price - prev.price
      } else if (curr.pos > 0 & prev.pos < 0) {
        profit = prev.price - curr.price
      } else {
        profit = 0
      }
      
      prev.pos = curr.pos
      prev.price = curr.price
      profit.list[[i]][j] = profit
    }
    profit.vec[i] = sum(profit.list[[i]]) * bt.result$lot.size
  }
  wealth = c(w0, profit.vec)
  wealth = cumsum(wealth)
  ret = diff(wealth) / wealth[1:(length(wealth) - 1)]
  
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
                  daily.return = ret, wealth = wealth, profit = profit.vec)
  
  return(ret.list)
}