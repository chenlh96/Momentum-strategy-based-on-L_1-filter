##############################
##### main function of backtest

backtest.high.freq <- function(strategy, data, start=NULL, end=NULL) {
  if (is.null(start) & is.null(end))
    bt.data = split(data$price, f='day')
  else
    bt.data = split(data$price[paste(start, end, sep = '/')], f='day')
  bt.date = as.character(unique(round(index(data$price),"day")))
  n.days = length(bt.data)
  
  bt.pos = lapply(bt.data, function(x) rep(0, length(x)))
  
  am.range = c('09:30:00', '12:00:00')
  pm.range = c('13:00:00', '16:00:00')
  
  for (i in 1:n.days) {
    curr.data = bt.data[[i]]
    curr.date = bt.date[[i]]
    print(curr.date)
    time.idx = index(curr.data)
    curr.am = as.POSIXlt(paste(curr.date, am.range))
    curr.pm = as.POSIXlt(paste(curr.date, pm.range))
    
    n.data = length(curr.data)
    trade.idx = (1:n.data)[(time.idx >= curr.am[1] & time.idx <= curr.am[2]) | (time.idx >= curr.pm[1] & time.idx <= curr.pm[2])]
    
    curr.vars = NULL
    for (j in 1:length(trade.idx)) {
      curr.idx = trade.idx[j]
      curr.vars = strategy(curr.data[1:j], curr.vars)
      bt.pos[[i]][j] = curr.vars$position
    }
  }
  sum.list = list(price=bt.data, pos=bt.pos, date=bt.date)
  return(sum.list)
}

hf.summary.strat <- function(bt.result) {
  bt.price = bt.result$price
  bt.pos = bt.result$pos
  bt.date = bt.result$date
  n.days = length(bt.price)
  profit.list = list()
  
  for (i in 1:n.days) {
    pos = bt.pos[[i]]
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
        prev.pos = curr.pos
        prev.price = curr.price
      } else if (curr.pos > 0 & prev.pos < 0) {
        profit = prev.price - curr.price
        prev.pos = curr.pos
        prev.price = curr.price
      } else {
        profit = 0
      }
      profit.list[[i]][j] = profit
    }
    print(sum(profit.list[[i]]))
  }
  
  return(profit.list)
}