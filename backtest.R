##############################
##### main function of backtest

backtest <- function(strategy, start, end, data, rf.rate, w0, transact.cost=1e-3) {
  start = as.Date(start)
  end = as.Date(end)
  date = index(data)
  bt.date = (1:length(date))[date >= start & date <= end]
  bt.length = length(bt.date)
  
  bt.data =lapply(bt.date, function(x) data[1:(x - 1),])
  signals = sapply(bt.data, strategy, w0=w0)
  
  bt.test.risky = unclass(data[bt.date]) / unclass(data[bt.date - 1]) - 1
  bt.test.rf = unclass(rf.rate[bt.date])
  bt.day.ret = signals * bt.test.risky + (1 - signals) * bt.test.rf + 1
  bt.total.ret = c(w0, bt.day.ret)
  bt.total.ret = cumprod(bt.total.ret)
  return(bt.total.ret)
}