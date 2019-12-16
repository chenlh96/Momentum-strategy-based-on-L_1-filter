library(xts)

getData <- function(path, symbol=c('0005.HK', '0700.HK')) {
  trades.raw = read.csv(path, stringsAsFactors = FALSE)
  trades.raw$cond = as.factor(trades.raw$cond)
  levels(trades.raw$cond)
  # trades.raw = trades.raw[,1:(ncol(trades.raw) - 4)]
  trades.raw$datetime = as.POSIXlt(paste(trades.raw$date, trades.raw$time, sep = ' '), format = '%Y-%m-%d %H:%M:%OS')
  # stock.sym = names(table(trades.raw$sym))
  stock.raw = lapply(symbol, function (x) trades.raw[trades.raw$sym == x,])
  names(stock.raw) <- symbol
  
  stock = lapply(stock.raw, function (x) as.xts(x[,c('price', 'size')], x$datetime))
  return(stock)
}

aggregate.tick <- function(tickdata, k = 1, on = 'minutes') {
  tick1min.list = lapply(split(tickdata, 'day'), function(x) aggregatets(x, k = k, on = on))
  tick1min = tick1min.list[[1]]
  for (i in 2:length(tick1min.list)) tick1min = c(tick1min, tick1min.list[[i]])
  return(tick1min)
}