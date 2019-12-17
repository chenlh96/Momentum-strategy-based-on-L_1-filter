L1.high.freq.strat <- function(data, trade.idx, curr.vars) {
  data = coredata(data)
  T1 = 410 * 5 ## previous half hours
  T2 = 410 ## forecast next 5 mins
  n.roll = 5 ## take half hour data as validation
  n.lambda = 15
  
  if (is.null(curr.vars)) {
    curr.vars = list(total.count = 10, count=0, data=c(), last=c(), position = rep(0, length(data)))
  }
  if (curr.vars$count < 10) {
    curr.vars$count = curr.vars$count + 1
    curr.vars$data = c(curr.vars$data, data)
    curr.vars$last = c(curr.vars$last, length(data))
  } else {
    l1c.result = cv.fit.l1tf(curr.vars$data, T1=T1, T2=T2, n.roll=n.roll,n.lambda=n.lambda, diff=2)
    position = ifelse(diff(l1c.result$predicted.trend)[1] > 0, 1, -1)
    curr.vars$position = rep(0, length(data))
    curr.vars$position[trade.idx[1]] = position
    curr.vars$position[trade.idx[length(trade.idx)]] = -position
    
    curr.vars$data = c(curr.vars$data[(curr.vars$last[1] + 1):length(curr.vars$data)], data)
    curr.vars$last = c(curr.vars$last[2:length(curr.vars$last)], length(data))
  }
  
  return(curr.vars)
}