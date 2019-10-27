L1.high.freq.strat <- function(data, curr.vars) {
  T1 = 30 * 60 ## previous half hours
  T2 = 5 * 60 ## forecast next 5 mins
  n.roll = 6 ## take half hour data as validation
  n.lambda = 15
  
  if (is.null(curr.vars)) {
    curr.vars = list(count=T2 + 1, l1c.trend=NULL, max.count=T2, position=0)
  }
  
  if (length(data) > T1 + T2 * n.roll) {
    if (curr.vars$count > curr.vars$max.count) {
      curr.vars$count = 1
      l1c.result = cv.fit.l1tf(data, T1=T1, T2=T2, n.roll=n.roll,n.lambda=n.lambda, diff=2)
      curr.vars$l1c.trend = l1c.result$predicted.trend
      curr.vars$position = ifelse(diff(curr.vars$l1c.trend)[1] > 0, 1, -1)
    }
    curr.vars$count = curr.vars$count + 1
  }
  return(curr.vars)
}