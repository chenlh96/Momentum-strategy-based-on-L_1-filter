simple.summary.tf <- function(trd.est, day=50, ylim=NA) {
  par(mfrow=c(2,2))
  if (!(is.null(trend.sp500$lambdas) & is.null(trend.sp500$err))) {
    plot(log(trend.sp500$lambdas), trend.sp500$err, type = 'l')
  }
  
  plot(trd.est$train.trend, type = 'l')
  lines(trd.est$train.data)
  if (all(is.na(ylim))) {
    plot(trd.est$predicted.trend, type = 'l')
  } else {
    plot(trd.est$predicted.trend, type = 'l', ylim = ylim)
  }
  lines(trd.est$test.data)
  
  print(trd.est$predicted.trend[day] - trd.est$predicted.trend[1])
}
