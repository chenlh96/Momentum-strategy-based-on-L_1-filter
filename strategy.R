wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)

data.dir = 'data\\trades.csv'
trade = read.csv(data.dir, stringsAsFactors = FALSE)
trade$date = as.Date(trade$date)
trade$time = strptime(trade$time, format = '%H:%M:%OS')

tmp = cv.l1tf(signal.nz, 400, 50, 12,15)
for (i in 1:12) plot(tmp[[15]][,i], type = 'l')