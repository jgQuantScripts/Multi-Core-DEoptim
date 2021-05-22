# Multi-Core Optimization - Finding the best Parameters for Breakouts
require("quantmod");require("RSQLite");require("lubridate");require("DEoptim");
require("PerformanceAnalytics");require("data.table");require("pbapply");require("dplyr")
# *************************************************************************************************
#                                    Wrappers
# *************************************************************************************************
# function to get stock data from SQLite DB
getOHLC = function(ticker)
{
  # connect to SQLite DB
  driver = dbDriver("SQLite")
  daytoday = "20210509"
  con = dbConnect(driver, dbname = paste0("/Volumes/3TB/SQLite/",daytoday,"_getSymbols.db"))
  # get stock data
  df = dbGetQuery(con, paste0("SELECT * FROM getSymbols WHERE Symbol ='",ticker,"'"))
  # disconnect
  dbDisconnect(con)
  # convert to xts object
  df <- xts(df[,c("Open","High","Low","Close","Volume")], order.by = as.Date(df$Date, origin="1970-01-01"))
  # format column names
  colnames(df) <- paste0(ticker,".",names(df))
  # only want OHLC data-> drop Volume Column
  OHLCV(df)
}
# function to find breakouts n Days
# df          : OHLC object
# lookBackDays: will lookback n-days to see if stock broke out of the range
# volDays     : N-Days to calculate Average Volume
# closeAtHigh : if TRUE will ONLY consider it a break out if stock closed at 
#               the High of the day
#             : if FALSE will consider it a break out if it is higher than n-days,
#               but did not close at the High of the Day
breakOut = function(df, lookBackDays, volDays, closeAtHigh=FALSE)
{
  # add MAX price column
  df$MAX = na.omit(apply(coredata(OHLC(df)),1, max))
  
  # rolling Volume Average
  df$VoAvg <- rollmean(x=Vo(df),k = volDays, align = "right")
  
  tmp = merge(Cl(df), df$MAX)
  # rollapply function every step ('by') & window is width
  tmp <- rollapply(data=tmp, width=lookBackDays,by=1,align = 'right',by.column = FALSE, 
                   FUN=function(x){
                     LAST <- as.numeric(last(Cl(x))) # last is the last/latest price in nth place 
                     bo <- nrow(x[LAST >= x$MAX,])   # compare how many times the lastest price >= MAX column
                   })
  # add column to data
  df$nDaysHigher <- as.numeric(coredata(tmp))
  # complete cases
  df <- na.omit(df)
  # subset breakout days
  if(closeAtHigh == TRUE)
  {
    breakOutDates =  df[which(df$nDaysHigher == lookBackDays & as.numeric(last(Vo(df))) > as.numeric(last(df$VoAvg))),]
  }
  
  if(closeAtHigh == FALSE)
  {
    # stocks not closing at the Highs of the day will return lookBackDays - 1 
    breakOutDates = rbind(df[which(df$nDaysHigher == lookBackDays & Vo(df) > df$VoAvg),],
                          df[which(df$nDaysHigher == (lookBackDays-1) & Vo(df) > df$VoAvg),]) %>% 
      make.index.unique(drop=TRUE, fromLast = TRUE)
  }
  # all time Highs
  
  breakOutDates
}
# function to return performance for: hold_Days
breakOutPerformance = function(df,breakOuts, hold_Days)
{
  # extract dates
  BO = index(breakOuts)
  # calculate performance After Break Out: 
  pct = lapply(as.list(1:length(BO)), function(ii){
    # subset starting date only
    START = BO[ii]
    # calculate date ranges
    PERF  = paste0(START,"/",START+days(hold_Days))
    # subset data    
    PERF  = df[PERF]
    # calculate performance
    boPRC= as.numeric(Cl(PERF))[1]
    PERF  = round((as.numeric(Cl(PERF[nrow(PERF)]))/boPRC)-1,4)
    # return as a data frame
    toRET = as.data.frame(cbind(paste(START), boPRC, PERF))
    colnames(toRET)[1] = c("boDATE")
    toRET
  })
  # row bind
  pct = do.call(rbind,pct)
  # convert to numeric
  pct$boPRC <- as.numeric(pct$boPRC)
  pct$PERF <- as.numeric(pct$PERF)
  # return df
  pct
}

# *************************************************************************************************
#                                   Optimization Functions
# *************************************************************************************************
# optimize the number of trading days to lookback for breakouts
# function to Optimize
toOptim = function(n)
{
  breakOuts = breakOut(df=df,lookBackDays=n[1],volDays = n[2],closeAtHigh=FALSE)
  boPerformance = breakOutPerformance(df=df,breakOuts=breakOuts,hold_Days = n[3])
  # geometric return / compounded return
  geoMean = table.Stats(boPerformance$PERF)["Geometric Mean",] %>% as.numeric()
  # Stdev
  stdev = sd(boPerformance$PERF,na.rm = TRUE)
  # Sharpe Ratio
  sharpeR = round(geoMean/stdev,4)
  # trades
  nTrades = nrow(boPerformance) %>% as.numeric()
  # will seek to minimize, so flip the sign
  return(-sharpeR*nTrades)
}
# function to round to whole numbers 
fnmap_f <- function(x) {c(round(x,0))}
# *************************************************************************************************
#                                   Optimization
# *************************************************************************************************
# set upper and lower limits: 
LOWER = c(9,9,3)
UPPER = c(252,50,50)
# split the data to avoid overfitting
FULL = getOHLC(ticker="GOOGL")
df = FULL["::2015"]

# Run optimization: Without Parallel
# system.time(
# r <- DEoptim(toOptim,lower=LOWER,upper=UPPER,control = list(itermax=100),fnMap = fnmap_f)
# )
#    Tested 10 iterations
#    user  system elapsed 
# 380.345  13.028 423.572

# Parallel - will use all available cores 
system.time(
r <- DEoptim(toOptim,lower=LOWER,upper=UPPER,
             control = list(itermax=100, parallelType=1, 
                            parVar=c("breakOut","df","breakOutPerformance","getOHLC"), 
             packages= c("quantmod","RSQLite","data.table","lubridate","pbapply",
                         "PerformanceAnalytics","dplyr"),
             trace=TRUE),
             fnMap = fnmap_f)
)
#    Tested 10 iterations
#    user  system elapsed 
#    0.39    0.40  198.18 
### 423.572 /198.18 == 2.13731 faster
plot(r, type="b")
# saveRDS(r,"AAPL_BO_Optim.rds")
# saveRDS(r,"SPY_BO_Optim.rds")
# saveRDS(r,"GOOGL_BO_Optim.rds")
# extract the best period
BEST = r$optim$bestmem %>% as.numeric()
# View results
breakOuts = breakOut(df=FULL,lookBackDays=BEST[1],volDays = BEST[2],closeAtHigh=FALSE)
boPerformance = breakOutPerformance(df=FULL,breakOuts=breakOuts,hold_Days = BEST[3])
# geometric return / compounded return
geoMean = table.Stats(boPerformance$PERF)["Geometric Mean",] %>% as.numeric()
# Stdev
stdev = sd(boPerformance$PERF,na.rm = TRUE)
# Sharpe Ratio
sharpeR = round(geoMean/stdev,4)
# trades
nTrades = nrow(boPerformance) %>% as.numeric()

# Extract Returns/Performance
RETS = xts(boPerformance[,"PERF"],order.by = as.Date(boPerformance$boDATE, format="%Y-%m-%d"))
# in sample
charts.PerformanceSummary(RETS["::2015"],geometric = FALSE)
# out of sample
charts.PerformanceSummary(RETS["2016::"],geometric = FALSE)
# ALL
charts.PerformanceSummary(RETS,geometric = FALSE,methods = 'HistoricalVaR')

