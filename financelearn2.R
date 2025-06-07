library(quantmod)

AAPL <- data.frame(getSymbols("AAPL", auto.assign = F))
colnames(AAPL) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
AAPL

write.csv(AAPL, "AAPL-Data.csv")
AAPL <- read.csv("AAPL-Data.csv", row.names = 1)
AAPL

plot(AAPL)

AAPL[, "Close"]
plot(Cl(AAPL))
plot(Cl(AAPL), type = 'l')
plot(Ad(AAPL), type = 'l')

length(Ad(AAPL))
#simple return calculation-example
100*((Ad(AAPL)[4637] - Ad(AAPL)[1])/Ad(AAPL)[1])
#daily returns- long process to calculate
100*((Ad(AAPL)[2] - Ad(AAPL)[1])/Ad(AAPL)[1])
100*((Ad(AAPL)[3] - Ad(AAPL)[2])/Ad(AAPL)[2])
100*((Ad(AAPL)[4] - Ad(AAPL)[3])/Ad(AAPL)[3])

#quicker function to calculate daily returns
class(AAPL)
class(as.xts(AAPL))
dailyReturn(Ad(as.xts(AAPL)), type = 'arithmetic')
plot(dailyReturn(Ad(as.xts(AAPL)), type = 'arithmetic'), type = 'l')
#graph for cumulative returns
plot(cumprod(1 + dailyReturn(Ad(as.xts(AAPL)), type = 'arithmetic')))

#log returns- use for things that will be compounded continuously and depends on situation, i.e savings in a bank or stock returns
plot(diff(log(Ad(AAPL))), type = 'l')

apply(AAPL[,c("Close", "Adjusted")], 2, length)
apply(AAPL[,c("Close", "Adjusted")], 2, log)
head(apply(AAPL[,c("Close", "Adjusted")], 2, log))
apply(apply(AAPL[,c("Close", "Adjusted")], 2, log), 2, diff)


#compare function- comparing adjusted to close in this example
compareAdtoCl <- data.frame(apply(apply(AAPL[,c("Close", "Adjusted")], 2, log), 2, diff))
compareAdtoCl
plot(compareAdtoCl, type = 'l')
plot(compareAdtoCl[,1], type = 'l')
lines(compareAdtoCl[,2], type = 'l', col = 'red') 

compareAdtoClCumSum <- data.frame(apply(compareAdtoCl, 2, cumsum))
compareAdtoClCumSum
plot(compareAdtoClCumSum[,1], type = 'l')
lines(compareAdtoClCumSum[,2], type = 'l', col = 'red')

#difference between log returns(line is red) and simple returns(line is black)
plot(cumprod(1 + dailyReturn(Ad(as.xts(AAPL)), type = 'arithmetic')))
lines(cumsum(dailyReturn(Ad(as.xts(AAPL)), type = 'log')), col = 'red')



#performance analytics-  best for multi-stock performance summaries 
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics) 
#uses xts zoo ojbects- which are souped up data frames

as.xts(compareAdtoCl)
data <- as.xts(compareAdtoCl)
data <- exp(data) - 1 #converts back to simple returns
data

charts.PerformanceSummary(data, main = 'Compare Close to Adjusted Cumulative Discrete Return')

#trying tesla now
TSLA <- dailyReturn(Ad(getSymbols("TSLA", auto.assign = F)))
TSLA

charts.PerformanceSummary(TSLA, main = 'TSLA')

#microsoft
MSFT <-  dailyReturn(Ad(getSymbols("MSFT", auto.assign = F)))
MSFT

charts.PerformanceSummary(MSFT, main = 'MSFT')

#charting tesla and microsoft together 
colnames(MSFT) <- c('MSFT')
colnames(TSLA) <- c('TSLA')
head(MSFT)
head(TSLA)

dim(TSLA)
dim(MSFT)
merge(TSLA, MSFT)
dim(merge(TSLA, MSFT))
# tesla doesnt start trading until three years after microsoft 
# we will keep days in which both are trading only- will start from 2010 then
merge(TSLA, MSFT, all = F)
dim(merge(TSLA, MSFT, all = F))

MSFTvsTSLA <- merge(TSLA, MSFT, all = F)
MSFTvsTSLA 

charts.PerformanceSummary(MSFTvsTSLA )


# sharpe ratio- measure of risk-adjusted return- the higher the better 
# sharpe ratio = (expected return - risk free rate)/ standard deviation of returns 
# two assumptions: 252 trading days in a year and Rf is about 4.65% in UK 

table.AnnualizedReturns(MSFTvsTSLA, scale =252, Rf= 0.0465/252)
#shows average annual returns adn shows % return if you held it for a certain number of years
# MSFT has a higher shapre ratio which is better in this case 