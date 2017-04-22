library(ggvis)
library(readxl)
library(PerformanceAnalytics)
library(dplyr)
library(lubridate)


nav <- read_excel("d:/NAV.xlsx") 
nav <- nav[complete.cases(nav),]
nav <-nav[year(nav$Date)==2016,]

xnav <- as.xts(as.data.frame(nav)[,-1],order.by = nav$Date )



annual.calmar.bench <- CalmarRatio(xnav[,2],scale = 242)
annual.downsidesd.bench <- DownsideDeviation(xnav[,2])*242/length(xnav[,2])
annual.sortino.bench <- SortinoRatio(xnav[,2])*242/length(xnav[,2])

return.stats.bench <- table.AnnualizedReturns(xnav[,2],scale=242,Rf=0.015/365)
max.drawdown.bench <- maxDrawdown(xnav[,2])
max.down.bench <- min(xnav[,2])



annual.calmar <- CalmarRatio(xnav[,1],scale = 242)
annual.downsidesd <- DownsideDeviation(xnav[,1])*242/length(xnav[,1])
annual.sortino <- SortinoRatio(xnav[,1])*242/length(xnav[,1])

return.stats <- table.AnnualizedReturns(xnav[,1],scale=242,Rf=0.015/365)
max.drawdown <- maxDrawdown(xnav[,1])
max.down <- min(xnav[,1])

chart.CumReturns(xnav[,1:3], wealth.index = T, geometric = TRUE,
                 legend.loc = "topleft", colorset = c(4,1,2), begin = "axis")

chart.Bar(xnav[,1:3], main = "Daily Returns",legend.loc = "bottomleft", colorset = c(4,1,2))


