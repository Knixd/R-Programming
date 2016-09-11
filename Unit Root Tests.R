#Import Libraries
library(timeDate)
library(Quandl)
library(midasr)
#library(xts)
options(xts_check_TZ=FALSE)
library(stats)
library(tseries)
library(FitAR) #for FitAr
library(forecast) #for ndiffs
library(devtools)
dev_mode(on=T)
  install_github("joshuaulrich/xts")
dev_mode(on=F)

set.seed(1234)

#Set Quandl api permissions key
Quandl.api_key("rxgz1xRVJ4ubED-3AGz9")
######################################################################
#Set Frequency
######################################################################

ycollapse<-"quarterly"
xcollapse<-"daily"

######################################################################
#Import Data
######################################################################

#Dependent Variable
######################
y <- Quandl("FED/RXI_N_B_JA", type="xts", collapse =ycollapse, start_date="1971-01-01")

#Independent Variables
######################


uj <- Quandl("FED/RXI_N_B_JA", type="xts", collapse =xcollapse, start_date="1971-01-01")
ub <- Quandl("FED/RXI_US_N_B_UK", type="xts", collapse =xcollapse, start_date="1971-01-01")
by <- (uj*((ub)^-1))
xx <- Quandl(c("NIKKEI/INDEX.4","YAHOO/INDEX_GSPC.4","FED/RXI_US_N_B_UK"), type="xts", collapse =xcollapse, start_date="1971-01-01") 
fed <- Quandl("FRED/DFF", type="xts", collapse =xcollapse, start_date="1971-01-01")
xx <- merge.xts(xx, by, join="outer")
xm <- merge.xts(xx, fed, join="left") 
######################################################################
#Rename Columns
######################################################################

colnames(y) <- "UJ"
colnames(xm) <- c("Nikkei", "SP500", "UB", "BY","Fed")

######################################################################
#Stationarity Tests
######################################################################

#X Unit Root Tests
apply(na.omit(xm),2,adf.test, alternative="e")
apply(na.omit(xm),2,pp.test, alternative = "stationary")
apply(na.omit(xm),2,kpss.test)

#Y Unit Root Tests
adf.test(na.omit(y), alternative = "stationary")
pp.test(na.omit(y), alternative = "stationary")
kpss.test(na.omit(y))

#Ndiffs
apply(xm,2,ndiffs, test="adf")
apply(xm,2,ndiffs, test="kpss")
apply(xm,2,ndiffs, test="pp")