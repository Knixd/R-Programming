#Import Libraries
library(memoise)
library(bigmemory)
library(microbenchmark)
library(timeDate)
library(Quandl)
library(midasr)
library(ggplot2)
#library(xts)
options(xts_check_TZ=FALSE)
library(stats)
library(tseries)
library(FitAR) #for FitAr
library(forecast) #for ndiffs
library(devtools)

dev_mode(on=T)

install_github("joshuaulrich/xts")
#install.packages('xxx', repo='http://repo_adress', type='source')
dev_mode(on=F)

set.seed(1234)

#Set Quandl api permissions key
Quandl.api_key("rxgz1xRVJ4ubED-3AGz9")
firstrun <- TRUE
######################################################################
#Set Frequency
######################################################################
ycollapse<-"quarterly"
xcollapse<-"weekly"
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
xm <- merge.xts(xx, fed, join="left") #left merge because fed includes repeats's for weekends making it biased towards all fridays (for all variables eventually)


######################################################################
#Stationarity Tests
######################################################################

ndiffs(y)
apply(xm,2,ndiffs, test="adf")
apply(xm,2,ndiffs, test="kpss")
apply(xm,2,ndiffs, test="pp")

######################################################################
#Take First Difference Manually
######################################################################

yD <- diff(y)
xmD <- diff(xm)

######################################################################
#Define Sample Range
######################################################################
#Define ranges for quarterly, monthly, daily
#Q1: 01-01/03-31; Q2: 04-01/06-30; Q3:07-01/09-30; Q4:10-01/12-31

date <- "2012Q3"

if(date=="2012Q3"){
  
  h<-2
  
  sample_start_yr <- "1987"
  sample_start_day_qtr <- "-01-01" 
  sample_start_day_mon <- sample_start_day_qtr
  sample_start_day_d <- sample_start_day_qtr
  
  sample_start_q <- paste(sample_start_yr, sample_start_day_qtr, sep="")
  sample_start_m <- "1987-01-01"
  sample_start_d <- sample_start_q
  
  #Assign end dates appropriate for the horizon
  if(h==1){
    #h1 - Forecasting 2012Q4 with 2012Q3, set end to 2012Q4.
    sample_end_q <- "2012-12"
    sample_end_m <- "2012-12-01"
    sample_end_d <- "2012-12-30"
  }else if(h==2){
    #h2 - Forecasting 2013Q1 with 2012Q3, set end to 2013Q1.
    sample_end_q <- "2013-03"
    sample_end_m <- "2013-03-01"
    sample_end_d <- "2013-03-30"
  }
  
  #Data used is only ever up to 2012Q3 so 2012-10-01
  #new_data_start <- "2012-10-01/"
  #YLastUsed_date <- "2012-07"
  #YLastUsed <- y[YLastUsed_date]
  #faD <- yD[new_data_start][1:h]
 # fa <- y[new_data_start][1:h]
}
range_q <- paste(sample_start_q, sample_end_q, sep= "::")
range_m <- paste(sample_start_m, sample_end_m, sep= "::")
range_d <- paste(sample_start_d, sample_end_d, sep= "::")

######################################################################
#Create Sample
######################################################################
#Manually Check End Dates!!!!!!!!!!

ys <- y[range_q]
yDs <- yD[range_q]

xms <- xm[range_d]
xmDs <- xmD[range_d]


######################################################################
#Rename Columns
######################################################################

colnames(yDs) <- "UJ"
colnames(xmD) <- c("Nikkei", "SP500", "UB", "BY","Fed")
colnames(xmDs) <- c("Nikkei", "SP500", "UB", "BY","Fed")

colnames(ys) <- "UJ"
colnames(xm) <- c("Nikkei", "SP500", "UB", "BY","Fed")
colnames(xms) <- c("Nikkei", "SP500", "UB", "BY","Fed")

xmDs$Fed[1,] <- 0

#Graphing

xdf <- data.frame(datetime = as.POSIXlt(index(xmDs)), xmDs)

BUB <- ggplot(xdf) + 
      geom_line(aes(x=datetime, y = SP500, colour = "Nikkei"), 
                size = 0.1, 
                linetype = 1, 
                alpha = 1, na.rm = TRUE) +
      ggtitle("S&P500 Weekly Stock Index Differenced") +
      labs(x = "Time", y="S&P500 Weekly Stock Index Differenced") +
      theme(plot.title = element_text(family = "", color="#000000", face="bold", size=12, hjust=0.5)) +
      theme(axis.title = element_text(family = "", color="#666666", face="bold", size=8))+
      guides(color = "none")
BUB

ydf <- data.frame(datetime = as.POSIXlt(index(yDs)), yDs)

YUB <- ggplot(ydf, aes(x=datetime)) + 
  geom_line(aes(y = UJ, colour = "JPY/USD Differenced Quarterly Spot Rate"), 
            size = 0.1, 
            linetype = 1, 
            alpha = 1) +
  ggtitle("JPY/USD Differenced Quarterly Spot Rate") +
  labs(x = "Time", y="JPY/USD Differenced Spot Rate (Quarterly)") +
  theme(plot.title = element_text(family = "", color="#000000", face="bold", size=14, hjust=0.5)) +
  theme(axis.title = element_text(family = "", color="#666666", face="bold", size=8))+
  guides(color = "none")
YUB

#geom_vline(xintercept=as.numeric(as.Date("2012-10-01")), linetype=3) +