#Import Libraries
library(timeDate)
library(Quandl)
library(midasr)
library(xts)
options(xts_check_TZ=FALSE)
library(stats)
library(tseries)
library(FitAR)
set.seed(1234)
#Set Quandl api permissions key
Quandl.api_key("rxgz1xRVJ4ubED-3AGz9")
firstrun <- TRUE
######################################################################
#Import Data
######################################################################

#Dependent Variable
######################
uj <- Quandl("FED/RXI_N_B_JA", type="xts", start_date="1971-01-01")
y <- to.quarterly(uj, OHLC = FALSE)

#Independent Variables
######################

#Daily Data
ub <- Quandl("FED/RXI_US_N_B_UK", type="xts", start_date="1971-01-01")
by <- (uj*((ub)^-1))
xx <- Quandl(c("NIKKEI/INDEX.4","YAHOO/INDEX_GSPC.4","FED/RXI_US_N_B_UK"), type="xts", start_date="1971-01-01") 
xm <- merge.xts(xx, by, join="outer")

#Monthly Data
fed <- Quandl("PERTH/IRFFR_M", type="xts", start_date="1971-01-01")
#boj <- read.csv("C:/Users/kNoWThySelf/OneDrive/Thesis/Data/boj_lending_discount_rates1.csv", stringsAsFactors = FALSE, header = FALSE)
#boj <- xts(boj[2], order.by = as.Date(boj$V1, format="%Y-%m-%d"))

#Other
#usint <- Quandl("FRED/USINTDCSJPY", type="xts", start_date="1971-01-01")

######################################################################
#Take First Difference
######################################################################
xmD <- diff(xm)
fD <- diff(fed)
bD <- diff(boj)
yD <- diff(y)

######################################################################
#Scale Data
######################################################################
xmD[,1] <- xmD[,1]/10 #Nikkei
#xmD[,2] <- xmD[,2]/100 #SP500
xmD[,3] <- xmD[,3]*100 #Dollar/ Pound
xmD[,4] <- xmD[,4]*10 #British/Yen
#xmD[,5] <- xmD[,5]/10 #Dollar / JPY

######################################################################
#Define Sample Range
######################################################################
#Define ranges for quarterly, monthly, daily
#Q1: 01-01/03-31; Q2: 04-01/06-30; Q3:07-01/09-30; Q4:10-01/12-31
date <- "2012Q3"

if(date=="2012Q3"){
  sample_start_yr <- "1987"
  sample_start_day_qtr <- "-01-01" 
  sample_start_day_mon <- sample_start_day_qtr
  sample_start_day_d <- sample_start_day_qtr
  
  sample_start_q <- paste(sample_start_yr, sample_start_day_qtr, sep="")
  sample_start_m <- "1986-12-31" #need to be 1 less than quarterly start range (bug in r makes subsetting grab T+1)
  sample_start_d <- sample_start_q
  
  sample_end_q <- "2012-09"
  sample_end_m <- "2012-09-01"
  sample_end_d <- "2012-09-30"
  
  new_data_start <- "2012-10-01"
}
range_q <- paste(sample_start_q, sample_end_q, sep= "::")
range_m <- paste(sample_start_m, sample_end_m, sep= "::")
range_d <- paste(sample_start_d, sample_end_d, sep= "::")


######################################################################
#Create Sample
######################################################################
yDs <- yD[range_q]
xmDs <- xmD[range_d]
fDs <- fD[range_m]
bDs <- bD[range_m]

######################################################################
#Make Daily Data Complete
######################################################################
#Method Summary: adding NA's in unused dates until data is complete

#1. Calculate number of na's needed to complete data
add_na <- (nrow(yDs)*65)-nrow(xmDs)

#2. Generate dates and keep only enough weekend dates equal to number of na's needed to be inserted
d <- seq(as.Date(paste(sample_start_yr, sample_start_day_d, sep="")), by = 'day', length = ceiling(add_na*3.5))
d <- d[isWeekend(d, 1:5)]
d <- d[1:add_na]

#3. Create xts vector
ee <- suppressWarnings(as.xts(as.numeric(rep(NA,add_na)), order.by = d))

#4. Merge with unprepared x to add dates
#WARNING: Doesn't check if these weekends already exist. they could exists and thus prevent the data from becoming complete
xmDs <- merge.xts(xmDs, ee, join = "outer")[,-ncol(xmDs)-1]

#5. Test if sufficient dates were indeed added
if(length(xmDs)%%65 == 0) {
  is_xmD_complete <- TRUE
}else{
  is_xmD_complete <- FALSE
  end()
}

#Deal with NA's
#Insample
xmDs <- na.locf(xmDs)
xmDs <- na.locf(xmDs,fromLast = TRUE)
#Total Sample
xmD <- na.locf(xmD)
xmD <- na.locf(xmD,fromLast = TRUE)


######################################################################
#Rename Columns
######################################################################

colnames(yDs) <- "UJ"
colnames(xmD) <- c("Nikkei", "SP500", "UB", "BY")
colnames(xmDs) <- c("Nikkei", "SP500", "UB", "BY")

######################################################################
#Declare Functions
######################################################################
if(firstrun==TRUE){
  #() Undifference Forecasts 
  ###########################
  undiff <- function(forecast,h,fa){
    ff <- xts(forecast[1]+fa, index(fa)+1/4)
    if(h == 2){
      ff <- c(ff, xts(ff+forecast[2], index(ff)+1/4))
    }
    return(ff)
  }
  
  #() Calculate Forecast Errors
  #############################
  ferrors <- function(actual,forecasted){
    #RMSE
    rmse <- sqrt(mean((actual-forecasted)^2))
    #MAPE
    mape <- 100*((sum(abs((actual-forecasted)/actual)))/length(forecasted))
    #MAE
    mae <- mean(abs(actual-forecasted))  
    
    return(list("mape"=mape,"rmse"=rmse,"mae"=mae))
  }
  
  #() Convert Differenced Confidence Intervals to Levels
  ######################################################
  difftoLvCI <- function(fm,lastY,model){
    upper <- fm$upper #establish data type and dimensions
    lower <- fm$lower
    if(nrow(fm$upper)==1){
      upper <- fm$upper+as.numeric(lastY)
      lower <- fm$lower+as.numeric(lastY)
      a<-matrix(c(lower[,1],upper[,1],lower[,2],upper[,2]),nrow=1, ncol=4, byrow=FALSE)
      rownames(a)<-c(paste(model,"h1", sep = '-'))    
    }else{
      upper[1,] <- fm$upper[1,]+as.numeric(lastY)
      upper[2,] <- fm$upper[2,]+upper[1,]
      lower[1,] <- fm$lower[1,]+as.numeric(lastY)
      lower[2,] <- fm$lower[2,]+lower[1,]
      a<-matrix(c(lower[,1],upper[,1],lower[,2],upper[,2]),nrow=2, ncol=4, byrow=FALSE)
      rownames(a)<-c(paste(model,"h1", sep = '-'), paste(model,"h2", sep = '-'))  
    }
    colnames(a)<-c("Lo 80", "Hi 80", "Lo 95","Hi 95")
    return(a)
  }
 
  #() Calculate AIC
  #################
  getAic <- function(regr){
    ssr <- sum((regr$residuals^2))
    k <- length(regr$coefficients)
    n <- regr$nobs
    df <- n-k
    sigmahatsquared <- ssr/df
    aic <- n*log(sigmahatsquared)+(2*k)
    return(aic)
  }
  firstrun<-FALSE
}
######################################################################
#Convert to frequency
######################################################################
xmDsq <- to.quarterly(xmDs, indexAt = "yearqtr", OHLC = FALSE)
fDsq <- to.quarterly(fDs,indexAt = "yearqtr", OHLC = FALSE)
tail(fDs)
tail(fDsq)

xts.testm <- xts(rnorm(440*12, mean=0, sd=10), order.by=timeBasedSeq(155001/1989))
xts.testq<-to.quarterly(xts.testm, OHLC = FALSE)
tail(xts.testm)
tail(xts.testq)
######################################################################
######################################################################
#Estimations
######################################################################
######################################################################
######################################################################
#Convert Variables to ts
######################################################################

yDs_ts <- as.ts(yDs)
trend <- c(1:nrow(yDs))
x <- as.ts(xmDsq$Nikkei)
w <- as.ts(xmDsq$UB)
v <- as.ts(xmDsq$BY)
z <- as.ts(xmDsq$SP500)
f <- as.ts(fDsq)
#b <- as.ts(bDs)
h <- 1

######################################################################
#Get Data After T (New Data)
######################################################################

nd <- FALSE
if(nd==TRUE){
  #Need h*m observations of x data immediately after insample end range.
  ndstart <- paste(new_data_start,"/", sep="")
  ndstart_m <- paste(sample_end_m,"/", sep="")
  
  #1) Get all data. 2) Keep only T - T+h
  xn <- as.ts(xmD$Nikkei[ndstart])[1:(65*h)]
  wn <- as.ts(xmD$UB[ndstart])[1:(65*h)]
  vn <- as.ts(xmD$BY[ndstart])[1:(65*h)]
  zn <- as.ts(xmD$SP500[ndstart])[1:(65*h)]
  fn <- as.ts(fD[ndstart_m])[1:(3*h)]
  
  trendn <- length(yDs) + (1:h)
  faD <- yD[ndstart][1:h]
  fa <- y[ndstart][1:h]
}
###################################################
#REGRESSIONS AND FORECASTS
###################################################

########################
#MIDAS
########################

#No new data
############

#MIDAS regression
mnn <- midas_r(yDs_ts ~ trend 
               + mls(yDs_ts,h + 1:2, 1)
               + mls(f, (3*h) + 0:12, m = 3, nealmon)
               + mls(w, (65*h) + 0:100, m = 65, nealmon)
               + mls(v, (65*h) + 0:100, m = 65, nealmon)
               + mls(x, (65*h) + 0:100, m = 65, nealmon)
               + mls(z, (65*h) + 0:100, m = 65, nealmon),
               start = list(
                 f = c(0, 0),
                 w = c(0, 0),
                 v = c(0, 0),
                 x = c(0, 0),
                 z = c(0, 0)
               )
)
summary(mnn)
#mnn$midas_coefficients

#MIDAS Forecast
fnn <- forecast(mnn, newdata = list(trend = trendn,
                                    f = rep(NA,3*h),
                                    w = rep(NA,65*h),
                                    v = rep(NA,65*h),
                                    x = rep(NA,65*h),
                                    z = rep(NA,65*h)
                                  ),
                                se = TRUE, method="dynamic"
)
if(nd==TRUE){
  #New data after time T
  ######################
  #MIDAS Regression: new data
  mnd <- midas_r(yDs_ts ~ trend
                 + mls(yDs_ts, 1:3, 1,"*")
                 + mls(f, 0:11, m = 3, nealmon)
                 + mls(w, 0:110, m = 65, nealmon)
                 + mls(v,  0:110, m = 65, nealmon)
                 + mls(x, 0:110, m = 65, nealmon)
                 + mls(z, 0:110, m = 65, nealmon),
                 start = list(
                   f = c(0, 0),
                   w = c(0, 0),
                   v = c(0, 0),
                   x = c(0, 0),
                   z = c(0, 0)
                 )
  )
  summary(mnd)
  #mnd$midas_coefficients
  
  #MIDAS Forecast: new data
  fnd <- forecast(mnd, newdata = list(
                                      trend = trendn,
                                      f = fn,
                                      w = wn,
                                      v = vn,
                                      x = xn,
                                      z = zn
                                    ),
                                  se = TRUE, method="dynamic")
}


##############################
#AR
##############################
#Manually find lowest AIC

#ARz
#p <- SelectModel(yDs_ts, lag.max = 17, ARModel = "ARz", Criterion = "AIC")[[1]]$p
#ftar <- FitAR(yDs_ts,p) 

#AIC
arfit <- SelectModel(yDs_ts, ARModel = "AR", Criterion = "AIC")
p<-arfit[1,1]
#Use these for matching purposes. They should match with ar.mle() and forecast()
#ftar <- FitAR(yDs_ts,p, demean = FALSE)
#predict(ftar, n.ahead=h)

#use ar.mle() with p determined by SelectModel so that forecast() could be used and forecast errors, C.I's functions work
yar <- ar.mle(as.ts(yDs), aic = FALSE, order.max = p, demean = FALSE, se.fit = TRUE)
far <- forecast(yar, h = h)

#Get AIC
#####################
mnnAIC <- getAic(mnn)
mndAIC <- getAic(mnd)
arAIC<-arfit[1,2]
aics <- matrix(c(mnnAIC,mndAIC,arAIC), nrow=3, ncol=1, byrow = FALSE)
rownames(aics) <- c("mnn","mnd","ar")
colnames(aics) <-("AIC")

##############################################################
#Calculate Forecast Errors
##############################################################

#Undifference Forecasts
#######################
YLastSampleObs <- y[last(index(yDs))]
fnnMean <- undiff(fnn$mean,h,YLastSampleObs)
fndMean <- undiff(fnd$mean,h,YLastSampleObs)
farMean <- undiff(far$mean,h,YLastSampleObs)

#Get Undifferenced Confidence Intervals
#######################################
fnnCI <- difftoLvCI(fnn,YLastSampleObs,"fnn")
fndCI <- difftoLvCI(fnd,YLastSampleObs,"fnd")
farCI <- difftoLvCI(far,YLastSampleObs,"far")

#Get Forecast errors
####################
fnnFe <-ferrors(fa,fnnMean)
fndFe <-ferrors(fa,fndMean)
farFe <-ferrors(fa,farMean)

#Create Matrix of Forecasts
#################################
mf<-matrix(c(fnnMean,fndMean,farMean,fa),nrow=4,ncol=h,byrow=FALSE)

#Name
rownames(mf)<-c("fnn","fnd","far","Actual")
if(h==2){ colnames(mf)<-c(index(fnnMean[1]),index(fnnMean[2]))}else
{colnames(mf)<-c(index(fnnMean[1]))}

#Create Matrix of Forecast Errors
#################################
fe <- matrix(c(fnnFe,fndFe,farFe), nrow=3, ncol=3, byrow = TRUE)

rownames(fe)<-c("fnn","fnd","far")
colnames(fe)<-c("MAPE","RMSE","MAE")

#Display Results
aics
mf
fnnCI
fndCI
farCI
fe