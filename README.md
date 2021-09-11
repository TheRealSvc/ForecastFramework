# ForecastFramework
A simple framework to perform univariate forecasting in R 

The idea is to generate a simple, yet scalable framework to register custom univariate forecasting algorithms to keep the "main_script" clean. 
This is achieved by working with a "function factory pattern" to generate all relevant functions (preproc, outlier, fitting and prediction ...) functions in a list. Like this a highly "function-parallel" code can be written (as opposed to data-parallel). Hence this pattern might be valuable for small-data/big-compute scenarious.

Currently the software architecture is not final and the idea doesnt shine through nicely. Future work will get it there - hopefully :-) 


# main workflow script ... just execute it :-) 

```{r class.source="bg-danger", class.output="bg-warning"}
source("helper_funs_framework.R")
source("helper_funs_pattern.R")
library(e1071)
library(forecast)


# --------- 1 Parametrization --------------------------------
TrainLength <- 48 # length of the training data for the splits 
ValidLength <- 6 # length of Validation duration for backtesting 


# parametrize models --> this will go as argument into the fitting function ... this is up to you what to define here ! 
# new models must be added in the FunGen function in the helper_funs_framework 
sigLevels <- c(80,95)
modelParams <- list("lastval"=list(type="lastval", sigLevels=sigLevels),
                    "LinReg"=list(type="linreg", numobs=3, sigLevels=sigLevels), 
                    "ets no trend season no damp"=list(type="ets",model="ANA", damped=FALSE, sigLevels=sigLevels),
                    "Arima (1,0,0) with drift"= list(type="arima", order=c(1,0,0), include.drift=1, include.mean=1, sigLevels=sigLevels),
                    "percentage"=list(type="percentage",bandPM=10, sigLevels=sigLevels))
#---------------------------------------------------------


# ---- 2 generate some testdata in the required data representation  --------
dat <- as.data.frame(matrix(runif(600,-10,10), ncol=10, nrow=60))

randomFun <- function(x) {
  a = runif(1,-1,1) 
  b = runif(1,-10,10)
  sinPeriodicity = runif(1,0,180)
  sinAmp = runif(1,10,50)
  return( x + a*seq(1,length(x),1) +b + sin(2*pi*seq(1,length(x),1)/sinPeriodicity)*sinAmp )
}

for(i in 1:ncol(dat)) {
  dat[,i] <-  randomFun(dat[,i]) 
}

colnames(dat) <- c("name" , paste0("timeseriesNo_",1:(ncol(dat)-1)))  # name
dat[,1] <- paste0("t_",1:nrow(dat))
# -------------------------------------------------------------------------



# ------ 3 creation of the models used for fitting to the data --------------
modelFitFuns <- CreateModelFuns(paramsList = modelParams)  # output is a list of functions by using a "function factory pattern" (no calculation happens here)


# creation of data splits (train and Validation) and use a list tree as data representation 
# Note: as many train/valid sets will be generated as possible given the data, TrainLength and ValidLength. Its all stored in a list tree for further enrichment   
splitFun <- CreateWalkForwardFuns(lTrain=TrainLength, lValid=ValidLength) 
splitDat <- lapply(dat[,2:ncol(dat)], splitFun)  

# actual prediction: calling all the fitting functions on all the splits of all the timeseries.
# Note: this can be easily parallelized (though it is not yet in that shape)
splitDat2 <- DoPrediction(splitDat=splitDat, modelFitFuns, ValiLength = ValidLength) # model fitting and forecast as a batch job


# ---- 3 some plotting  --------
# just play around. A wrapper around this could make sense in order to show more info in one plot    
graphics.off()
plotSingleModelSingleAccount(accountName = "timeseriesNo_1", splitDat = splitDat2, modelName = "ets no trend season no damp", split=1)
``` 

![example](https://github.com/TheRealSvc/ForecastFramework/blob/main/plotexample.png)
