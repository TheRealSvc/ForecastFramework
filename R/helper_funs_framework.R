
#' Helper to detect the neccessity for a conversion
#'
#' This is a helper function to determine based on the prediction function if a conversion to time-series is necessary
#' For instance forecast::arima with seasonality expects a timesseries input with given frequency
#' @param inFun a function
#' @return flag
#' @export
needsTSconversion <- function(inFun) {
  # the following lines are to determine wether timeseries conversion (freq = 12 month) is necessary
  isETS <- "model" %in% unlist(names(formals(inFun)))
  isARIMA <- "seasonal" %in% unlist(names(formals(inFun)))
  if (isARIMA) {
    tsTrans <- (!is.null(formals(inFun)$seasonal) & !(identical(eval(formals(inFun)$seasonal),c(0,0,0))))
  } else if(isETS) {
    tsTrans <- substr(formals(inFun)$model,3,3) %in% c("A","M")
  } else {
    tsTrans <- FALSE
  }
  return(tsTrans)
}



#' split the time series and creates a list tree as the main object for further operations
#'
#' @param lTrain length of training intervall in a walk forward validation
#' @param lValid length of validation data for backtesting
#' @return  all splits in form of a list "out\[\[name\]\]\[\[train\]\]" und "out\[\[name\]\]\[\[valdation\]\]"
#' @export
CreateWalkForwardFuns <- function(lTrain, lValid=1) { #}, outlierFun) {
  force(lTrain)
  force(lValid)

  outCreateFun <- function(tsDat, TrainL=lTrain, ValiL=lValid) {
    tsL <- length(tsDat)
    totalL <- TrainL + ValiL
    startInd <- 1
    EndInd <- totalL
    outList <- list()
    splitId <- 1
    outList[["Original"]] <- tsDat
    MetaDat <- attributes(tsDat)

    while (EndInd <= tsL) {
      outList[["Train"]][[as.character(splitId)]] <- c(tsDat[startInd:(startInd+TrainL-2)], tsDat[(startInd+lTrain+ValiL-2)]) # last val is original val
      outList[["Vali"]][[as.character(splitId)]] <- tsDat[(startInd+TrainL):(startInd+lTrain+ValiL-1)]
      startInd <- startInd + 1
      EndInd <- EndInd + 1
      splitId <- splitId + 1
    }
    outList[["Vali"]] <- as.list(outList[["Vali"]])
    #class(outList) <- append(class(outList),"ListTree")
    return(outList)
  }
  formals(outCreateFun)[2] <- lTrain
  return(outCreateFun)
}



#' creates a list of model functions based on parametrization
#'
#' This function serves as a wrapper for the Function Factory FunGen to generate a multiple of forecasting functions
#' @param paramsList a names list containing the non default parameters for a model class
#' @return a list of functions
#' @export
CreateModelFuns <-  function(paramsList) {
  force(paramsList)
  outFuns <- list()

  for (i in 1:length(paramsList)) {
    outFuns[[as.character(i)]] <- FunGen(params=paramsList[[i]])
    attr(outFuns[[as.character(i)]],"sigLevels") <-  paramsList[[i]][["sigLevels"]]
  }
  names(outFuns) <- names(paramsList)
  return(outFuns)
}


#' Function generator that returns a model functions for Model fitting
#'
#' This is to create a unique way to define functions that can be called exactly in the same way
#' The returned function is such, that the output can be used by the subsequent call to forecast
#' @param params  a named listwith one element containing the non-default params
#' @return a function
#' @export
FunGen <- function(params) {
  force(params)

  if (params[["type"]]=="arima") {
    params[["type"]] <- NULL
    sigLevels <- params[["sigLevels"]]
    params[["sigLevels"]] <- NULL

    arimaFormals <- formals(Arima)
    arimaFormals[which(names(arimaFormals) %in% names(params))] <- params[order(match(names(params),names(arimaFormals)))]
    formals(Arima) <- arimaFormals
    attr(Arima,"sigLevels") <- sigLevels
    return(Arima)

  } else if (params[["type"]]=="ets") {
    params[["type"]] <- NULL
    sigLevels <- params[["sigLevels"]]
    params[["sigLevels"]] <- NULL
    etsFormals <- formals(ets)
    etsFormals[which(names(etsFormals) %in% names(params))] <- params[order(match(names(params),names(etsFormals)))]
    formals(ets) <- etsFormals
    attr(ets,"sigLevels") <- sigLevels
    return(ets)

  } else if (params[["type"]]=="lastval") {
    sigLevels <- params[["sigLevels"]]
    lastval <- function(y, sigLevel1, sigLevel2) {
      force(sigLevel1)
      force(sigLevel2)

      e <- y[length(y)]
      s <- sd(tail(y,12)) # only last 12 month for standard deviation used

      s1 <- s*qnorm((100-(100-sigLevel1)*0.5)/100,sd=1, mean=0)
      s2 <- s*qnorm((100-(100-sigLevel2)*0.5)/100,sd=1, mean=0)
      s70 <- s1  # + 10 # can make sense
      s85 <- s2  # + 10
      a <- list(`Point Forecast`=e,`Lo 70` =e-s70, `Hi 70`=e+s70, `Lo 85`=e-s85, `Hi 85`=e+s85)
      class(a) <- append(class(a),"lastval")
      attr(a,"sigLevels") <- sigLevels
      return(a)
    }
    # attr(lastval, "sigLevels") <- sigLevels
    formals(lastval)[2:3] <- list(sigLevel1=sigLevels[1], sigLevel2=sigLevels[2])
    attr(lastval,"sigLevels") <- sigLevels
    return(lastval)

  } else if (params[["type"]]=="linreg") {
    sigLevels <- params[["sigLevels"]]
    params[["sigLevels"]] <- NULL

    linreg <- function(y, sigLevel1, sigLevel2) {
      force(sigLevel1)
      force(sigLevel2)
      yy <- tail(y, params[["numobs"]])
      xx <- c(1:params[["numobs"]])
      lmDat <- data.frame(x=xx, y=yy)
      lm2 <- lm(y~x, data=lmDat, x = T)
      #attr(lm2,"sigLevels") <- c(sigLevel1, sigLevel2)
      return(lm2)
    }
    class(linreg) <- append(class(linreg),"linreg")
    formals(linreg)[2:3] <- list(sigLevel1=sigLevels[1], sigLevel2=sigLevels[2])
    attr(linreg,"sigLevels") <- sigLevels
    return(linreg)

  } else if (params[["type"]]=="percentage") {
    return(function(y) {
      bandPM <- params[["bandPM"]]
      sigLevels <- params[["sigLevels"]]# bandPM --> e.g. >10 Percent Yellow, >20 Percent Rrd
      e <- y[length(y)]
      s70 <- abs(bandPM*e/100)
      print(s70)
      s85 <- abs(bandPM*2*e/100)
      eSeries <- e + seq(1,6,1)*1
      low70Series <- e-seq(1,1,1)*s70
      high70Series <-e + seq(1,6,1)*s70
      low85Series <-  e-seq(1,6,1)*s85
      high85Series <- e+seq(1,6,1)*s85

      a <- list(`Point Forecast`=eSeries,`Lo 70` =low70Series, `Hi 70`=high70Series, `Lo 85`=low85Series, `Hi 85`=high85Series)
      class(a) <- append(class(a),"percentage")
      attr(a,"sigLevels") <- sigLevels
      return(a)
    })
  }
}


#' Function that serves as a warpper to forecasts
#'
#' This is to create a unique way to create a forecast object. The forecast object are similar independently of the class of the fitted model
#' @param Model a fitted Model
#' @return forecast object
#' @param ValiLenght length of Validation data for backtesting
#' @export
forecastGen <- function(fittedModel, ValiLength=1) {
#browser()
  if (any(class(fittedModel) %in% c("Arima" ,"ets"))) {
    tryCatch({forecast(object = fittedModel, h = ValiLength, level=attr(fittedModel,"sigLevels")) }, error=function(err) {"error in Arima/ETS Forecast"})
  } else if (any(class(fittedModel) %in% "lm")) {
    tryCatch({ forecast.lm(fittedModel, ValiLength = ValiLength)}, error=function(err) {"error in LM-Forecast"})

  } else if (any(class(fittedModel) %in% c("lastval"))) { # for some simple functions the fittedModel is already the result of the forecast
    fittedModel$`Point Forecast` <- rep(fittedModel$`Point Forecast`, ValidLength)
    fittedModel$`Lo 70` <- rep(fittedModel$`Lo 70`, ValidLength) + seq(from = 1, to = ValiLength, by = 1)*(fittedModel$`Lo 70`-fittedModel$`Point Forecast`)
    fittedModel$`Hi 70` <- rep(fittedModel$`Hi 70`, ValidLength) + seq(from = 1, to = ValiLength, by = 1)*(fittedModel$`Hi 70`-fittedModel$`Point Forecast`)
    fittedModel$`Lo 85` <- rep(fittedModel$`Lo 85`, ValidLength) + seq(from = 1, to = ValiLength, by = 1)*(fittedModel$`Lo 85`-fittedModel$`Point Forecast`)
    fittedModel$`Hi 85` <- rep(fittedModel$`Hi 85`, ValidLength) + seq(from = 1, to = ValiLength, by = 1)*(fittedModel$`Hi 85`-fittedModel$`Point Forecast`)

    return(fittedModel)
  } else if (any(class(fittedModel) %in% c("percentage"))) {
    return(fittedModel)
  }
}


#' this is  a wrapper around the lm function in order to generate a result similar to forecast(ArimaObject)
#'
#' Note: in the function are two hidden parameters for calculating the s70 and s85 confidence intervals
#' @param Model a fitted model from a lm call
#' @param ValiLength length of Validation data for backtesting
#' @return a list containing the forecast result
#' @keywords internal
forecast.lm <- function(fittedModel,ValiLength=1) {
  sigLevels <- attr(fittedModel, "sigLevels")
  s1 <- qnorm((100-(100-sigLevels[1])*0.5)/100,sd=1, mean=0)
  s2 <- qnorm((100-(100-sigLevels[2])*0.5)/100,sd=1, mean=0)

  ll <- length(fittedModel$fitted.values)
  pred <- predict(object = fittedModel, newdata = list(x=c((ll+1):(ll+ValiLength))), se.fit = T)
  e <- round(pred$fit)
  s70 <- round(s1*pred$se.fit*sqrt(ll))  # mvp version ... needs check !!!
  s85 <- round(s2*pred$se.fit*sqrt(ll))
  out <- list(`Point Forecast`=e,`Lo 70` =e-s70-5, `Hi 70`=e+s70+5, `Lo 85`=e-s85-10, `Hi 85`=e+s85+10)
  attr(out,"sigLevels") <- sigLevels
  return(out)
}



#' Plots the time series for a given account and model and split
#'
#' This function identifies the correct account in the tree-list and displays all forecasts with confidence intervalls in a single plot
#' @param accountName name of account to be plotted
#' @param modelName name of the model result to be plotted as defined in the modelParams
#' @param splitDat fitted list tree
#' @param split index of the split to be plotted
#' @export
plotSingleModelSingleAccount <- function(accountName, splitDat , modelName = "lastval", split=1) {
  dat <- splitDat[[accountName]]

  forecastSeries = c(dat$Train[[split]], dat$Prediction[[modelName]]$Predicted[[split]])
  l70 <- c(dat$Train[[split]], dat$Prediction[[modelName]]$L70[[split]])
  l85 <- c(dat$Train[[split]], dat$Prediction[[modelName]]$L85[[split]])
  h70 <- c(dat$Train[[split]], dat$Prediction[[modelName]]$H70[[split]])
  h85 <- c(dat$Train[[split]], dat$Prediction[[modelName]]$H85[[split]])

  xx <- c(split:(split+length(l70)-1))
  original <- dat$Original[xx]

  maxi <- max(h85)[1]
  mini <- min(l85)[1]

  plot( c(split:(length(forecastSeries)+split-1)), forecastSeries,col="blue", type="line",lwd=2,  ylim=c(mini,maxi), axes=FALSE, frame.plot=TRUE,ylab="Y", xlab="Time")

  par(new=TRUE)
  plot(xx, l70,col="green", type="line", ylim=c(mini,maxi), axes=FALSE, frame.plot=TRUE,ylab="", xlab="")
  par(new=TRUE)
  plot(xx, h70, col="green", type="line",ylim=c(mini,maxi), axes=FALSE, frame.plot=TRUE,ylab="", xlab="")
  par(new=TRUE)
  plot(xx, l85,col="red", type="line", ylim=c(mini,maxi),axes=FALSE, frame.plot=TRUE,ylab="", xlab="")
  par(new=TRUE)
  plot(xx, h85,col="red", type="line", ylim=c(mini,maxi), axes=FALSE, frame.plot=TRUE,ylab="", xlab="")
  par(new=TRUE)
  plot(xx, original,col="black", type="line", lwd=2,  ylim=c(mini,maxi), yaxt = "n",ylab="", xlab="")
  legend(x = "topleft",          # Position
         legend = c("orig", "forecast", "70%","85%"),  # Legend texts
         lty = c(1,1,1,1),           # Line types
         col = c("black","blue","green","red"),           # Line colors
         lwd = c(2,2,1,1))                 # Line width

}



#' Do the prediction for training based on the list-tree structure and the modelFitFuns
#'
#' This function enriches the list tree structure with the forecast results
#' Step 1: The models in modelFitFuns are fitted to alle the training splits
#' Step 2: The forecast is performed
#' Step 3: Results are attached to the list tree
#' @param splitDat list-tree
#' @param modelFitFuns list of generated model funs
#' @param ValiLength length of Validation data for backtesting
#' @return the list tree with addional information
#' @export
DoPrediction <- function(splitDat=splitDat, modelFitFuns=modelFitFuns, ValiLength=1) {

    for (kontoName in names(splitDat)) {
      print(paste("Account Name:", kontoName))
      modNum<- names(modelFitFuns)

      for (modNo in modNum) {
        # the following lines are to determine wether timeseries conversion (freq = 12 month) is necessary
        tsTrans <- needsTSconversion(modelFitFuns[[modNo]])

        if(tsTrans) {
          fittedModelParams <- lapply(splitDat[[kontoName ]][["Train"]], FUN=function(x) { tryCatch({ modelFitFuns[[modNo]](y=ts(x,frequency = 12)) }, error=function(err) { return("error") })})
          fittedModelParams <- lapply(fittedModelParams, 'attr<-', which="sigLevels",value= attr(modelFitFuns[[modNo]],"sigLevels"))
        } else {
          fittedModelParams <- lapply(splitDat[[kontoName ]][["Train"]], FUN=function(x) { tryCatch({ modelFitFuns[[modNo]](y=x) }, error=function(err) { return("error") })})
          fittedModelParams <- lapply(fittedModelParams, 'attr<-', which="sigLevels",value= attr(modelFitFuns[[modNo]],"sigLevels"))
        }
        foreCastVals <- lapply(fittedModelParams, function(x) { forecastGen(x, ValiLength=ValiLength)} )
        capture.output(  aaa <- lapply(foreCastVals, print), file="NUL" )

        splitDat[[kontoName]][["Prediction"]][[modNo]][["Predicted"]] <- lapply(aaa, FUN=function(x) { x[[1]] }) # add the forecast to the tree
        splitDat[[kontoName]][["Prediction"]][[modNo]][["L70"]] <- lapply(aaa, FUN=function(x) { x[[2]] }) # add the forecast to the tree
        splitDat[[kontoName]][["Prediction"]][[modNo]][["L85"]] <- lapply(aaa, FUN=function(x) { x[[4]] }) # add the forecast to the tree
        splitDat[[kontoName]][["Prediction"]][[modNo]][["H70"]] <- lapply(aaa, FUN=function(x) { x[[3]] }) # add the forecast to the tree
        splitDat[[kontoName]][["Prediction"]][[modNo]][["H85"]] <- lapply(aaa, FUN=function(x) { x[[5]] }) # add the forecast to the tree
        splitDat[[kontoName]][["Prediction"]][[modNo]][["Diff"]] <- Map('-',splitDat[[kontoName]][["Prediction"]][[modNo]][["Predicted"]],
                                                                                     splitDat[[kontoName]][["Vali"]])
        }
  }
  return(splitDat)
}

