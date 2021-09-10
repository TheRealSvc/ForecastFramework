# ForecastFramework
A simple framework pattern to perform univariate forecasting in R 

The idea is to generate a simple, yet scalable framework to register no custom univariate forecasting algoriths to keep the "main_script" clean. 
This is achieved by working with a "function factory pattern" to generate all relevant functions (preproc, outlier, fitting and prediction ...) functions in a list. Like this a highly "function-parallel" code can be written (as opposed to data-parallel). Hence this pattern might be valuable for small-data/big-compute scenarious.

Just execute main_workflow.R to see what it does: 
![example](https://github.com/TheRealSvc/ForecastFramework/blob/main/plotexample.png)
