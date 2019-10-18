#
# Generic file to set the environment
init <- function(){
  werkdir <- "D:/datafiles"
  if( werkdir != getwd()){
    
    library(devtools)
    library(roxygen2)
    document("DataDiggersPackage")
    #install.packages("DataDiggersPackage")
    library(DataDiggersPackage)
    initializeDQScoringFramework()
    addScoreToDQFramework(CONSISTENTIE, 4, 5)
    
    futile.logger::flog.threshold(futile.logger::DEBUG)
    
    
    setwd('D:/datafiles')
    
    #flog.threshold(DEBUG)
    
    packages <- c("elastic"
                  ,"tidyverse"
                  ,"naniar"
                  ,"dplyr"
                  ,"caret"
                  ,"doSNOW"
                  ,"sf"
                  ,"futile.logger"
                  ,"lubridate"
                  ,"openxlsx"
                  #              ,"RJDBC"
                  ,"anomalize"
                  ,"VIM"
                  , "reshape2"
                  ,"shiny"
                  ,"naniar"
                  , "ggplot2"
                  , "tidyquant"
                  , "sqldf"
                  , "devtools"
                  , "roxygen2"
    )
    
    ## Installeer packages
    for (p in packages) {
      if (p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p, repos = 'http://cran.us.r-project.org')
      }
    }
    
    ## Laad the packages
    for (p in packages){
      suppressPackageStartupMessages(
        library(p, quietly = TRUE, character.only = TRUE ) 
      )
    }
    if (exists("calendarHeat")){
      source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
    }
    
    # Gebruik meerdere CPU's
    cl <- makeCluster(4, type = "SOCK")
    registerDoSNOW(cl)
    
    # no scientific notation
    options(scipen = 999)
    
  }
}

# 
# transposeWorkflowDataFrame <- function (data, measureVars, dcastFormula){
#   # converteren posix naar numeric
#   for (i in names(data)){
#     if(is.POSIXct(data[,i])){
#       flog.debug(i)
#       data[,i] = unclass(data[,i])
#       flog.debug(class(data[,i]))
#     }
#   }
#   flog.debug(all.vars(dcastFormula))
#   flog.debug(colnames(data))
#   meltDF <- melt(data, id.vars = all.vars(dcastFormula), measure.vars = measureVars)
#   flog.debug(colnames(meltDF))
#   dcastFormula <- update(dcastFormula, ~.+variable)
#   tempDF <- dcast(meltDF, dcastFormula, value.var = "value")
#   # converteren naar posix
#   for (i in names(tempDF)){
#     if(is.numeric(tempDF[,i])){
#       if(!str_detect(i, "NormDoorlooptijd")){
#         flog.debug(i)
#         tempDF[,i] = as.POSIXct(tempDF[,i], origin="1970-01-01")
#       }
#     }
#   }
#   tempDF
# }

do.call("init",list())
rm(init)
