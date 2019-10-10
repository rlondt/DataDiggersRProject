#
# Generic file to set the environment
init <- function(){
  werkdir <- "D:/datafiles"
  if( werkdir != getwd()){
    
    setwd('D:/datafiles')
    
    packages <- c("elastic"
                  ,"tidyverse"
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
    
    # Gebruik meerdere CPU's
    cl <- makeCluster(4, type = "SOCK")
    registerDoSNOW(cl)
    
    # no scientific notation
    options(scipen = 999)
    
  }
}

do.call("init",list())

rm(init)