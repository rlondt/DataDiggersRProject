## 
# setup of libraries en packages

    packages <- c("DataDiggersPackage"
                  ,"tidyverse"
                  ,"naniar"
                  ,"dplyr"
                  ,"miniCRAN"
                  ,"doSNOW"
                  ,"sf"
                  ,"futile.logger"
                  ,"lubridate"
                  ,"openxlsx"
                  ,"anomalize"
                  ,"VIM"
                  ,"reshape2"
                  ,"shiny"
                  ,"naniar"
                  ,"ggplot2"
                  ,"tidyquant"
                  ,"sqldf"
                  ,"devtools"
                  ,"roxygen2"
    )

    ## Installeer packages
    for (p in packages) {
      if (p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p, dependencies = TRUE, repos = 'file:///D:/minicran', )
      }
    }



    ## Laad the packages
    for (p in packages){
      suppressPackageStartupMessages(
        library(p, quietly = TRUE, character.only = TRUE )
      )
    }


# install.packages("broom", dependencies = TRUE, repos = 'file:///D:/miniCRAN' )
# if ("devtools" %in% rownames(installed.packages()) == FALSE) {
#   install.packages("devtools", dependencies = TRUE, repos = 'file:///D:/miniCRAN', )
# }
if ("broom" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("tidyverse/broom", dependencies = TRUE, repos = 'file:///D:/miniCRAN')
}
suppressPackageStartupMessages(
  library("devtools", quietly = TRUE, character.only = TRUE ) 
)
if (!exists("calendarHeat")){
  source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
}
if ("DataDiggersPackage" %in% rownames(installed.packages()) == FALSE) {
  install.packages("DataDiggersPackage", dependencies = TRUE, repos = 'file:///D://miniCRAN')
}
library(DataDiggersPackage)
