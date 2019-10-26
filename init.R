## 
# setup of dataframes

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
# if (!exists("calendarHeat")){
#   source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
# }
if ("DataDiggersPackage" %in% rownames(installed.packages()) == FALSE) {
  install.packages("DataDiggersPackage", dependencies = TRUE, repos = 'file:///D://miniCRAN')
}
library(DataDiggersPackage)

#.libPaths()
# 
# install.packages("DataDiggersPackage", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
# library(DataDiggersPackage)
# 
# library(devtools)
# devtools::install_deps("DataDiggersPackage", dependencies = TRUE)
# build("DataDiggersPackage")
# addLocalPackage()
# 
# install("DataDiggersPackage", force = TRUE)
# 
# 
# # Generic file to set the environment
# init <- function(){
#     futile.logger::flog.threshold(futile.logger::DEBUG)
#     
#     
#     setwd('D:/datafiles')
#     
#     #flog.threshold(DEBUG)
#     
#     packages <- c("tidyverse"
#                   ,"naniar"
#                   ,"dplyr"
#                   ,"caret"
#                   ,"doSNOW"
#                   ,"sf"
#                   ,"futile.logger"
#                   ,"lubridate"
#                   ,"openxlsx"
#                   #              ,"RJDBC"
#                   ,"anomalize"
#                   ,"VIM"
#                   , "reshape2"
#                   ,"shiny"
#                   ,"naniar"
#                   , "ggplot2"
#                   , "tidyquant"
#                   , "sqldf"
#                   , "devtools"
#                   , "roxygen2"
#                   , "crul"
#     )
#     
#     ## Installeer packages
#     for (p in packages) {
#       if (p %in% rownames(installed.packages()) == FALSE) {
#         install.packages(p, dependencies = TRUE, repos = 'file:///D:/minicran', )
#       }
#     }
#     
#   
#     
#     ## Laad the packages
#     for (p in packages){
#       suppressPackageStartupMessages(
#         library(p, quietly = TRUE, character.only = TRUE ) 
#       )
#     }
#     if (exists("calendarHeat")){
#       source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
#     }
#     
#     # Gebruik meerdere CPU's
#     cl <- makeCluster(4, type = "SOCK")
#     registerDoSNOW(cl)
#     
#     # no scientific notation
#     options(scipen = 999)
# 
#     document("DataDiggersPackage")
#     library(DataDiggersPackage)
# 
#     detach(package:DataDiggersPackage, unload=TRUE)
#     document("DataDiggersPackage")
#     #install.packages("DataDiggersPackage", )
#     # library(DataDiggersPackage, verbose = TRUE)
#     startPreparation("D:/datafiles", dataframesToGlobalEnvironment = TRUE)
#  }
# 
# do.call("init",list())
# rm(init)
