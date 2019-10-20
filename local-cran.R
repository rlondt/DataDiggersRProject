install.packages("miniCRAN")
library("miniCRAN")

org_repo <- c(CRAN = "http://cran.us.r-project.org")
pkgTypes <- c("source", "win.binary")
pth <- "D://miniCRAN"

miniCRAN::updateRepoIndex(pth, type="win.binary")

# Specify list of packages to download
pkgs      <- c("tidyverse"
              ,"naniar"
              ,"dplyr"
              ,"caret"
              ,"doSNOW"
              ,"sf"
              ,"futile.logger"
              ,"lubridate"
              ,"openxlsx"
              ,"anomalize"
              ,"VIM"
              ,"formatR"
              ,"reshape2"
              ,"shiny"
              ,"naniar"
              ,"ggplot2"
              ,"tidyquant"
              ,"sqldf"
              ,"devtools"
              ,"roxygen2"
              ,"bupaR"
              ,"edeaR"
              ,"eventdataR"
              ,"processmapR"
              ,"processmonitR"
              ,"xesreadR"
              ,"petrinetR"
              ,"broom"
)

pkgList <- pkgDep(packages, repos = org_repo, type = "source", suggests = FALSE)

# Make repo for source and win.binary
makeRepo(pkgList, path = pth, repos = org_repo, type = c("source", "win.binary"))

# Check for available packages
pkgAvail(repos = "D:/miniCRAN", type = "win.binary")[, c(1:3, 5)]

addPackage(c("broom"), path="D:/miniCRAN", repos=org_repo)


for (p in packages) {
  #if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE, reppos = 'file:///D:/minicran',type = "win.binary" )
  #}
}

pkgs2 <- c("csvy", "feather", "fst", "hexView", "readODS", "rmatio")
for (p in pkgs2) {
  #if (p %in% rownames(installed.packages()) == FALSE) {
  install.packages(p, dependencies = TRUE, type = "win.binary" )
  #}
}

library(miniCRAN)
updateRepoIndex(pth, type = pkgTypes, Rversion = R.version)
install.packages("tidyverse", repos = 'file:///D:/minicran' )
install.packages("VIM", repos = 'file:///D:/minicran' )
install.packages("rio", repos = 'file:///D:/minicran' )
install.packages("carData", repos = 'file:///D:/minicran' )
install.packages("broom", repos = 'file:///D:/minicran' )

install.packages("tidyverse", repos = 'file:///D:/minicran' )
library(broom)
library(tidyverse)
library(rio)
library(VIM)
install.packages("roxygen2", repos = 'file:///D:/minicran' )
library(roxygen2)


install.packages("devtools", repos = 'file:///D:/minicran' )
library(devtools)
devtools::install_github("tidyverse/broom", dependencies = TRUE)
install.packages("DataDiggersPackage", dependencies = TRUE, repos = 'file:///D://miniCRAN')


document("DataDiggersPackage")
build("DataDiggersPackage")

addLocalPackage(c("DataDiggersPackage"), ".", "D:/miniCRAN", build = FALSE )
addLocalPackage(c("DataDiggersPackage"), ".", "D:/miniCRAN", build = FALSE )


library(DataDiggersPackage)
startPreparation("D:/datafiles", dataframesToGlobalEnvironment = TRUE)
