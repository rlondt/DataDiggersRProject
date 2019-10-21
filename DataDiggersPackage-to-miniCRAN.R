##
# Executing this file will update the local cran repository with 
# our own DataDigersPackage
# be sure to upgrade the version number 
source(./init.R)
install.packages("miniCRAN")
install.packages("roxygen2", repos = 'file:///D:/minicran' )


library(roxygen2)
library(devtools)
library("miniCRAN")

org_repo <- c(CRAN = "http://cran.us.r-project.org")
pkgTypes <- c("source", "win.binary")
pth <- "D://miniCRAN"
detach(package:DataDiggersPackage, unload=TRUE)
remove.packages("DataDiggersPackage")
document("DataDiggersPackage")
build("DataDiggersPackage")

addLocalPackage(c("DataDiggersPackage"), ".", "D:/miniCRAN", build = FALSE )
