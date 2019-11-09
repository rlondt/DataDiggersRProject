##
# Executing this file will update the local cran repository with 
# our own DataDigersPackage
# be sure to upgrade the version number 
source("./init.R")

org_repo <- c(CRAN = "http://cran.us.r-project.org")
pkgTypes <- c("source", "win.binary")
pth <- "D://miniCRAN"

detach(package:DataDiggersPackage, unload=TRUE)
remove.packages("DataDiggersPackage")

# delete package in minicran
pkgFile <- dir(path = paste(pth,"/src/contrib/", sep = ""), pattern = "DataDiggers*")
file.remove(pkgFile)

document("DataDiggersPackage")
build("DataDiggersPackage")
addLocalPackage(c("DataDiggersPackage"), ".", "D:/miniCRAN", build = FALSE)
detach(package:DataDiggersPackage, unload=TRUE)
install.packages("DataDiggersPackage", repos = 'file:///D:/minicran' )

