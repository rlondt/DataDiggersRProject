##
# Met dit script wordt de DataDiggersPackage opnieuw gebouwd en in de locale repo verwerkt
# Zorg voor een nieuw versienummer!!

source("./init.R")

org_repo <- c(CRAN = "http://cran.us.r-project.org")
pkgTypes <- c("source", "win.binary")
pth <- "D://miniCRAN"

# verwijderen package uit het environment/sessie
detach(package:DataDiggersPackage, unload=TRUE)
remove.packages("DataDiggersPackage")

# verwijder package in minicran
pkgFile <- dir(path = paste(pth,"/src/contrib/", sep = ""), pattern = "DataDiggers*")
file.remove(pkgFile)

# compileren en bouwen package
document("DataDiggersPackage")
build("DataDiggersPackage")

# toevoegen package aan minicran
addLocalPackage(c("DataDiggersPackage"), ".", "D:/miniCRAN", build = FALSE)

# verwijderen package uit sessie
detach(package:DataDiggersPackage, unload=TRUE)

# installeren package vanuit cran
install.packages("DataDiggersPackage", repos = 'file:///D:/minicran' )

