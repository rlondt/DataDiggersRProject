
.DataDiggersPackageOptions <- new.env()
# zzz.r
# initializing package variables
.onLoad<- function(libname, pkgname){
  futile.logger::flog.debug(paste("onload executed: ", pkgname))
}

.onAttach<- function(libname,pkgname){
  packageStartupMessage("Go DataDiggers!!!")
}
