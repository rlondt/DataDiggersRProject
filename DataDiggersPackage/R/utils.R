# Utils
#' @export
getLocationNaam <- function(naam, checkExists=TRUE){
  stopifnot(is.logical(checkExists))
  werkdir = get("werkdir", envir=.DataDiggersPackageOptions)
  locatie = paste(werkdir, naam, sep = "/")
  if (!file.exists(locatie)&checkExists){
    futile.logger::flog.error(paste("Bestand niet gevonden: ", locatie))
    stop()
  }
  futile.logger::flog.debug(paste("Locatie: ", locatie))
  locatie
}

#' @export
setWorkdir <- function(directory){
  assign("werkdir", directory,  envir=.DataDiggersPackageOptions)
  invisible()
}

#' @export
dumpRDS <- function(object, naam, ...){
  saveRDS(object = object, file = getLocationNaam(naam, checkExists = FALSE), ...)
  invisible()
}

#' @export
readCSV <- function(naam, ...){
  #  werkdirOld <- getwd()
  #  setwd(get("werkdir", envir=.DataDiggersPackageOptions))
  csv <- read.csv2(file = getLocationNaam(naam), ...)
  #  setwd(werkdirOld)
  csv
}

#' @export
readRDSdd <- function(naam, ...){
  readRDS(getLocationNaam(naam), ...)  
}

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ret <-ifelse(as.character(x)!="", x, NA)
  ret <-ifelse(as.character(x)!="NA", x, NA)
  ret <-ifelse(as.character(x)!=" ", x, NA)
  ret
}