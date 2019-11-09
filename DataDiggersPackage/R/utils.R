## Utils

#' Bepalen bestandsnaam en controleren of het bestand bestaat
#'
#' Bepalen bestandsnaam en controleren of het bestand bestaat
#' @param naam naam van het bestand
#' @param checkExists is er controle nodig om te kijken of het bestand bestaat
#' @keywords file local location
#' @export
getLocationNaam <- function(naam, checkExists=TRUE){
  stopifnot(is.logical(checkExists))
  werkdir = get("werkdir", envir=.DataDiggersPackageOptions)
  locatie = paste(werkdir, naam, sep = "/")
  if (!file.exists(locatie)&checkExists){
    futile.logger::flog.debug(paste("Bestand niet gevonden: ", locatie))
    stop(paste("Bestand niet gevonden: ", locatie))
  }
  futile.logger::flog.debug(paste("Locatie: ", locatie))
  locatie
}

#' A Function to specify the data location
#'
#' This function om de exacte locatie te achterhalen
#' @param directory naam van de basisdirectory
#' @keywords file local location
#' @export
setWorkdir <- function(directory){
  assign("werkdir", directory,  envir=.DataDiggersPackageOptions)
  invisible()
}

#' A Function to save the rds to a location whithin the datafiles-location
#'
#' This function om de exacte locatie te achterhalen
#' @param object naam van het object dat bewaard moet worden
#' @param naam naam van het bestand
#' @param ... overige parameters 
#' @keywords file rds location
#' @export
dumpRDS <- function(object, naam, ...){
  saveRDS(object = object, file = getLocationNaam(naam, checkExists = FALSE), ...)
  invisible()
}

#' A Function to read a csv-file from a location whithin the work-location
#'
#' This function om de exacte locatie te achterhalen
#' @param naam naam van het bestand
#' @param ... overige parameters 
#' @keywords file csv location
#' @export
readCSV <- function(naam, ...){
  #  werkdirOld <- getwd()
  #  setwd(get("werkdir", envir=.DataDiggersPackageOptions))
  csv <- utils::read.csv2(file = getLocationNaam(naam), ...)
  #  setwd(werkdirOld)
  csv
}

#' A Function to read a rds-file from a location whithin the work-location
#'
#' This function om de exacte locatie te achterhalen
#' @param naam naam van het bestand
#' @param ... ove
#' rige parameters 
#' @keywords file rds location
#' @export
readRDSdd <- function(naam, ...){
  readRDS(getLocationNaam(naam), ...)  
}

