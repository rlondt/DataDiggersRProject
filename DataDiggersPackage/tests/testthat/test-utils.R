library(testthat)
library(DataDiggersPackage)
# test-utils.R
context("Utils")
test_that("setWorkdir sets the working directory", {
  wcd <- paste(getwd(),"test_dir", sep = "/")
  expect_invisible(setWorkdir(wcd))
})

test_that("functionality of getLocationNaam", {
  wcd <- paste(getwd(),"test_dir", sep = "/")
  setWorkdir(wcd)
  expect_invisible(
    temp <- getLocationNaam("test", checkExists = FALSE)
  )
  expect_equal(temp, paste(wcd, "test", sep="/"))
  expect_error(getLocationNaam("test"))
  expect_error(getLocationNaam("test", checkExists = TRUE))
})

# dumpRDS <- function(object, naam, ...){
#   saveRDS(object = object, file = getLocationNaam(naam, checkExists = FALSE), ...)
#   invisible()
# }
# 
# readCSV <- function(naam, ...){
#   #  werkdirOld <- getwd()
#   #  setwd(get("werkdir", envir=.DataDiggersPackageOptions))
#   csv <- read.csv2(file = getLocationNaam(naam), ...)
#   #  setwd(werkdirOld)
#   csv
# }
# 
# readRDSdd <- function(naam, ...){
#   readRDS(getLocationNaam(naam), ...)  
# }
# 
