library(testthat)
library(DataDiggersPackage)
# test-utils.R
context("Utils")
test_that("setWorkdir sets the working directory", {
  wcd <- paste(getwd(),"test_dir", sep = "/")
  expect_invisible(setWorkdir(wcd))
})