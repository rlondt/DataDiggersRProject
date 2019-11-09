# test DQ Framework
context("DQFramework")

test_that("Framework initializes correctly", {
  expect_invisible(initializeDQScoringFramework())
  DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
  expect_true(!is.null(DQScoringDF))
})

test_that("Framework accepts new scores", {
  #initializeDQScoringFramework()
  # correcte scores
  expect_invisible(addScoreToDQFramework(COMPLEETHEID, 1, 5))
  expect_invisible(addScoreToDQFramework(CONSISTENTIE, 1, 5))
  expect_invisible(addScoreToDQFramework(UNICITEIT, 1, 5))
  expect_invisible(addScoreToDQFramework(VALIDITEIT, 1, 5))
  expect_invisible(addScoreToDQFramework(ACCURAATHEID, 1, 5))
  DQScoringDF <- get("DQScoringDF", envir=.DataDiggersPackageOptions)
  expect_true(nrow(DQScoringDF)==5)
  expect_error(
    addScoreToDQFramework(COMPLEETHEID, 6, 600)
  )
  expect_error(
    addScoreToDQFramework(COMPLEETHEID, -1, 1)
  )
})

