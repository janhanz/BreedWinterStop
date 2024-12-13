library('move2')

source("../../src/io/rds.R")

test_that("read move2", {
  actual <- readRdsInput(sourceFile = "data/Input_Curlew.rds")
  expect_true(mt_is_move2(actual))
})
