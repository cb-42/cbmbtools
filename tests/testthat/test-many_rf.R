context("many_rf")

test_that("output contains randomForest formula and models", {
  rflist <- many_rf(df = mtcars, y = mtcars$mpg, nforest = 5)
  expect_identical(class(rflist[[1]]), c("randomForest.formula", "randomForest"))
  expect_identical(class(rflist[[5]]), c("randomForest.formula", "randomForest"))
})
