context("test-unit-test")

test_that("scale_expression works", {
  matrix <- matrix(c(1:100), ncol = 10)
  scaled.matrix <- scaleExpression(matrix)
  expect_equal(mean(scaled.matrix[,1]), 1)
})
