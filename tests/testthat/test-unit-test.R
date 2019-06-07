context("test-unit-test")

test_that("scaleExpression works", {
  matrix <- matrix(c(1:100), ncol = 10)
  scaled.matrix <- scaleExpression(matrix)
  expect_equal(mean(scaled.matrix[, 1]), 1)
})

test_that("makeMetaData works", {
    cell.vec <- c(1:100)
    t.vec <- c(1:100)
    replicate.vec <- purrr::map(1:10, rep, 10) %>% unlist()
    treatment.vec <- purrr::map(1:5, rep, 20) %>% unlist()
    meta.df <- makeMetaData(cell.vec, t.vec, treatment.vec, replicate.vec)
    expect_equal(nrow(meta.df), 10)
    expect_equal(ncol(meta.df), 4)
})

test_that("calcculateCompExpMat works", {
  exp.mat <- matrix(1:100, ncol = 10)
  comp.mat <- calculateCompExpMat(exp.mat, 2)
  expect_equal(dim(comp.mat), c(10, 5))
  expect_equal(mean(comp.mat[1, ]), mean(exp.mat[1, ]))
})

test_that("calcculateCompTVec works", {
  t.vec <- c(1:100)
  comp.t.vec <- calculateCompTVec(t.vec, 10)
  expect_equal(length(comp.t.vec), 10)
  expect_equal(mean(comp.t.vec), mean(t.vec))
})
