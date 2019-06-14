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

test_that("calculateCompExpMat works", {
  exp.mat <- matrix(1:100, ncol = 10)
  comp.mat <- calculateCompExpMat(exp.mat, 2)
  expect_equal(dim(comp.mat), c(10, 5))
  expect_equal(mean(comp.mat[1, ]), mean(exp.mat[1, ]))
})

test_that("calculateCompTVec works", {
  t.vec <- c(1:100)
  comp.t.vec <- calculateCompTVec(t.vec, 10)
  expect_equal(length(comp.t.vec), 10)
  expect_equal(mean(comp.t.vec), mean(t.vec))
})

test_that("calculateGP works", {
  t.vec <- 1:10
  exp.vec <- rnorm(10)
  gp.rlt <- calculateGP(t.vec, exp.vec)
  expect_equal(names(gp.rlt), c("ll", "params", "predict.exp.vec", "predict.dexp.vec"))
})

test_that("concatExpMat works", {
  mat <- matrix(1:30, nrow = 3)
  rownames(mat) <- c("a", "b", "c")
  ptcomp.df <- tibble(comp.exp.mat = list(mat, mat))
  gene.vec <- c("a", "b", "d")
  concat.mat <- concatExpMat(gene.vec, ptcomp.df)
  expect_equal(dim(concat.mat), c(2, 20))
})

test_that("concatTVec works", {
  t.vec <- 1:10
  ptcomp.df <- tibble(comp.t.vec = list(t.vec, t.vec))
  concat.t.vec <- concatTVec(ptcomp.df)
  expect_equal(length(concat.t.vec), 20)
})
