context("test-system-test")

test_that("set up ptcomp works", {
  count.mat <- matrix(c(1:1000), ncol = 100)
  colnames(count.mat) <- as.character(1:100)
  t.vec <- c(1:100)
  replicate.vec <- purrr::map(1:10, rep, 10) %>% unlist()
  treatment.vec <- purrr::map(1:5, rep, 20) %>% unlist()
  ptcomp.df <- setupPtcomp(count.mat, t.vec, treatment.vec, replicate.vec, 2)
  expect_equal(dim(ptcomp.df$comp.exp.mat[[1]]), c(10, 5))
  expect_equal(length(ptcomp.df$comp.t.vec[[1]]), 5)
})

test_that("calculateGPDf works", {
  exp.mat <- matrix(rnorm(30), nrow = 3)
  rownames(exp.mat) <- c("a", "b", "c")
  t.vec <- 1:10
  ptcomp.df <- tibble(
    comp.exp.mat = list(exp.mat, exp.mat),
    comp.t.vec = list(t.vec, t.vec))
  gene.vec <- c("a", "b")
  gp.df <- calculateGPDf(gene.vec, ptcomp.df)
  expect_equal(
    colnames(gp.df),
    c("gene", "ll", "params", "predict.exp.vec", "predict.dexp.vec"))
  expect_equal(nrow(gp.df), 2)
})

test_that("calculateLLDiff works", {
  exp.mat <- matrix(rnorm(30), nrow = 3)
  rownames(exp.mat) <- c("a", "b", "c")
  t.vec <- 1:10
  ptcomp.df <- tibble(
    comp.exp.mat = list(exp.mat, exp.mat),
    comp.t.vec = list(t.vec, t.vec))
  tr1.ptcomp.df <- ptcomp.df %>%
    {.$treatment <- "tr1";.}
  tr2.ptcomp.df <- ptcomp.df %>%
    {.$treatment <- "tr2";.}
  gene.vec <- c("a", "b", "d")
  ll.diff.df <- calculateLLDiff("tr1", "tr2", gene.vec, ptcomp.df)
  expect_equal(
    colnames(ll.diff.df),
    c("gene", "ll.tr1", "ll.tr2", "ll", "diff.ll"))
  expect_equal(nrow(ll.diff.df), 2)
})
