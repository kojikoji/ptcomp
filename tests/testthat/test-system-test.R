context("test-unit-test")

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
