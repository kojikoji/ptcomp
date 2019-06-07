##' Seup \code{tibble} object which is used for following analysis
##'
##' This function set up the \code{tibble} data frame for following analysis
##' @title setupPtcomp
##' @param count.mat numeric matrix, gene (row) * cell (column) matrix
##' @param t.vec numeric vector, pseudo time estimation for each cell
##' @param treatment.vec character vector, experimental treatment for each cell
##' @param replicate.vec character vector, experimental replicate for each cell
##' @param unit.num numeric, how many cells are calculated for one entry of mean expression entry
##' @return ptcomp tibble
##' @author Yasuhiro Kojima
##'
##' @import tibble Matrix tidyr
setupPtcomp <- function(count.mat, t.vec, treatment.vec, replicate.vec, unit.num = 100){
  scale.exp.mat <- scaleExpression(count.mat)
  meta.data <- makeMetaData(colnames(count.mat), t.vec, treatment.vec, replicate.vec)
  meta.data %>%
    addCompressedExpression(scale.exp.mat)
}

##' Scaling gene expresiion
##'
##' Scaling the expression mean to 1 for each column (cells)
##' @title scaleExpression.
##' @param count.mat numeric matrix, gene (row) * cell (column) matrix
##' @return scale.exp.mat numeric matrix, expression
##' @author Yasuhiro Kojima
scaleExpression <- function(count.mat){
  count.vec <- Matrix::colMeans(count.mat)
  t(t(count.mat) / count.vec)
}

##' Set up meta data
##'
##' Set up meta data for ptcomp calculation. Basically, it group cells by replicate
##' @title makeMetaData
##' @param cell.vec charactor vecotor, cell index for each cell
##' @param t.vec numeric vector, pseudo time estimation for each cell
##' @param treatment.vec character vector, experimental treatment for each cell
##' @param replicate.vec character vector, experimental replicate for each cell
##' @return meta.df
##' @author Yasuhiro Kojima
makeMetaData <- function(cell.vec, t.vec, treatment.vec, replicate.vec){
  tibble::tibble(
    cell = cell.vec,
    t = t.vec,
    treatment = treatment.vec,
    replicate = replicate.vec) %>%
    tidyr::nest(-replicate) %>%
    dplyr::mutate(
      cell.vec = purrr::map(data, ~ .x$cell),
      t.vec = purrr::map(data, ~ .x$t),
      treatment = purrr::map(data, ~ .x$treatment[1])) %>%
    dplyr::select(replicate, treatment, cell.vec, t.vec)
}
