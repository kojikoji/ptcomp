##' Seup \code{tibble} object which is used for following analysis
##'
##' This function set up the \code{tibble} data frame for following analysis
##' @title setupPtcomp
##' @param count.mat numeric matrix, gene (row) * cell (column) matrix
##' @param t.vec numeric vector, pseudo time estimation for each cell
##' @param treatment.lab character vector, experimental treatment for each cell
##' @param replicate.lab character vector, experimental replicate for each cell
##' @param unit.num numeric, how many cells are calculated for one entry of mean expression entry
##' @return ptcomp tibble
##' @author Yasuhiro Kojima
##'
##' @import tibble Matrix
setupPtcomp <- function(count.mat, t.vec, treatment.lab, replicate.lab, unit.num = 100){
  scale.exp.mat <- scaleExpression(count.mat)
  meta.data <- makeCompressedMetaData(t.vec, treatment.lab, replicate.lab)
  meta.data %>%
    dplyr::mutate(
      compress.mat = purrr::map(
        cell.vec,
        function(cell.vec){
          compressMat(scale.exp.mat[, cell.vec], unit.num)
        }))
}

##' Scaling gene expresiion
##'
##' .. content for \details{} ..
##' @title scaleExpression.
##' @param count.mat numeric matrix, gene (row) * cell (column) matrix
##' @return scale.exp.mat numeric matrix, expression
##' @author Yasuhiro Kojima
scaleExpression <- function(count.mat){
  count.vec <- Matrix::colMeans(count.mat)
  t(t(count.mat) / count.vec)
}
