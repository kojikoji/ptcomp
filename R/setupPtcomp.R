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
##' @export
setupPtcomp <- function(count.mat, t.vec, treatment.vec, replicate.vec, unit.num = 100){
  scale.exp.mat <- scaleExpression(count.mat)
  meta.data <- makeMetaData(colnames(count.mat), t.vec, treatment.vec, replicate.vec)
  meta.data %>%
    addCompressedExpression(scale.exp.mat, unit.num)
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


##' Add columns of compressed expression
##'
##' Add columns of compressed expression
##' @title 
##' @param meta.df tibble, data frame containing cell information
##' @param exp.mat numeric matrix, gene (row) * cell (column) matrix
##' @param unit.num numeric, how many cells are calculated for one entry of mean expression entry
##' @return 
##' @author 小嶋泰弘
addCompressExpression <- function(meta.df, exp.mat, unit.num){
  meta.df %>%
    dplyr::mutate(
      comp.exp.mat = purrr::map(
        cell.vec, t.vec,
        function(cell.vec, t.vec){
          calculateCompExpMat(exp.mat[, cell.vec[order(t.vec)]], unit.num)
        }),
      comp.t.vec = purrr::map(
        cell.vec, t.vec,
        function(cell.vec, t.vec){
          calculateCompTVec(exp.mat[, cell.vec[order(t.vec)]], unit.num)
        }))
}


##' Calculate compressed expression
##'
##' Calculate compressed expression from \code{exp.mat}.
##' @title 
##' @param exp.mat numeric matrix, gene (row) * cell (column) matrix
##' @param unit.num numeric, how many cells are calculated for one entry of mean expression entry
##' @return 
##' @author 小嶋泰弘
calculateCompExpMat <- function(exp.mat, unit.num){
  col.num <- as.integer(ncol(exp.mat) / unit.num)
  purrr::map(
    0:(col.num - 1),
    function(idx){
      Matrix::rowMeans(exp.mat[, (idx * unit.num + 1):((idx + 1) * unit.num)])
    }) %>%
    {do.call(cbind, .)}
}
