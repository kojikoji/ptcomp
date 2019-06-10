##' Convert vector to 1 column matrix
##'
##' 
##' @title vec2mat
##' @param vec numeric vector
##' @return mat numeric matrix
##' @author 小嶋泰弘
vec2mat <- function(vec){
  matrix(vec, ncol = 1)
}

##' Calculate GP for each gene
##'
##' This function calculate the log likelihood and optimized parameters as list
##' @title calculateGP
##' @param exp.vec numeric vector, expression values
##' @param t.vec numeric vector, pseudo time values
##' @return gp.rlt list, contain loglikelihood and optimized parameters
##' @author Yasuhiro Kojima
##' @import dplyr
calculateGP <- function(t.vec, exp.vec, sparse.gp.res = 20){
  Z <- seq(from = min(t.vec), to = max(t.vec), length.out = sparse.gp.res) %>%
    vec2mat()
  gp.model <- GPy$models$SparseGPRegression(vec2mat(t.vec), vec2mat(exp.vec), Z = Z)
  gp.model$optimize('bfgs')
  list(ll = gp.model$log_likelihood(),
       params = list(gp.model$param_array[(1:3) + sparse.gp.res]))
}

##' Calculate GP for all genes and store them in data frame
##'
##' The columns represent gene name, log likelihood and each parameter
##' @title calculateGPDf
##' @param gene.vec charactor vector, gene names
##' @param ptcomp.df data frame, contain expression and annotation
##' @return gp.df data frame
##' @author Yasuhiro Kojima
##' @import tibble
##' @export
calculateGPDf <- function(gene.vec, ptcomp.df){
  concat.exp.mat <- concatExpMat(gene.vec, ptcomp.df)
  concat.t.vec <- concatTVec(ptcomp.df)
  gene.vec <- intersect(gene.vec, rownames(concat.exp.mat))
  purrr::map_dfr(
    gene.vec,
    function(gene){
      c(
        list(gene = gene),
        calculateGP(concat.exp.mat[gene, ], concat.t.vec))
    })
}


##' Concatenate expression matrix included in ptcomp data frame
##'
##' 
##' @title concatExpMat
##' @param gene.vec charactor vector, gene names
##' @param ptcomp.df data frame, contain expression and annotation
##' @return exp.mat
##' @author Yasuhiro Kojima
concatExpMat <- function(gene.vec, ptcomp.df){
  gene.vec <- ptcomp.df$comp.exp.mat %>%
    purrr::reduce(
      function(pre.gene.vec, exp.mat){
        intersect(pre.gene.vec, rownames(exp.mat))
      },
      .init = gene.vec)
  ptcomp.df$comp.exp.mat %>%
    {do.call(cbind, .)} %>%
    {.[gene.vec, ]}
}


##' Concatenate pseudo time vector included in ptcomp data frame
##'
##' 
##' @title concatTVec
##' @param pt.comp.df data frame, contain expression and annotation  
##' @return t.vec  numeric vector, each element represent pseudo time for each gene
##' @author Yasuhiro Kojima
concatTVec <- function(pt.comp.df){
  pt.comp.df %>%
    {do.call(c, .$comp.t.vec)}
}

