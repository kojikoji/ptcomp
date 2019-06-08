##' Calculate GP for each gene
##'
##' This function calculate the log likelihood and optimized parameters as list
##' @title calculateGP
##' @param exp.vec numeric vector, expression values
##' @param t.vec numeric vector, pseudo time values
##' @return gp.rlt list, contain loglikelihood and optimized parameters
##' @author Yasuhiro Kojima
##' @import dplyr
calculateGP <- function(exp.vec, t.vec){
}

##' Calculate GP for all genes and store them in data frame
##'
##' The columns represent gene name, log likelihood and each parameter
##' @title calculateGPDf
##' @param ptcomp.df data frame, contain expression and annotation
##' @return gp.df data frame
##' @author Yasuhiro Kojima
##' @import tibble
##' @export
calculateGPDf <- function(ptcomp.df){
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
}


##' Concatenate pseudo time vector included in ptcomp data frame
##'
##' 
##' @title concatTVec
##' @param pt.comp.df data frame, contain expression and annotation  
##' @return t.vec  numeric vector, each element represent pseudo time for each gene
##' @author Yasuhiro Kojima
concatTVec <- function(pt.comp.df){
}

