##' Compare the LL for separated or merged model
##'
##' Calcualte LL for each treatment and merginalized data, and see the difference
##' @title calculateLLDiff
##' @param treatment1 character, first treatment
##' @param treatment2 character, second treatment
##' @param gene.vec character vec, gene names used in this comparison
##' @param ptcomp.df data frame, contain expression and annotation
##' @return diff.ll.df data frame, gene name, treatment1 ll, treatment2 ll, merged ll
##' @author Yasuhiro Kojima
##' @export
calculateLLDiff <- function(treatment1, treatment2, gene.vec, ptcomp.df){
}


##' Permutate treatment labels
##'
##' @title permutateTreatment
##' @param ptcomp.df data frame, contain expression and annotation
##' @return permutated.ptcomp.df data frame, contain expression and annotation
##' @author Yasuhir Kojima
##' @export
permutateTreatment <- function(ptcomp.df){
}
