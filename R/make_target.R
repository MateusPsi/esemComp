
#' Make target
#'
#' @description Make target rotation matrix for use with \cite{esem_efa()}.
#' @param nitems An integer. The total number of items.
#' @param mainloadings A list. A list indicating the indexes of the items related to each latent variable. See examples.
#' @param bifactor Logical. If TRUE, adds a G-factor column to the resulting target matrix.
#'
#' @details A target rotation matrix is composed of cells indicating which loadings should be freely estimated
#' and which should be as close to zero as possible when the factor solution is rotated. Freely
#' estimated loadings are represented with NAs. `mainloadings` identify which items are free
#' for each latent variable. See the vignette esem as efa for suggestions on how to use.
#' @return Target matrix with NAs and zeros \[nItems x nLatentVariables\].
#' @export
#'
#' @examples
#' #target matrix for the Tucker dataset in the psych package
#' make_target(9, list(f1 = c(1,2,5:7), f2 = c(3,4,8,9)))
#'
#' #bifactor matrix for the same dataset
#' make_target(9, list(f1 = c(1,2,5:7), f2 = c(3,4,8,9)),TRUE)
make_target <- function (nitems, mainloadings, bifactor = FALSE){
  target_mat <- psych::make.keys(nitems, mainloadings)
  if(bifactor) target_mat <- cbind(target_mat, G = 1)

  psych::scrub(target_mat, isvalue = 1)
}

esem_efa <- function(data,  nfactors, target, bifactor = FALSE, fm = "pa", rotate = "targetQ",...){
  ifelse(bifactor
    ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                          rotate = "TargetT", Target = target,...)
    ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                           rotate = rotate, Target = target,...)
  )
  esem_fit
}

make_loadings_dt <- function(fa_object, factor_names)

syntax_composer <- function(efa_object){}
