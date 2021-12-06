
#' Make target
#'
#' @description Make target rotation matrix for use with \cite{esem_efa()}.
#' @param nitems An integer. The total number of items.
#' @param mainloadings A list. A list indicating the indexes of the items related
#' to each latent variable in a rotation matrix \[nItems x nLatentVariables\]. See examples.
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
#' main_loadings_list <- list(f1 = c(1,2,5:7),
#'                            f2 = c(3,4,8,9))
#' make_target(9, main_loadings_list)
#'
#' #bifactor matrix for the same dataset
#' make_target(9, main_loadings_list, TRUE)
make_target <- function (nitems, mainloadings, bifactor = FALSE){
  ## check inputs
  mainloadings_vec <- unlist(mainloadings)
  expected_mainloadings <- c(1:nitems)
  if(!all(expected_mainloadings %in% mainloadings_vec) |
     any(!(mainloadings_vec %in% expected_mainloadings))){
    abort_bad_indexes(nitems, not = mainloadings_vec)
  }

  ## make target matrix
  target_mat <- psych::make.keys(nitems, mainloadings)
  if(bifactor) target_mat <- cbind(target_mat, G = 1)

  psych::scrub(target_mat, isvalue = 1)
}

#' ESEM EFA
#'
#' Wrapper around \cite{psych::fa()} for running ESEM-like exploratory factor analysis.
#' @param data Raw data.frame or matrix. Subjects x Items data or item covariance matrix.
#' @param nfactors An integer. Number of factors to extract (including G when bifactor).
#' @param target Target rotation matrix. Usually obtained with \cite{make_target()}.
#' Defaults to "none", in which case geominQ rotation is used.
#' @param bifactor Logical. Set to TRUE if model is bifactor.
#' @param fm Factor extraction method. Defaults to "Principal Axis", see \cite{psych::fa()}
#' for alternatives.
#' @param targetAlgorithm Character vector. Factor rotation to use with target matrix.
#' Defaults to oblique target rotation ("targetQ"). An alternative is "TargetT", for
#' orthogonal target rotation. Automatically set to
#' orthogonal target rotation if `bifactor = TRUE`.
#' @param ... Additional parameters passed to \cite{psych::fa()}.
#'
#' @details Facilitates ESEM-like exploratory factor analyses.
#' In the simplest case, a (oblique) Geomin rotation is used after factor extraction.
#' The user can control the value of Geomin `delta` (aka `epsilon` in the literature)
#' by specifying the desired number (usually between 0.1 and 0.9) with `delta` in `...`
#' (see examples).
#'
#' To run a factor extraction with target rotation, a target rotation matrix (items x factors)
#' must be supplied to `target`. Matrix cells should show zeros where loadings are expected
#' to be as close to zero as possible and `NA` where loadings should be freely estimated.
#' One can create such a matrix by hand with \cite{base::matrix()}, but check the helping
#' function \cite{make_target()} included with this package. The default target rotation
#' is oblique, but can be changed to the orthogonal alternative
#' by setting `targetAlgorithm = "TargetT"`.
#'
#' To run a factor extraction bifactor target rotation, a bifactor target rotation matrix must be
#' supplied to `target` and `bifactor` should be set to `TRUE`.
#'
#' @return \cite{psych::fa()} object with factor extraction results.
#' @export
#'
#' @examples
#' #use Tucker 9 cognitive variables cov matrix
#' tucker <- psych::Tucker
#'
#' # esem with Geomin rotation
#' esem_efa(tucker, 2)
#' # esem with Geomin rotation setting the rotation delta (aka epsilon)
#' esem_efa(tucker, 2, delta = .5)
#'
#' # esem with oblique target rotation
#' target_mat <- make_target(9, list(f1 = c(1,2,5:7), f2 = c(3,4,8,9)))
#' esem_efa(tucker,2,target_mat)
#' # esem with bifactor target rotation
#' bifactor_target_mat <- make_target(9, list(f1 = c(1,2,5:7), f2 = c(3,4,8,9)), TRUE)
#' esem_efa(tucker,3,bifactor_target_mat, maxit = 2000) #maxit needed for convergence
esem_efa <- function(data,  nfactors, target = "none", bifactor = FALSE, fm = "pa", targetAlgorithm = "targetQ",...){
  ## check if number of factors is the same in rotation matrix and input
  if(is.matrix(target)){
    if(nfactors != ncol(target)){
      abort_bad_n_esem(ncol(target), nfactors)
    }
  }

  ifelse(target== "none"
         ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                                rotate = "geominQ",...)
         ,ifelse(bifactor
                 ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                                        rotate = "TargetT", Target = target,...)
                 ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                                        rotate = targetAlgorithm, Target = target,...)
                 )
  )
  esem_fit
}

