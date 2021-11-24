
#' Make loadings DT
#' Given a psych::efa object, make a loadings dt for syntax_composer
#' @param fa_object
#' @param factor_names
#'
#' @return long_loadings data.table
#'
make_loadings_dt <- function(fa_object, factor_names){
  nrow <- nrow(fa_object$loadings)
  nfactors <- length(factor_names)
  #include check between names and fa_object factors

  efa_matrix <- matrix(fa_object$loadings, nrow = nrow,
                       ncol = nfactors,
                       dimnames = list(row.names(fa_object$loadings)))
  efa_loading <- data.table::data.table(efa_matrix, keep.rownames = "item")

  data.table::setnames(efa_loading,
                       paste0("V",c(1:nfactors)),
                       factor_names)

  long_loadings <- data.table::melt(efa_loading, "item", variable.name = "latent")
  long_loadings[,value := round(value,3)]
  #data.table::setnames(long_loadings$esem,"Item","item")

  long_loadings[]
}

#' ESEM-within-CFA syntax composer for lavaan
#'
#' @param efa_object A \cite{psych::fa()} object with the results of a exploratory factor analysis.
#' The factor loadings from this analysis will be set as starting values or fixed values in the
#' lavaan syntax created.
#' @param referents A named character vector in the form `c(factor1 = "referent1", etc)`. Each
#' entry identifies the referent for one of the factors. The order of this vector must be
#' the same order the factors are ordered in the `efa_object`.
#'
#' @details ESEM-within-CFA models have latent variables estimations with all cross-loadings.
#' To improve fit, starting values from a previous exploratory analysis are used. To ensure
#' model identification, a referent is chosen for each latent variable and all their
#' cross-loadings are set as fixed values. This function "composes" such a lavaan model
#' syntax from an EFA object. This model can subsequently be used in lavaan with
#' \cite{lavaan::cfa()} or \cite{lavaan::sem()}. The best way to check the output is
#' with \cite{writeLines()}.
#'
#' It is very important to make sure the order of the `referents` vector is the same as the
#' factor order in the factor loadings matrix in `efa_object`. Otherwise, the names of the
#' latent variables may be wrong in the syntax. See also the vignette \cite{esem-within-cfa}.
#'
#' @return A character vector with a lavaan syntax for the ESEM model.
#' @export
#' @seealso \cite{make_target}
#' @examples
#' # use Holzinger and Swineford (1939) dataset in lavaan package
#' hw_data <- lavaan::HolzingerSwineford1939
#' hw_data <- hw_data[,c(7:15)]
#'
#' #make exploratory analysis with geomin rotation
#' geomin_efa <- esem_efa(hw_data,3)
#' referents_vector <- c(textual = "x5", visual = "x3", speed = "x7")
#' model_syntax <- syntax_composer(geomin_efa, referents_vector)
#' writeLines(model_syntax)
#'
#'# esem-within-cfa
#'esem_w_cfa <- lavaan::cfa(model_syntax, data = hw_data)
#'summary(esem_w_cfa)
syntax_composer <- function(efa_object, referents){

  loadings_dt <- make_loadings_dt(efa_object, names(referents))
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  for (l in names(referents)) loadings_dt[latent != l & item == referents[l], is_anchor := 1]

  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]

  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+\n"),"\n")
  }

  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = "\n")

}
