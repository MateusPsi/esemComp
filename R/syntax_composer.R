
#' Make loadings DT
#' Given a psych::efa object, make a loadings dt for syntax_composer
#' @param fa_object
#' @param factor_names
#'
#' @return long_loadings data.table
#'
.make_loadings_dt <- function(fa_object, factor_names){
  factor_names <- unlist(factor_names)
  nrow <- nrow(fa_object$loadings)
  ncol <- ncol(fa_object$loadings)
  nfactors <- length(factor_names)

  #check number of names and fa_object factors
  if(ncol != nfactors){
    msg <- glue::glue(
      "Number of referents is {nfactors} and number of factors in efa_object is {ncol}.
      The number of referents and factors must be the same. If you are trying to compose
      a bifactor model, remember that a referent for the G-factor is also required."
    )
    rlang::abort("error_bad_argument",
                 message = msg)
  }

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
#' @param referents A named list in the form `list(factor1 = "referent1", etc)`. Each
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
#' referents_list <- list(textual = "x5", visual = "x3", speed = "x7")
#' model_syntax <- syntax_composer(geomin_efa, referents_list)
#' writeLines(model_syntax)
#'
#'# esem-within-cfa
#'esem_w_cfa <- lavaan::cfa(model_syntax, data = hw_data, std.lv = TRUE)
#'summary(esem_w_cfa)
syntax_composer <- function(efa_object, referents, only_fix_crossloadings = TRUE){

  loadings_dt <- .make_loadings_dt(efa_object, names(referents))
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  if(only_fix_crossloadings){
    for (l in names(referents)) loadings_dt[latent != l & item == referents[l], is_anchor := 1]
  }else {
    for (l in names(referents)) loadings_dt[item == referents[l], is_anchor := 1]
  }

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

#' Automatically find referent items for ESEM
#'
#' @param efa_object A \cite{psych::fa()} object with the results of a exploratory factor analysis.
#' Referents will be selected based on the factor loadings in this solution.
#' @param factor_names Character vector. The names should identify the factors in the `efa_object`.
#' They must be in the same order they appear in the `efa_object`.
#'
#' @details Given an exploratory factor analysis results, this function finds a referent item for each
#' factor and outputs a list to be used in \cite{syntax_composer}.
#'
#' The automatic selection currently does not choose the referents with the recommended approach
#' in the literature, that is, items that load heavily on one factor and lightly on all the others.
#' In its current implementation, the items are chosen solely based on their highest loadings.
#' This may lead to less than ideal referent selection in some situations. It is recommended to always
#' compare the resulting  referents with the items one would choose when inspecting the exploratory
#' solution loadings (usually with `loadings(efa_object)`). In fact, the user should always check
#'  `loadings(efa_object)` _before_ calling the function to be able to specify `factor_names` correctly.
#'
#' @return A named list in the format `c(FactorName = referent)`, in the same order as the given
#' factor names.
#' @export
#'
#' @examples
#' # use Holzinger and Swineford (1939) dataset in lavaan package
#' hw_data <- lavaan::HolzingerSwineford1939
#' hw_data <- hw_data[,c(7:15)]
#'
#' #make exploratory analysis with geomin rotation
#' geomin_efa <- esem_efa(hw_data,3)
#'
#' # check the order of factors in the efa solution
#' loadings(geomin_efa)
#' #find referents with factors in the order checked above
#' find_referents(geomin_efa, c("textual", "visual", "speed"))
#' #In this particular case, automatic selection chooses
#' #the same items one would choose manually. For comparison:
#' loadings(geomin_efa)
find_referents <- function(efa_object, factor_names){
  #factor_names <- unlist(factor_names)
  nrow <- nrow(efa_object$loadings)
  ncol <- ncol(efa_object$loadings)
  nfactors <- length(factor_names)

  #check number of names and fa_object factors
  if(ncol != nfactors){
    msg <- glue::glue(
      "Number of factor_names is {nfactors} and number of factors in efa_object is {ncol}.
      The number of factor_names and factors must be the same. If you are trying to find
      referents for a bifactor model, remember that a referent for the G-factor is also required."
    )
    rlang::abort("error_bad_argument",
                 message = msg)
  }

  efa_matrix <- matrix(efa_object$loadings, nrow = nrow,
                       ncol = nfactors,
                       dimnames = list(row.names(efa_object$loadings)))
  efa_loading <- data.table::data.table(efa_matrix, keep.rownames = "item")

  data.table::setnames(efa_loading,
                       paste0("V",c(1:nfactors)),
                       factor_names)

  long_loadings <- data.table::melt(efa_loading, "item", variable.name = "latent")
  referent_loadings <- long_loadings[order(-value),.SD[1],by = latent]
  unique_referents <-  length(unique(referent_loadings$item))

  # Check ties in referents among latents
  # tries to break the ties by moving forward
  # from the highest to the lowest values.
  # Currently moves to the next value in all
  # dupplicated at once.
  n_value <- 1
  while(nfactors != unique_referents){
    #latents with bad referent
    bad_referents <- referent_loadings[duplicated(item),latent]
    n_value <- n_value+1
    new_referents <- long_loadings[latent %in% bad_referents][
      order(-value),.SD[n_value],by = latent]
    referent_loadings[latent %in% bad_referents] <- new_referents
    unique_referents <-  length(unique(referent_loadings$item))
  }

  #make sure rows are in the right order
  factor_names_unique <- make.unique(factor_names)
  referent_loadings <- referent_loadings[match(factor_names_unique
                                               ,as.character(latent))]
  referent_list <- as.list(referent_loadings$item)
  names(referent_list) <- referent_loadings$latent

  referent_list
}
