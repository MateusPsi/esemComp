#' McDonald's Omega for ESEM
#'
#' @param esem_model_fit Lavaan object with an ESEM fitted model.
#' @param target_matrix Target rotation matrix. Usually, the same used to run \cite{esem_efa()}.
#' The names of the factors in the matrix must
#' be exactly the same as the names in the fitted lavaan model.
#'
#' @description Calculates McDonald's omega using the data inside the Lavaan object of a fitted
#' model. The formula is the following:
#' \deqn{ \omega = |\sum{\lambda_i}|^2 / |\sum{\lambda_i}|^2 + \delta_i_i }
#' Where  \eqn{\lambda_i} are the factor loadings and \eqn{\delta_i_i}, the error variances.
#' @return McDonald's Omega value.
#' @export
#'
#' @examples
#' #use Tucker 9 cognitive variables cov matrix
#' tucker <- psych::Tucker
#'
#' # esem with oblique target rotation
#' target_mat <- make_target(9, list(f1 = c(1,2,5:7), f2 = c(3,4,8,9)))
#' tucker_esem_efa <- esem_efa(tucker,2,target_mat)
#'
#' # fit lavaan model
#' referents_list <- find_referents(tucker_esem_efa, c("f2", "f1"))
#' model_syntax <- syntax_composer(tucker_esem_efa, referents_list)
#' fit <- lavaan::cfa(model_syntax, sample.cov = as.matrix(tucker), sample.nobs = 710)
#'
#' # McDonald's Omega
#' omega_esem(fit, target_mat)
omega_esem <- function(esem_model_fit, target_matrix){
  factor_order <- dimnames(target_matrix)[[2]]
  loadings <- lavaan::lavInspect(esem_model_fit,"std")[["lambda"]][,factor_order]

  loadings_sqr <- sum(abs(loadings[is.na(target_matrix)]))^2

  res_var <- lavaan::lavInspect(esem_model_fit,"std")[["theta"]]
  res_var <- sum(diag(res_var))

  overall_omega <- loadings_sqr*(1/(loadings_sqr+res_var))

  # per factor
  factor_omega <- function(factor_name){
    items_idx <- is.na(target_matrix[,factor_name])
    loadings <- lavaan::lavInspect(esem_model_fit,"std")[["lambda"]][items_idx,factor_name]
    loadings_sqr <- sum(abs(loadings))^2
    res_var <- lavaan::lavInspect(esem_model_fit,"std")[["theta"]][items_idx]
    res_var <- sum(diag(res_var))
    loadings_sqr*(1/(loadings_sqr+res_var))
  }

  names(factor_order) <- factor_order
  omegas <- sapply(factor_order, factor_omega)
  omegas[["overall"]] <- overall_omega

  omegas
}


#' Export (dump) lavaan results
#'
#' @param lavaan_model_fit Lavaan object. Lavaan object with fitted ESEM model, usually obtained with `lavaan::cfa()`.
#' @param path_name Character vector. Path directing where to save the text file with the results. It must end with the
#' desired file name with the desired text file extension. The default is to save the file in the current directory
#' with file name "lavaan_summary.txt".
#' @param preamble Character vector. Allows the user to add some text before the results report. Useful to
#' identify the analysis, add information about the data etc.
#' @param ... `parameter = value` pairs. Additional modifiers to lavaan::summary() call. See the full list
#' of available modifiers at \cite(lavaan-class).
#'
#' @details Runs `lavaan::summary()` on `lavaan_model_fit` with modifiers `std = TRUE`
#' and `fit.measures = TRUE` and dumps the result to a text file. The user has the option to add some
#' text as a preamble before the results description. It is also possible to add more information to the
#' exported results by including more modifiers to the `summary` call with the `...` named values
#' (e.g. `rsquare = TRUE` to include r-squared measures), see \cite(lavaan-class) for the full list
#' of available modifiers.
#'
#' The `path_name` directories must be separated by "\" (even in Windows).
#'
#' @return Message confirming data dump. Saves the data to a text file as a side-effect.
#' @export
#'
#' @examples
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' fit <- lavaan::cfa(HS.model, data= lavaan::HolzingerSwineford1939, std.lv = TRUE)
#'
#' export_lavaan_results(fit)
#' # name the analysis with preamble
#' export_lavaan_results(fit,
#' path_name = "titled_lavaan_summary.txt",
#' preamble = "CLASSIC HOLZINGER SWINEFORD CFA MODEL")
#' # add r-squared measure to the exported results
#' export_lavaan_results(fit,
#' path = "lavaan_summary_rsquared.txt",
#' rsquare = TRUE)

export_lavaan_results <- function(lavaan_model_fit, path_name = "lavaan_summary.txt", preamble = NULL, ...){
  # check for file extension in path_name
  if(!grepl("\\.[[:alpha:]]{3}$", path_name)){
    error_msg <- "It seems the path_name supplied does not end in a file extension.
Perhaps you forgot to finish with '.txt'?"
    rlang::abort(error_msg)
  }

  # save summary with preamble
  summary_data <- capture.output(
    lavaan::summary(lavaan_model_fit, fit.measures = T, std = T, ...))
  cat(preamble,
      summary_data,
      file = path_name,
      sep = "\n")

  # output message informing where it has been saved
  if(any(grepl("/", path_name))){
         message(paste("Lavaan summary saved successfully to", path_name))
  } else{
         message(
           paste("Lavaan summary saved successfully to",
                 paste(getwd(), path_name, sep = "/"))
                 )
  }
}


#' Model fitting for free factor variance ESEM
#'
#' @param model_syntax An ESEM model fitting create with \cite{syntax_composer()}
#' and `only_fix_crossloadings = FALSE`.
#' @param data Data.frame where rows are observations and columns are indicators. Alternatively,
#' a covariance matrix. If a covariance matrix, `sample.nobs` must be included in `...`
#' @param ... Other `name = value` pairs to \cite{lavaan::lavaan()}.
#'
#'@details Lavaan's \cite{lavaan::cfa()} function automatically fixes the first indicator of
#'factors when factor (residual) variances are set as free parameters with `std.lv = TRUE`.
#'The present function is a wrapper around \cite{lavaan::lavaan()} with the same parameters as
#'the cfa function, except that factor variances are set free and no additional fixing is done.
#'This allows the correct estimation of ESEM-within-CFA models with free factor variances, granted
#'that the model syntax was made with \cite{syntax_composer()} with `only_fix_crossloadings = FALSE` and
#'thus includes the correct number of fixed parameters. See the \cite{esem-as-efa} vignette for an example.
#'
#' @return A lavaan object with the fitted model.
#' @export
#'
fit_free_factor_var_esem <- function(model_syntax, data,...){
  if(!is.matrix(data)){
    lavaan::lavaan(model = model_syntax, data = data,
                   int.ov.free = TRUE, int.lv.free = FALSE,
                   auto.fix.first = FALSE, std.lv = FALSE,
                   auto.fix.single = TRUE, auto.var = TRUE,
                   auto.cov.lv.x = TRUE, auto.efa = TRUE,
                   auto.th = TRUE, auto.delta = TRUE,
                   auto.cov.y = TRUE,...)
  }else{
    lavaan::lavaan(model = model_syntax, sample.cov = data,
                   int.ov.free = TRUE, int.lv.free = FALSE,
                   auto.fix.first = FALSE, std.lv = FALSE,
                   auto.fix.single = TRUE, auto.var = TRUE,
                   auto.cov.lv.x = TRUE, auto.efa = TRUE,
                   auto.th = TRUE, auto.delta = TRUE,
                   auto.cov.y = TRUE,...)
  }

}
