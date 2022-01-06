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
#' fit <- cfa(model_syntax, sample.cov = as.matrix(tucker), sample.nobs = 710)
#'
#' # McDonald's Omega
#' omega_esem(fit, target_mat)
omega_esem <- function(esem_model_fit, target_matrix){
  target_order <- dimnames(target_matrix)[[2]]
  loadings <- lavaan::lavInspect(esem_model_fit,"std")[["lambda"]][,target_order]
  loadings_sqr <- sum(abs(loadings[is.na(target_matrix)]))^2

  res_var <- lavaan::lavInspect(esem_model_fit,"std")[["theta"]]
  res_var <- sum(diag(res_var))

  loadings_sqr*(1/(loadings_sqr+res_var))

}


export_lavaan_summary <- function(lavaan_model_fit, path_name = "lavaan_summary.txt", preamble = NULL){
  # check for file extension in path_name
  if(!grepl("\\.[[:alpha:]]{3}$", path_name)){
    error_msg <- "It seems the path_name supplied does not end in an file extension.
Perhaps you forgot to finish with '.txt'?"
    rlang::abort(error_msg)
  }

  # save summary with preamble
  summary_data <- capture.output(lavaan::summary(lavaan_model_fit, fit, fit.measures = T, std = T))
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

