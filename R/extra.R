#' McDonald's Omega for ESEM
#'
#' @param esem_model_fit
#' @param target_matrix
#'
#' @return
#' @export
#'
#' @examples
omega_esem <- function(esem_model_fit, target_matrix){
  target_order <- dimnames(target_rot)[[2]]
  loadings <- lavaan::lavInspect(cfa_fit,"std")[["lambda"]][,target_order]
  loadings_sqr <- sum(abs(loadings[is.na(target_rot)]))^2

  res_var <- lavaan::lavInspect(cfa_fit,"std")[["theta"]]
  res_var <- sum(diag(res_var))

  loadings_sqr*(1/(loadings_sqr+res_var))

}
