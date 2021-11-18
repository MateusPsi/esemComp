make_target <- function (nitems, mainloadings){
  target_mat <- psych::make.keys(nitems, mainloadings)
  psych::scrub(target_mat, isvalue = 1)
}

esem <- function(data,  nfactors, target, bifactor = FALSE, fm = "pa", rotate = "targetQ",...){
  ifelse(bifactor
    ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                          rotate = "TargetT", Target = target,...)
    ,esem_fit <- psych::fa(data, nfactors, fm = fm,
                           rotate = rotate, Target = target,...)
  )
  esem_fit
}
# Tucker
