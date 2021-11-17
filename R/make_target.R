make_target <- function (nitems, mainloadings){
  target_mat <- psych::make.keys(nitems, mainloadings)
  psych::scrub(target_mat, isvalue = 1)
}

esem <- function(data,  nfactors, target, fm = "ml", rotate = "TargetQ",...){
  psych::fa(data, nfactors, fm = fm,
            rotate = rotate, Target = target,...)
}
# Tucker
