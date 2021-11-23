# Given a psych::efa object, make a loadings dt for syntax_composer
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
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")

}
