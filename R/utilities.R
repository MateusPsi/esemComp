abort_bad_indexes <- function(nitems, must, not = NULL) {
  msg <- glue::glue("Item indexes must be integers between 1 and {nitems} when `nitems = {nitems}`")
  if (!is.null(not)) {
    #if there are extra indexes
    if(any(!(not %in% c(1:nitems)))){
      not1 <- not[!(not %in% c(1:nitems))]
      msg <- glue::glue("{msg}; Value(s) '{glue::glue_collapse(not1,\", \")}' are not in the expected indexes.")
    }else
      # if there are missing indexes that should have been supplied
      if(!all(c(1:nitems) %in% not)){
      not2 <- c(1:nitems)[!(c(1:nitems) %in% not)]
      msg <-
        glue::glue("{msg}; Value(s) '{glue::glue_collapse(not2,\", \")}' are missing in the supplied indexes.")
    }
  }

  rlang::abort("error_bad_argument",
        message = msg,
        nitems = nitems,
       # must = must,
        not = not
  )
}
