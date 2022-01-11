# if the value of x is null, use y
# for example, given a val that, if
# null should be an NA of type character
# either:
#   `nullable(val, NA_character_)`
#   `val %||% NA_character_`
nullable <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  return(x)
}

`%||%` <- nullable
