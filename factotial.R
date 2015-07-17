fact <- function(x) {
  cat(x, "")
  if (x < 1) {
    cat("\n")
    return(1)
  } else {
    return(x * fact(x - 1))
  }
}
