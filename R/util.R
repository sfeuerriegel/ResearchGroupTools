signifianceToStars <- function(p) {
  if (is.na(p)) {
    return("")
  } else if (p < .001) {
    return("***")
  } else if (p < .01) {
    return("** ")
  } else if (p < .05) {
    return("*  ")
  }

  return("   ")
}



