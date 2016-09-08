signifianceToStars <- function(p) {
  if (is.na(p)) {
    return("")
  } else if (p < .001) {
    return("***")
  } else if (p < .01) {
    return("**")
  } else if (p < .05) {
    return("*")
  }

  return("")
}

signifianceToTeX <- function(p, prefix = "\\sym{", suffix = "}") {
  if (is.na(p)) {
    return("")
  } else if (p < .001) {
    return(paste0(prefix, "***", suffix))
  } else if (p < .01) {
    return(paste0(prefix, "**", suffix))
  } else if (p < .05) {
    return(paste0(prefix, "*", suffix))
  }

  return("")
}

show_columns <- function(names) {
  cat("Column names: \\textbf{", paste0(names, collapse = "} & \textbf{"), "} \\\\ \n", sep = "")
}
