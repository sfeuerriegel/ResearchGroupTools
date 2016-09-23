#' Extracts a column from a tbl object
#'
#' @param object Object of type \code{\link[dplyr]{tbl}}
#' @param column Column identifier, name as string or integer depending on function.
#' @return Column as vector
#' @source Tommy O'Dell: \url{http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector}
#' @examples
#' library(dplyr)
#'
#' d <- data_frame(x = 1:10,
#'                 y = rnorm(10))
#' d %>% pull(x)
#' d %>% pull("x")
#'
#' v <- "x"
#' d %>% pull_string(v)
#'
#' d %>% pull_ith(1)
#' @rdname pull
#' @export
pull <- function(object, column) {
  if (is.name(substitute(column))) {
    cols <- deparse(substitute(column))
  } else {
    cols <- column
  }

  return(object[, cols, drop = FALSE][[1]])
}

#' @rdname pull
#' @export
pull_string <- function(object, column) {
  return(object[, column, drop = FALSE][[1]])
}

#' @rdname pull
#' @export
pull_ith <- function(object, column) {
  return(object[, colnames(object)[column], drop = FALSE][[1]])
}

#' Duplicates data with low time resolution to match high resolution
#'
#' Let data in low resolution be given (e.g. monthly, quarterly). This
#' is then matched to a high resolution (e.g. daily) and the corresponding
#' entries are copied.
#' @param date_target Time series of desired output (high resolution).
#' @param df Data frame with the low resolution data.
#' @param by_column String which identifies the date column in \code{df}.
#' As a default, it uses \code{"Date"}.
#' @return Returns data frame with copied entries. The response is sorted
#' in chronological order.
#' @examples
#' ts <- data.frame(Date = seq(from = as.Date("2000-01-01"), to = as.Date("2000-03-31"), by = "1 day"))
#'
#' df_monthly <- data.frame(Month = c(as.Date("2000-01-31"),
#'                                    as.Date("2000-02-29"),
#'                                    as.Date("2000-03-31")),
#'                          Values = 1:3)
#' df_daily <- completeLowResolutionData(ts$Date, df_monthly, "Month")
#'
#' # example of how to bind things together
#' library(dplyr)
#' ts <- ts %>%
#'   left_join(df_daily, by = c("Date" = "Month"))
#' @export
completeLowResolutionData <- function(date_target, df, by_column = "Date") {
  df <- df %>%
    dplyr::arrange_(.dots = by_column)

  n_obs <- (df %>% pull_string(by_column) - dplyr::lag(df %>% pull_string(by_column)))[-1]
  n_obs <- c(n_obs, length(date_target) - sum(n_obs))

  result <- data.frame(XXX = date_target) %>%
    dplyr::rename_(.dots = setNames("XXX", by_column)) %>%
    dplyr::bind_cols(df[rep(1:nrow(df), n_obs), ] %>% dplyr::select(-dplyr::matches(by_column)))

  return(result)
}


