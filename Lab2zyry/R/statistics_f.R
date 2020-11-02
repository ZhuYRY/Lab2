#' Title
#'
#' @param df a dataframe (Given dataframe: Inpatient_Prospective_Payment)
#' @param option "mean", "median", or "standard deviation" Have to choose one of them.
#'
#' @return the chosen statistic over all of the DRG codes for average Medicare payments
#' @export
#'
#' @examples
#' data = read.csv("lab2zyry/data/Payments.csv")
#' statistics_f(data, "mean")
statistics_f <- function(df, option) {
  # Select the column we are interested in:
  # 'Average.Medicare.Payments'
  vec = df$'Average.Medicare.Payments'
  if (option == "mean") {                  ## Calculate Mean
    return(mean(vec))
  } else if (option == "median") {         ## Calculate Median
    return(median(vec))
  } else {                                 ## Calculate Standard Deviation
    return(sd(vec))
  }
}

