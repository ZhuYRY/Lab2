#' Title
#'
#' @param df a dataframe (Given dataframe: Inpatient_Prospective_Payment)
#' @param option "mean", "median", or "sd" Have to choose one of them.
#'
#' @return the chosen statistic over all of the DRG codes for average Medicare payments
#' @import ggplot2, dplyr, knitr
#' @export
#'
#' @examples
#' data = read.csv("lab2zyry/data/Payments.csv")
#' statistics_f(data, mean)
statistics_f <- function(df, option) {

table <-  df %>%
    # Extract the DRG codes
    mutate(DRG.Num = substr(df$DRG.Definition, 0, 3)) %>%
    # Group by DRG code
    group_by(DRG.Num) %>%
    # select out DRG code and Payments
    select(DRG.Num, Average.Medicare.Payments) %>%
    # Calculate required statistics
    summarise(stat = option(Average.Medicare.Payments))
# Return a well-formatted table
opt <- deparse(substitute(option))

kable(table,
      col.names = c("DRG name", opt))   # Rename columns
}

