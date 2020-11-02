#' Title
#'
#' @param df a dataframe (Given dataframe: Inpatient_Prospective_Payment)
#' @param var_name "Average.Total.Payments", "Average.Covered.Charges", "Average.Medicare.Payments"
#'
#' @return a boxplot by DRG codes
#' @import ggplot2, dplyr
#'
#' @export
#'
#' @examples
boxplot_f <- function(df, var_name) {
  title = paste("Boxplot of DRG code vs.", gsub('\\.', ' ', var_name))
  df %>%
    # Select only first 3 digits of DRG code
    mutate(DRG.Num = substr(df$DRG.Definition, 0, 3)) %>%
    # Initialize ggplot with x = DRG code, y = chosen variable
    ggplot(aes(x = DRG.Num, y = get(var_name))) +
    # Make the boxplot
    geom_boxplot() +
    # Customize the plot
    labs(x = "DRG code",
         y = gsub('\\.', ' ', var_name),
         title = title) +
    # Flip x and y axis
    coord_flip()
}

