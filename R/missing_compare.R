#' Compare missing data
#'
#' @param .data Dataframe.
#' @param dependent Variable to test missingness against other variables with.
#' @param explanatory Variables to have missingness tested against.
#' @param p Logical: Include null hypothesis statistical test.
#' @param na_include Include missing data in explanatory variables as a factor
#'   level.
#' @param ... Other arguments to \code{\link{summary_factorlist}()}.
#'
#' @return A dataframe comparing missing data in the dependent variable across
#'   explanatory variables. Continuous data are compared with an Analysis of Variance F-test by default. 
#'   Discrete data are compared with a chi-squared test.
#' @export
#'
#' @examples
#' library(finalfit)
#'
#' explanatory = c("age", "age.factor", "extent.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#'   ff_glimpse(dependent, explanatory)
#'
#' colon_s %>%
#'  missing_pattern(dependent, explanatory)
#'
#' colon_s %>%
#'   missing_compare(dependent, explanatory)

missing_compare <- function(.data, dependent, explanatory, p = TRUE, na_include = FALSE, ...){
  if(length(dependent) != 1){
    stop("One and only one dependent variable must be provided")
  }
  
  df.out = .data %>% 
    dplyr::mutate(
      !! rlang::sym(dependent) := dplyr::case_when(
        !is.na(!! rlang::sym(dependent)) ~ "Not missing",
        is.na(!! rlang::sym(dependent)) ~ "Missing"
      ) %>% 
        factor(levels = c("Not missing", "Missing"))
    ) %>% 
    ff_relabel_df(.data)
  
  args = list(.data=df.out, dependent=dependent, explanatory=explanatory, p = TRUE, 
              na_include = na_include, ...)
  if(is.null(args$column)) args$column = FALSE
  if(is.null(args$add_dependent_label)) args$add_dependent_label = TRUE
  if(is.null(args$dependent_label_prefix)) args$dependent_label_prefix = "Missing data analysis: "
  
  do.call(summary_factorlist, args)
}
