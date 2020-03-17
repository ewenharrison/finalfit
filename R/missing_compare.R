#' Compare missing data
#'
#' @param .data Dataframe.
#' @param dependent Variable to test missingness against other variables with.
#' @param explanatory Variables to have missingness tested against.
#' @param na_include Include missing data in explanatory variables as a factor
#'   level.
#' @param ... Other arguments to \code{\link{summary_factorlist}()}.
#'
#' @return A dataframe comparing missing data in the dependent variable across
#'   explanatory variables. Continuous data are compared with a Kruskal Wallis
#'   test. Discrete data are compared with a chi-squared test.
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

missing_compare <- function(.data, dependent, explanatory, na_include = FALSE, ...){
  if(length(dependent) != 1){
    stop("One and only one dependent variable must be provided")
  }
  
  vlabels = .data %>% 
    extract_variable_label()
  
  df.out = .data %>% 
    dplyr::mutate(
      !! rlang::sym(dependent) := dplyr::case_when(
        !is.na(!! rlang::sym(dependent)) ~ "Not missing",
        is.na(!! rlang::sym(dependent)) ~ "Missing"
      ) %>% 
        factor(levels = c("Not missing", "Missing"))
    ) %>% 
    ff_relabel(vlabels)
  
  # Old code. Remove after checks. 
  # # Extract variables
  # d_vars = .data[ ,names(.data) %in% dependent, drop = FALSE]
  # e_vars = .data[ ,names(.data) %in% explanatory]
  # 
  # # Extract dependent variable as lost in next move
  # d_label = attr(d_vars[,1], "label")
  # 
  # # Create new variable for missings
  # d_vars[,1] = as.character(d_vars[, 1])
  # d_vars[!is.na(d_vars)] = 0
  # d_vars[is.na(d_vars)] = 1
  # d_vars[,1] = factor(d_vars[,1], levels = c(0, 1), labels = c("Not missing", "Missing"))
  # 
  # # df.out
  # df.out = data.frame(d_vars, e_vars)
  # attr(df.out[,1], "label") = d_label

  args = list(.data=df.out, dependent=dependent, explanatory=explanatory, 
              na_include = na_include, ...)
  if(is.null(args$p)) args$p = TRUE
  if(is.null(args$add_dependent_label)) args$add_dependent_label = TRUE
  if(is.null(args$dependent_label_prefix)) args$dependent_label_prefix = "Missing data analysis: "
  
  do.call(summary_factorlist, args)
}
