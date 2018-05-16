#' Extract model fit results to dataframe (generic): \code{finalfit} model extractors
#'
#' Takes output from \code{finalfit} model wrappers and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df} is a generic (S3) function for model extract.
#'
#' @param .data Output from \code{finalfit} model wrappers.
#' @param condense Logical: when true, effect estimates, confidence intervals and p-values
#'   are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param remove_intercept Logical: remove the results for the intercept term.
#' @param explanatory_name Name for this column in output
#' @param estimate_name Name for this column in output
#' @param estimate_suffix Appeneded to estimate name
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param confint_sep String to separate confidence intervals, typically "-" or
#'   " to ".
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a list of two dataframes,
#'   one is model parameters, one is model metrics. length two
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")

fit2df <- function(.data, condense, metrics, remove_intercept,
                   explanatory_name, estimate_name, estimate_suffix, p_name,
                   digits, confint_sep, ...){
  UseMethod("fit2df")
}
