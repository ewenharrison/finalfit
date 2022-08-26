#' Linear regression multivariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces a multivariable linear regression
#' model for a set of explanatory variables against a continuous dependent.
#'
#' Uses \code{\link[stats]{lm}} with \code{finalfit} modelling conventions.
#' Output can be passed to \code{\link{fit2df}}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1: name of depdendent variable
#'   (must a continuous vector).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param weights Character vector of length 1: name of variabe for weighting. 
#' 'Prior weights' to be used in the fitting process.
#' @param ... Other arguments to pass to \code{\link[stats]{lm}}.
#' @return A multivariable \code{\link[stats]{lm}} fitted model.
#'
#' @seealso \code{\link{fit2df}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmulti(dependent, explanatory) %>%
#'   fit2df()
#' 
lmmulti <- function(.data, dependent, explanatory, weights = "", ...){
	result = ff_eval(
		lm(ff_formula(dependent, explanatory), data = .data, weights = !!sym(weights), ...)
	)
	result$call$formula = formula(result)
	return(result)
}
