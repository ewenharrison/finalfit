#' Binomial logistic regression univariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multiple univariable binomial logistic
#'   regression models for a set of explanatory variables against a binary dependent.
#'
#' Uses \code{\link[stats]{glm}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}.
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1:  name of depdendent variable (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param family Character vector quoted or unquoted of the error distribution
#'   and link function to be used in the model, see \code{\link[stats]{glm}}.
#' @param ... Other arguments to pass to \code{\link[stats]{glm}}. 
#' @return A list of univariable \code{\link[stats]{glm}} fitted model outputs.
#'   Output is of class \code{glmlist}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")
#'
#'
glmuni <- function(.data, dependent, explanatory, family = "binomial", ...){
	result <- list()
	for (i in 1:length(explanatory)){
		result[[i]] <- ff_eval(
			glm(ff_formula(dependent, explanatory[i]), data = .data, family = family, ...)
		)
	}
	class(result) = "glmlist"
	return(result)
}
