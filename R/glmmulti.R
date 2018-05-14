#' Binomial logistic regression multivariable models: \code{finalfit} model
#' wrapper
#'
#' Using \code{finalfit} conventions, produces multiple multivariable binomial
#' logistic regression models for a set of explanatory variables against a
#' binary dependent.
#'
#' Uses \code{\link[stats]{glm}} with \code{finalfit} modelling conventions.
#' Output can be passed to \code{\link{fit2df}}. Note that this function can
#' take multiple \code{dependent} variables as well, but performs multiple
#' individual models, not a multivariate analysis.
#'
#' @param .data Dataframe.
#' @param dependent Character vector usually of length 1, but can take more than
#'   1 dependent:  name of depdendent variable (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @return A list of multivariable \code{\link[stats]{glm}} fitted model
#'   outputs. Output is of class \code{glmlist}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family \code{finalfit} model wrappers
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
#'
glmmulti <- function(.data, dependent, explanatory){
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = glm(paste(dependent[i], "~", paste(explanatory, collapse="+")),
											data=.data, family="binomial")
	}
	result = setNames(result, dependent)
	class(result) = "glmlist"
	return(result)
}
