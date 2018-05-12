#' Linear regression multivariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multiple multivariable linear
#'   regression models for a set of explanatory variables against a continuous dependent.
#'
#' Uses \code{\link[stats]{lm}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}. Note that this function can take multiple \code{dependent}
#'   variables as well, but performs multiple individual models, not a multivariate analysis.
#'
#' @param .data Dataframe.
#' @param dependent Character vector usually of length 1, but can take more than 1
#'   dependent:  name of depdendent variable (must a continuous vector).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @return A list of multivariable \code{\link[stats]{lm}} fitted model outputs.
#'   Output is of class \code{lmlist}.
#'
#' @seealso \code{\link{fit2df}}
#' @family \code{finalfit} model wrappers
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
lmmulti <- function(.data, dependent, explanatory){
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = lm(paste(dependent[i], "~", paste(explanatory, collapse="+")), data=.data)
	}
	result = setNames(result, dependent)
	class(result) = "lmlist"
	return(result)
}
