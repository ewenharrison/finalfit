#' Cox proprotional hazards multivariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multivariable Cox
#' Proportional Hazard regression models for a set of explanatory variables
#' against a survival object.
#'
#' Uses \code{\link[survival]{coxph}} with \code{finalfit} modelling
#' conventions. Output can be passed to \code{\link{fit2df}}.
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1:  name of survival object in
#'   form \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param ... Other arguments to pass to \code{\link[survival]{coxph}}.
#' @return A multivariable \code{\link[survival]{coxph}} fitted model
#'   output. Output is of class \code{coxph}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' # Cox Proportional Hazards multivariable analysis.
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#' colon_s %>%
#' 	coxphmulti(dependent, explanatory) %>%
#' 	fit2df()

coxphmulti <- function(.data, dependent, explanatory, ...){
	requireNamespace("survival")
	result = ff_eval(
		coxph(as.formula(paste0(dependent, "~",
														paste(explanatory, collapse="+"))), data=.data, ...)
	)
	result$call$formula = formula(result)
	return(result)
}


