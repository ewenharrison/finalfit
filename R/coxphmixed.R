#' Cox proprotional hazards mixed effects models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multivariable Cox Proportional
#' Hazard regression mixed effects models for a set of explanatory variables and
#' random effects structure against a survival object.
#'
#' Uses \code{\link[coxme]{coxme}} with \code{finalfit} modelling conventions.
#' Output can be passed to \code{\link{fit2df}}. This function is in development
#' given the coxme package is new/
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1:  name of survival object in
#'   form \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param random_effect Character vector of length 1, either, (1) name of random
#'   intercept variable, e.g. "var1", (automatically convered to "(1 | var1)");
#'   or, (2) the full \code{lme4} specification, e.g. "(var1 | var2)". Note
#'   parenthesis MUST be included in (2) but NOT included in (1).
#' @return A multivariable \code{\link[coxme]{coxme}} fitted model. Output is of
#'   class \code{coxme}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' # Cox Proportional Hazards mixed effects analysis.
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#' random_effect = "hospital"
#' colon_s %>%
#' 	coxphmixed(dependent, explanatory, random_effect)
#' 	
coxphmixed <- function(.data, dependent, explanatory, random_effect){
	requireNamespace("survival")
	
	# If single term random effect, default to random intercept model
	if(!grepl("\\|", random_effect)) random_effect = paste0("(1 | ", random_effect, ")")
	
	coxme::coxme(as.formula(ff_formula(dependent, explanatory, random_effect)), data=.data)
}
