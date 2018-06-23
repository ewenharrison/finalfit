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
#' @param ... Other arguments: `X` Design matrix from stanfit modelling. Details
#'   documented else where.
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a list of two dataframes,
#'   one is model parameters, one is model metrics. length two
#'
#' @family \code{finalfit} model extractors
#'
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' library(survival)

#' # glm
#' fit = glm(mort_5yr ~  age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data=colon_s, family="binomial")
#' fit %>%
#'   fit2df(estimate_suffix=" (multivariable)")
#'
#' # glmlist
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")
#'
#' # glmerMod
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")
#'
#' # glmboot
#' ## Note number of draws set to 100 just for speed in this example
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	glmmulti_boot(dependent, explanatory,  R = 100) %>%
#' 	fit2df(estimate_suffix=" (multivariable (BS CIs))")
#'
#' # lm
#' fit = lm(nodes ~  age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data=colon_s)
#' fit %>%
#'   fit2df(estimate_suffix=" (multivariable)")
#'
#' # lmerMod
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")
#'
#' # coxphlist
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#'
#' colon_s %>%
#' 	coxphuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")
#'
#' colon_s %>%
#' 	coxphmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable)")
#'
#' # coxph
#' fit = coxph(Surv(time, status) ~ age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data = colon_s)
#'
#' fit %>%
#' 	fit2df(estimate_suffix=" (multivariable)")

fit2df <- function(.data, condense, metrics, remove_intercept,
                   explanatory_name, estimate_name, estimate_suffix, p_name,
                   digits, confint_sep, ...){
  UseMethod("fit2df")
}
