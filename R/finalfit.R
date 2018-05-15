#' Final output tables for common regression models
#'
#' An "all-in-one" function that takes a single dependent variable with a vector
#' of explanatory variable names (continuous or categorical variables) to
#' produce a final table for publication including summary statistics. The
#' appropriate model is selected on the basis of dependent variable and whether
#' a random effect is specified.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param explanatory Character vector of any length: quoted name(s) of
#'   explanatory variables.
#' @param explanatory_multi Character vector of any length: quoted name(s) of a
#'   subset of explanatory variables for multivariable analysis only (must only
#'   contain variables contained in \code{explanatory})
#' @param random_effect Character vector of length 1: quoted name of random
#'   effects variable. When included mixed effects model generated
#'   (\code{lme4::glmer lme4::lmer}).
#' @param  metrics Logical: include useful model metrics in output in
#'   publication format.
#' @param ... Other arguments to pass to \code{\link{fit2df}}: estimate_name,
#'   p_name, digits, confint_sep.

#' @return Returns a dataframe with the final model table.
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' # Summary, univariable and multivariable analyses of the form:
#' # glm(depdendent ~ explanatory, family="binomial")
#' # lmuni(), lmmulti(), lmmixed(), glmuni(), glmmulti(), glmmixed(), glmmultiboot(),
#' #   coxphuni(), coxphmulti()
#'
#' data(colon_s) # Modified from survival::colon
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#' 	finalfit(dependent, explanatory)
#'
#' # Multivariable analysis with subset of explanatory
#' #   variable set used in univariable analysis
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	finalfit(dependent, explanatory, explanatory_multi)
#'
#' # Summary, univariable and multivariable analyses of the form:
#' # lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	finalfit(dependent, explanatory, explanatory_multi, random_effect)
#'
#' # Include model metrics:
#' colon_s %>%
#' 	finalfit(dependent, explanatory, explanatory_multi,  metrics=TRUE)
#'
#' # Summary, univariable and multivariable analyses of the form:
#' # survival::coxph(dependent ~ explanatory)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#'
#' colon_s %>%
#' 	finalfit(dependent, explanatory)
#'
#' # Rather than going all-in-one, any number of subset models can
#' # be manually added on to a summary_factorlist() table using finalfit.merge().
#' # This is particularly useful when models take a long-time to run or are complicated.
#'
#' # Note requirement for fit_id=TRUE.
#' # `fit2df` is a subfunction extracting most common models to a dataframe.
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#'
#' # Separate tables
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)") -> example.univariable
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' colon_s %>%
#' 	glmmixed(dependent, explanatory, random_effect) %>%
#' 	fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#' 	finalfit_merge(example.univariable) %>%
#' 	finalfit_merge(example.multivariable) %>%
#' 	finalfit_merge(example.multilevel) %>%
#' 	select(-c(fit_id, index)) -> example.final
#' example.final
#'
#' # Cox Proportional Hazards example with separate tables merged together.
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' dependent = "Surv(time, status)"
#'
#' # Separate tables
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example2.summary
#'
#' colon_s %>%
#' 	coxphuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)") -> example2.univariable
#'
#' colon_s %>%
#' 	coxphmulti(dependent, explanatory_multi) %>%
#' 	fit2df(estimate_suffix=" (multivariable)") -> example2.multivariable
#'
#' # Pipe together
#' example2.summary %>%
#' 	finalfit_merge(example2.univariable) %>%
#' 	finalfit_merge(example2.multivariable) %>%
#' 	select(-c(fit_id, index)) -> example2.final
#' example2.final
#'

finalfit = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
										metrics=FALSE, add_dependent_label=TRUE, ...){
	if(is.data.frame(.data)==FALSE) stop(".data is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")

	args = list(.data=.data, dependent=dependent, explanatory=explanatory, explanatory_multi=explanatory_multi,
							random_effect=random_effect, metrics=metrics, ...)

	# What is dependent variable
	d_variable = .data[,names(.data) %in% dependent]
	d_is.factor = is.factor(d_variable) |
		is.character(d_variable)
	d_is.surv = grepl("^Surv[(].*[)]", dependent)

	# Send to method
	if (!d_is.factor & !d_is.surv){
		do.call(finalfit.lm, args)
	} else if (d_is.factor & !d_is.surv){
		do.call(finalfit.glm, args)
	} else if (!d_is.factor & d_is.surv){
		do.call(finalfit.coxph, args)
	}
}
