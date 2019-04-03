#' Univariable survey-weighted generalised linear models
#'
#' Wrapper for \code{\link[survey]{svyglm}}. Fit a generalised linear model to
#' data from a complex survey design, with inverse-probability weighting and
#' design-based standard errors.
#'
#' @param design Survey design.
#' @param dependent Character vector of length 1: name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param ... Other arguments to be passed to \code{\link[survey]{svyglm}}.
#'
#' @return A list of univariable fitted model outputs. Output is of class
#'   \code{svyglmlist}.
#' @export
#'
#' @examples
#' # Examples taken from survey::svyglm() help page. 
#' 
#' library(survey)
#' library(dplyr)
#' 
#' data(api)
#' dependent = "api00"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' library(survey)
#' library(dplyr)
#'
#' data(api)
#' 
# Label data frame
#' apistrat = apistrat %>% 
#'   mutate(
#'   api00 = ff_label(api00, "API in 2000 (api00)"),
#'   ell = ff_label(ell, "English language learners (percent)(ell)"),
#'   meals = ff_label(meals, "Meals eligible (percent)(meals)"),
#'   mobility = ff_label(mobility, "First year at the school (percent)(mobility)"),
#'   sch.wide = ff_label(sch.wide, "School-wide target met (sch.wide)")
#'   )
#'
#' # Linear example
#' dependent = "api00"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' # Stratified design
#' dstrat = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' 
#' # Univariable fit
#' fit_uni = dstrat %>%
#'   svyglmuni(dependent, explanatory) %>%
#'   fit2df(estimate_suffix = " (univariable)")
#'
#' # Multivariable fit
#' fit_multi = dstrat %>%
#'   svyglmmulti(dependent, explanatory) %>%
#'   fit2df(estimate_suffix = " (multivariable)")
#'
#' # Pipe together
#' apistrat %>%
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(fit_uni) %>% 
#'   ff_merge(fit_multi) %>% 
#'   select(-fit_id, -index) %>%
#'   dependent_label(apistrat, dependent)
#'
#' # Binomial example
#' ## Note model family needs specified and exponentiation if desired
#' 
#' dependent = "sch.wide"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' # Univariable fit
#' fit_uni = dstrat %>%
#'   svyglmuni(dependent, explanatory, family = "quasibinomial") %>%
#'   fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (univariable)")
#'
#' # Multivariable fit
#' fit_multi = dstrat %>%
#'   svyglmmulti(dependent, explanatory, family = "quasibinomial") %>%
#'   fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (multivariable)")
#'
#' # Pipe together
#' apistrat %>%
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(fit_uni) %>% 
#'   ff_merge(fit_multi) %>% 
#'   select(-fit_id, -index) %>%
#'   dependent_label(apistrat, dependent)
svyglmuni <- function(design, dependent, explanatory, ...){
	result <- list()
	for (i in 1:length(explanatory)){
		result[[i]] <- ff_eval(
			survey::svyglm(ff_formula(dependent, explanatory[i]), design = design, ...)
		)
	}
	class(result) = c("svyglmlist", "glmlist")
	return(result)
}


#' Multivariable survey-weighted generalised linear models
#'
#' Wrapper for \code{\link[survey]{svyglm}}. Fit a generalised linear model to
#' data from a complex survey design, with inverse-probability weighting and
#' design-based standard errors.
#'
#' @param design Survey design.
#' @param dependent Character vector of length 1: name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param ... Other arguments to be passed to \code{\link[survey]{svyglm}}.
#'
#' @return A list of univariable fitted model outputs. Output is of class
#'   \code{svyglmlist}.
#' @export
#'
#' @examples
#' # Examples taken from survey::svyglm() help page. 
#' 
#' library(survey)
#' library(dplyr)
#' 
#' data(api)
#' dependent = "api00"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' library(survey)
#' library(dplyr)
#'
#' data(api)
#' 
# Label data frame
#' apistrat = apistrat %>% 
#'   mutate(
#'   api00 = ff_label(api00, "API in 2000 (api00)"),
#'   ell = ff_label(ell, "English language learners (percent)(ell)"),
#'   meals = ff_label(meals, "Meals eligible (percent)(meals)"),
#'   mobility = ff_label(mobility, "First year at the school (percent)(mobility)"),
#'   sch.wide = ff_label(sch.wide, "School-wide target met (sch.wide)")
#'   )
#'
#' # Linear example
#' dependent = "api00"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' # Stratified design
#' dstrat = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' 
#' # Univariable fit
#' fit_uni = dstrat %>%
#'   svyglmuni(dependent, explanatory) %>%
#'   fit2df(estimate_suffix = " (univariable)")
#'
#' # Multivariable fit
#' fit_multi = dstrat %>%
#'   svyglmmulti(dependent, explanatory) %>%
#'   fit2df(estimate_suffix = " (multivariable)")
#'
#' # Pipe together
#' apistrat %>%
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(fit_uni) %>% 
#'   ff_merge(fit_multi) %>% 
#'   select(-fit_id, -index) %>%
#'   dependent_label(apistrat, dependent)
#'
#' # Binomial example
#' ## Note model family needs specified and exponentiation if desired
#' 
#' dependent = "sch.wide"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' # Univariable fit
#' fit_uni = dstrat %>%
#'   svyglmuni(dependent, explanatory, family = "quasibinomial") %>%
#'   fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (univariable)")
#'
#' # Multivariable fit
#' fit_multi = dstrat %>%
#'   svyglmmulti(dependent, explanatory, family = "quasibinomial") %>%
#'   fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (multivariable)")
#'
#' # Pipe together
#' apistrat %>%
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(fit_uni) %>% 
#'   ff_merge(fit_multi) %>% 
#'   select(-fit_id, -index) %>%
#'   dependent_label(apistrat, dependent)
svyglmmulti <- function(design, dependent, explanatory, ...){
	result = list()
	for (i in 1:length(dependent)){
		result[[i]] = ff_eval(
			survey::svyglm(ff_formula(dependent[i], explanatory),  design = design, ...)
		)
	}
	result = setNames(result, dependent)
	class(result) = c("svyglmlist", "glmlist")
	return(result)
}
