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
#' data(api)
#' dependent = "api00"
#' explanatory = c("ell", "meals", "mobility")
#' 
#' # Stratified design
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' 
#' # Linear example
#' ## Note respects svgglm default settings, family=stats::gaussian()
#' apistrat %>% 
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>% 
#'   ff_merge(
#'     dstrat %>% 
#'       svyglmuni(dependent, explanatory) %>% 
#'       fit2df(estimate_suffix = " (univariable)")
#'     ) %>% 
#'   ff_merge(
#'     dstrat %>% 
#'       svyglmmulti(dependent, explanatory) %>% 
#'       fit2df(estimate_suffix = " (multivariable)")
#'     ) %>% 
#'   select(-fit_id, -index) %>% 
#'   dependent_label(apistrat, dependent)
#'  
#' # Binomial example
#' ## Require to specify family = "quasibinomial" and (if desired)
#' ## to exponentiate result and name it Odds ratio
#' 
#' dependent = "sch.wide"
#' 
#' apistrat %>%
#' summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(
#'   dstrat %>%
#'     svyglmuni(dependent, explanatory, family = "quasibinomial") %>%
#'     fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (univariable)")
#'     ) %>%
#'     ff_merge(
#'   dstrat %>%
#'     svyglmmulti(dependent, explanatory, family = "quasibinomial") %>%
#'     fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (multivariable)")
#'     ) %>%
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
#' # Stratified design
#' dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#' 
#' # Linear example
#' apistrat %>% 
#'   summary_factorlist(dependent, explanatory, fit_id = TRUE) %>% 
#'   ff_merge(
#'     dstrat %>% 
#'       svyglmuni(dependent, explanatory) %>% 
#'       fit2df(estimate_suffix = " (univariable)")
#'     ) %>% 
#'   ff_merge(
#'     dstrat %>% 
#'       svyglmmulti(dependent, explanatory) %>% 
#'       fit2df(estimate_suffix = " (multivariable)")
#'     ) %>% 
#'   select(-fit_id, -index) %>% 
#'   dependent_label(apistrat, dependent)
#'  
#' # Binomial example
#' dependent = "sch.wide"
#' 
#' apistrat %>%
#' summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
#'   ff_merge(
#'   dstrat %>%
#'     svyglmuni(dependent, explanatory, family = "quasibinomial") %>%
#'     fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (univariable)")
#'     ) %>%
#'     ff_merge(
#'   dstrat %>%
#'     svyglmmulti(dependent, explanatory, family = "quasibinomial") %>%
#'     fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (multivariable)")
#'     ) %>%
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
