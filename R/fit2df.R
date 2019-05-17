#' Extract model fit results to dataframe (generic): \code{finalfit} model
#' extractors
#'
#' Takes output from \code{finalfit} model wrappers and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df} is a generic (S3) function for model extract.
#'
#' @param .data Output from \code{finalfit} model wrappers.
#' @param condense Logical: when true, effect estimates, confidence intervals
#'   and p-values are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted.
#' @param remove_intercept Logical: remove the results for the intercept term.
#' @param explanatory_name Name for this column in output
#' @param estimate_name Name for this column in output
#' @param estimate_suffix Appeneded to estimate name
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param exp Currently GLM only. Exponentiate coefficients and confidence
#'   intervals. Defaults to TRUE.
#' @param confint_type One of \code{c("profile", "default")} for GLM models
#'   (\code{\link[MASS]{confint.glm}}) or \code{c("profile", "Wald", "boot")}
#'   for \code{glmer/lmer} models (\code{\link[lme4]{confint.merMod}}.). Not
#'   implemented for \code{lm, coxph or coxphlist}.
#' @param confint_level The confidence level required.
#' @param confint_sep String to separate confidence intervals, typically "-" or
#'   " to ".
#' @param ... Other arguments: \code{X}: Design matrix from stanfit modelling.
#'   Details documented else where.
#'
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a
#'   list of two dataframes, one is model parameters, one is model metrics.
#'   length two
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
#'   glmmulti(dependent, explanatory) %>%
#'   fit2df(estimate_suffix=" (univariable)")
#'
#' # glmerMod
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#'   fit2df(estimate_suffix=" (multilevel)")
#'
#' # glmboot
#' ## Note number of draws set to 100 just for speed in this example
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   glmmulti_boot(dependent, explanatory,  R = 100) %>%
#'   fit2df(estimate_suffix=" (multivariable (BS CIs))")
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
#'   fit2df(estimate_suffix=" (multilevel")
#'
#' # coxphlist
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#'
#' colon_s %>%
#'   coxphuni(dependent, explanatory) %>%
#'   fit2df(estimate_suffix=" (univariable)")
#'
#' colon_s %>%
#'   coxphmulti(dependent, explanatory) %>%
#'   fit2df(estimate_suffix=" (multivariable)")
#'
#' # coxph
#' fit = coxph(Surv(time, status) ~ age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data = colon_s)
#'
#' fit %>%
#'   fit2df(estimate_suffix=" (multivariable)")
#' 	
#' # crr: competing risks
#' library(cmprsk)
#' library(magrittr)
#' 	
#' # Simulated data from package 
#' set.seed(10)
#' ftime = rexp(200)
#' fstatus = sample(0:2, 200, replace=TRUE)
#' cov = matrix(runif(600), nrow=200)
#' dimnames(cov)[[2]] = c('var1','var2','var3')
#' df = data.frame(ftime, fstatus, cov)
#' 	
#' df %$% 
#'   crr(ftime, fstatus, cov) %>%
#'   fit2df()

fit2df <- function(...){
	UseMethod("fit2df")
}



#' Extract \code{glm::lm} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.lm} is the model extract method for \code{\link[stats]{lm}}.
#'
#' @rdname fit2df
#' @method fit2df lm
#' @export
#'

fit2df.lm <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
											explanatory_name = "explanatory",
											estimate_name = "Coefficient",
											estimate_suffix = "",
											p_name = "p",
											digits=c(2,2,3),
											confint_level = 0.95,
											confint_sep = " to ", ...){
	
	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name, digits=digits,)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{lmuni} and \code{lmmulti} model fit results to dataframe:
#' \code{finalfit} model extracters
#'
#' \code{fit2df.lmlist} is the model extract method for \code{lmuni} and
#' \code{lmmulti}.
#'
#' @rdname fit2df
#' @method fit2df lmlist
#' @export

fit2df.lmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													explanatory_name = "explanatory",
													estimate_name = "Coefficient",
													estimate_suffix = "",
													p_name = "p", digits=c(2,2,3),
													confint_level = 0.95,
													confint_sep = " to ", ...){
	
	if (metrics==TRUE && length(.data)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}
	
	df.out = .data %>% 
		purrr::map_dfr(extract_fit, explanatory_name=explanatory_name,
									 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
									 p_name=p_name,  confint_level=confint_level)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}


#' Extract \code{glm} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.glm} is the model extract method for standard
#' \code{\link[stats]{glm}} models, which have not used \code{finalfit} model
#' wrappers.
#'
#' @rdname fit2df
#' @method fit2df glm
#' @export
#'
fit2df.glm <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
											 explanatory_name = "explanatory",
											 estimate_name = "OR",
											 estimate_suffix = "",
											 p_name = "p",
											 digits=c(2,2,3),
											 exp = TRUE,
											 confint_type = "profile",
											 confint_level = 0.95,
											 confint_sep = "-", ...){
	
	df.out = extract_fit(.data = .data, explanatory_name = explanatory_name,
											 estimate_name = estimate_name, estimate_suffix = estimate_suffix,
											 exp = exp, 
											 confint_type = confint_type,
											 confint_level = confint_level,
											 p_name=p_name)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name = explanatory_name,
													estimate_name = estimate_name, estimate_suffix = estimate_suffix,
													p_name = p_name, digits = digits, confint_sep = confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{glmboot} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.glmboot} is the model extract method for \code{\link{glmmulti_boot}} models.
#'
#' @rdname fit2df
#' @method fit2df glmboot
#' @export

fit2df.glmboot = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													explanatory_name = "explanatory",
													estimate_name = "OR",
													estimate_suffix = "",
													p_name = "p",
													digits=c(2,2,3),
													confint_sep = "-", ...){
	if(metrics == TRUE) warning("Metrics not currently available for this model")
	
	x = .data
	d.estimate = digits[1]
	d.confint = digits[2]
	d.p = digits[3]
	
	R = dim(x$t)[1]
	
	df.out = data.frame(
		explanatory = names(x$t0),
		estimate = exp(x$t0))
	for (i in 1:dim(df.out)[1]){
		df.out$L95[i] = exp(sort(x$t[,i]))[floor(R*0.025)]
		df.out$U95[i] = exp(sort(x$t[,i]))[floor((R*0.975)+1)]
		df.out$p[i] = ifelse(x$t0[i] >= 0, mean(x$t[,i]<0)*2, mean(x$t[,i]>0)*2)
	}
	df.out$estimate = round(df.out$estimate, d.estimate)
	df.out$L95 = round(df.out$L95, d.confint)
	df.out$U95 = round(df.out$U95, d.confint)
	df.out$p = round(df.out$p, d.p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	return(df.out)
}

#' Extract \code{glmuni} and \code{glmmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.glmlist} is the model extract method for \code{glmuni} and \code{glmmulti}.
#'
#' @rdname fit2df
#' @method fit2df glmlist
#' @export

fit2df.glmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													 explanatory_name = "explanatory",
													 estimate_name = "OR",
													 estimate_suffix = "",
													 p_name = "p",
													 digits=c(2,2,3),
													 exp = TRUE, 
													 confint_type = "profile",
													 confint_level = 0.95,
													 confint_sep = "-", ...){
	
	if (metrics==TRUE && length(.data)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}
	
	df.out = .data %>% 
		purrr::map_dfr(extract_fit, explanatory_name = explanatory_name,
									 estimate_name = estimate_name, estimate_suffix = estimate_suffix,
									 p_name = p_name, exp = exp, 
									 confint_type = confint_type,
									 confint_level = confint_level,
									 digits=digits)
	
	if (condense==TRUE){
		df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}


#' Extract \code{svyglmuni} and \code{svyglmmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.svyglmlist} is the model extract method for \code{svyglmuni} and \code{svyglmmulti}.
#'
#' @rdname fit2df
#' @method fit2df svyglmlist
#' @export

fit2df.svyglmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													 explanatory_name = "explanatory",
													 estimate_name = "Coefficient",
													 estimate_suffix = "",
													 p_name = "p",
													 digits=c(2,2,3),
													 exp = FALSE, 
													 confint_type = "profile",
													 confint_level = 0.95,
													 confint_sep = "-", ...){
	
	if (metrics==TRUE && length(.data)>1){
		stop("Metrics only generated for single models: multiple models supplied to function")
	}
	
	df.out = .data %>% 
		purrr::map_dfr(extract_fit, explanatory_name = explanatory_name,
									 estimate_name = estimate_name, estimate_suffix = estimate_suffix,
									 p_name = p_name, exp = exp, 
									 confint_type = confint_type,
									 confint_level = confint_level,
									 digits=digits)
	
	if (condense==TRUE){
		df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}


#' Extract \code{lmerMod} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.lmerMod} is the model extract method for standard
#' \code{lme4::\link[lme4]{lmer}} models and for the
#' \code{finalfit::\link{lmmixed}} model wrapper.
#'
#' @rdname fit2df
#' @method fit2df lmerMod
#' @export

fit2df.lmerMod = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													explanatory_name = "explanatory",
													estimate_name = "Coefficient",
													estimate_suffix = "",
													p_name = "p",
													digits=c(2,2,3),
													confint_type = "Wald",
													confint_level = 0.95,
													confint_sep = "-", ...){
	
	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name, confint_type = confint_type, confint_level = confint_level)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{glmerMod} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.glmerMod} is the model extract method for standard
#' \code{lme4::\link[lme4]{glmer}} models and for the
#' \code{finalfit::\link{glmmixed}} model wrapper.
#'
#' @rdname fit2df
#' @method fit2df glmerMod
#' @export

fit2df.glmerMod = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													 explanatory_name = "explanatory",
													 estimate_name = "OR",
													 estimate_suffix = "",
													 p_name = "p",
													 digits=c(2,2,3),
													 confint_type = "Wald",
													 confint_level = 0.95,
													 confint_sep = "-", ...){
	
	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name, confint_type = confint_type,
											 confint_level = confint_level)
	
	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}
	
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{survival::coxph} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.coxph} is the model extract method for \code{survival::\link[survival]{coxph}}.
#'
#' @rdname fit2df
#' @method fit2df coxph
#' @export
#'
fit2df.coxph <- function(.data, condense=TRUE, metrics=FALSE,
												 explanatory_name = "explanatory",
												 estimate_name = "HR",
												 estimate_suffix = "",
												 p_name = "p",
												 digits=c(2,2,3),
												 confint_sep = "-", ...){
	
	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name)
	
	if (condense==TRUE){
		df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{cmprsk::crr} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.crr} is the model extract method for \code{cmprsk::\link[cmprsk]{crr}}.
#'
#' @rdname fit2df
#' @method fit2df crr
#' @export
#'
fit2df.crr <- function(.data, condense=TRUE, metrics=FALSE,
												 explanatory_name = "explanatory",
												 estimate_name = "HR",
												 estimate_suffix = "",
												 p_name = "p",
												 digits=c(2,2,3),
												 confint_sep = "-", ...){
	
	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name)
	
	if (condense==TRUE){
		df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}

#' Extract \code{coxphuni} and \code{coxphmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.coxphlist} is the model extract method for \code{coxphuni} and \code{coxphmulti}.
#'
#' @rdname fit2df
#' @method fit2df coxphlist
#' @export

fit2df.coxphlist <- function(.data, condense=TRUE, metrics=FALSE,
														 explanatory_name = "explanatory",
														 estimate_name = "HR",
														 estimate_suffix = "",
														 p_name = "p",
														 digits=c(2,2,3),
														 confint_sep = "-", ...){
	#if(metrics==TRUE) warning("Metrics not currently available for this model")
	
	df.out = .data %>% 
		purrr::map_dfr(extract_fit, explanatory_name=explanatory_name,
									 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
									 p_name=p_name, digits=digits)
	
	if (condense==TRUE){
		df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}
	
	# Extract model metrics
	if (metrics==TRUE){
		metrics.out = ff_metrics(.data)
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
}


#' Extract \code{stanfit} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.stanfit} is the model extract method for our standard Bayesian
#' hierarchical binomial logistic regression models. These models will be fully
#' documented separately. However this should work for a single or multilevel
#' Bayesian logistic regression done in Stan, as long as the fixed effects are
#' specified in the parameters block as a vector named \code{beta}, of length
#' \code{P}, where \code{P} is the number of fixed effect parameters. e.g.
#' parameters{ vector[P] beta; }
#'
#' @rdname fit2df
#' @method fit2df stanfit
#' @export
#'
fit2df.stanfit = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
													explanatory_name = "explanatory",
													estimate_name = "OR",
													estimate_suffix = "",
													p_name = "p",
													digits=c(2,2,3),
													confint_sep = "-", ...){
	args = list(...)

	if(is.null(args$X)) stop("Must include design matrix from Stan procedure, e.g. X=X")

	df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
											 estimate_name=estimate_name, estimate_suffix=estimate_suffix,
											 p_name=p_name, digits=digits, X=args$X)

	if (condense==TRUE){
		df.out = condense_fit(df.out, explanatory_name=explanatory_name,
													estimate_name=estimate_name, estimate_suffix=estimate_suffix,
													p_name=p_name, digits=digits, confint_sep=confint_sep)
	}

	if (remove_intercept==TRUE){
		df.out = remove_intercept(df.out)
	}

	# Extract model metrics
	## This needs an ff_metrics() method
	if (metrics==TRUE){
		# n_data = dim(x$data)[1] # no equivalent here
		n_model = dim(args$X)[1]
		# aic = round(x$aic, 1) # add WAIC later?
		# auc = round(roc(x$y, x$fitted)$auc[1], 3) # Add predicted mu later?
		metrics.out = paste0(
			#	"Number in dataframe = ", n_data,
			", Number in model = ", n_model)
		#	", Missing = ", n_data-n_model,
		#	", AIC = ", aic,
		#	", C-statistic = ", auc)
	}

	if (metrics==TRUE){
		return(list(df.out, metrics.out))
	} else {
		return(df.out)
	}
	return(df.out)
}
