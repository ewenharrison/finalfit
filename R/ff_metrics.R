#' Generate common metrics for regression model results
#'
#' @param .data Model output.
#'
#' @return Model metrics vector for output.
#' @export
#' 
#' @importFrom stats AIC
#'
#' @examples
#' library(finalfit)
#'
#' # glm
#' fit = glm(mort_5yr ~  age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data=colon_s, family="binomial")
#' fit %>%
#'   ff_metrics()
#'
#' # glmlist
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   glmmulti(dependent, explanatory) %>%
#'   ff_metrics()
#'
#' # glmerMod
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#'   ff_metrics()
#'
#' # lm
#' fit = lm(nodes ~  age.factor + sex.factor + obstruct.factor + perfor.factor,
#'   data=colon_s)
#' fit %>%
#'   ff_metrics()
#'
#' # lmerMod
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmixed(dependent, explanatory, random_effect) %>%
#'   ff_metrics()
#'
#' # coxphlist
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#'
#'
#' colon_s %>%
#'   coxphmulti(dependent, explanatory) %>%
#'   ff_metrics()
#'
#' # coxph
#' fit = survival::coxph(survival::Surv(time, status) ~ age.factor + sex.factor +
#'   obstruct.factor + perfor.factor,
#'   data = colon_s)
#'
#' fit %>%
#'   ff_metrics()

ff_metrics <- function(.data){
	 if (any(class(.data) %in% c("lmlist", "glmlist", "coxphlist")) && length(.data)>1){
	 	stop("Metrics only generated for single models: multiple models supplied to function")
	 }
	UseMethod("ff_metrics")
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics lm
ff_metrics.lm <- function(.data){
	x = .data
	n_model = dim(x$model)[1]
	n_missing = length(summary(x)$na.action)
	n_data = n_model+n_missing
	n_model = dim(x$model)[1]
	loglik = round(logLik(x), 2)
	aic = round(AIC(x), 1)
	r.squared = signif(summary(x)$r.squared, 2)
	adj.r.squared = signif(summary(x)$adj.r.squared, 2)
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_missing,
		", Log-likelihood = ", loglik,
		", AIC = ", aic,
		", R-squared = ", r.squared,
		", Adjusted R-squared = ", adj.r.squared)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics lmlist
ff_metrics.lmlist <- function(.data){
	x = .data[[1]]
	n_model = dim(x$model)[1]
	n_missing = length(summary(x)$na.action)
	n_data = n_model+n_missing
	n_model = dim(x$model)[1]
	loglik = round(logLik(x), 2)
	aic = round(AIC(x), 1)
	r.squared = signif(summary(x)$r.squared, 2)
	adj.r.squared = signif(summary(x)$adj.r.squared, 2)
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_missing,
		", Log-likelihood = ", loglik,
		", AIC = ", aic,
		", R-squared = ", r.squared,
		", Adjusted R-squared = ", adj.r.squared)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics glm
ff_metrics.glm <- function(.data){
	x = .data
	n_data = dim(x$data)[1]
	n_model = dim(x$model)[1]
	aic = round(x$aic, 1)
	auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
	h_l = metrics_hoslem(x$y, x$fitted)
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_data-n_model,
		", AIC = ", aic,
		", C-statistic = ", auc,
		", H&L = ", h_l)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics glmlist
ff_metrics.glmlist <- function(.data){
	x = .data[[1]]
	n_data = dim(x$data)[1]
	n_model = dim(x$model)[1]
	aic = round(x$aic, 1)
	auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
	h_l = metrics_hoslem(x$y, x$fitted)
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_data-n_model,
		", AIC = ", aic,
		", C-statistic = ", auc,
		", H&L = ", h_l)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics lmerMod
ff_metrics.lmerMod <- function(.data){
	x = .data
	n_model = length(x@resp$mu)
	n_groups = summary(x)$ngrps
	loglik = round(summary(x)$logLik, 2)
	aic = round(summary(x)$AICtab[[1]], 1)
	metrics.out = paste0(
		"Number in model = ", n_model,
		", Number of groups = ", paste(n_groups, collapse="/"),
		", Log likelihood = ", loglik,
		", REML criterion = ", aic)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics glmerMod
ff_metrics.glmerMod <- function(.data){
	x = .data
	n_model = length(x@resp$mu)
	n_groups = summary(x)$ngrps
	aic = round(summary(x)$AICtab[[1]], 1)
	auc = round(pROC::roc(x@resp$y, x@resp$mu)$auc[1], 3)
	metrics.out = paste0(
		"Number in model = ", n_model,
		", Number of groups = ", paste(n_groups, collapse="/"),
		", AIC = ", aic,
		", C-statistic = ", auc)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics coxph
ff_metrics.coxph <- function(.data){
	x = .data
	n_model = x$n
	n_missing = length(x$na.action)
	n_data = n_model+n_missing
	n_event = x$nevent
	concordance = summary(x)$concordance
	r.squared = summary(x)$rsq
	logtest = summary(x)$logtest
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_missing,
		", Number of events = ", n_event,
		", Concordance = ", paste0(round_tidy(concordance[1], 3), " (SE = ",
															 round_tidy(concordance[2], 3), ")"),
		", R-squared = ", paste0(round_tidy(r.squared[1], 3), "( Max possible = ",
														 round_tidy(r.squared[2], 3), ")"),
		", Likelihood ratio test = ", paste0(round_tidy(logtest[1], 3), " (df = ",
																				 round(logtest[2], 0), ", p = ",
																				 round_tidy(logtest[3], 3), ")")

	)
	return(metrics.out)
}

#' @export
#' @rdname ff_metrics
#' @method ff_metrics coxphlist
ff_metrics.coxphlist <- function(.data){
	x = .data[[1]]
	n_model = x$n
	n_missing = length(x$na.action)
	n_data = n_model+n_missing
	n_event = x$nevent
	concordance = summary(x)$concordance
	r.squared = summary(x)$rsq
	logtest = summary(x)$logtest
	metrics.out = paste0(
		"Number in dataframe = ", n_data,
		", Number in model = ", n_model,
		", Missing = ", n_missing,
		", Number of events = ", n_event,
		", Concordance = ", paste0(round_tidy(concordance[1], 3), " (SE = ",
															 round_tidy(concordance[2], 3), ")"),
		", R-squared = ", paste0(round_tidy(r.squared[1], 3), "( Max possible = ",
														 round_tidy(r.squared[2], 3), ")"),
		", Likelihood ratio test = ", paste0(round_tidy(logtest[1], 3), " (df = ",
																				 round_tidy(logtest[2], 0), ", p = ",
																				 round_tidy(logtest[3], 3), ")")

	)
	return(metrics.out)
}
