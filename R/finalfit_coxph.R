#' Final output tables for common regression models
#'
#' Internal function, not called directly. Method for \code{finalfit} generic.
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
#'   p_name, digits, confint_sep
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table
#' @return Returns a dataframe with the final model table.
#'
#' @keywords internal

finalfit.coxph = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
													metrics=FALSE, add_dependent_label=TRUE, ...){

	args = list(...)

	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "HR"
	if (is.null(args$p_name)) args$p_name="p"
	if (is.null(args$digits)) 	args$digits=c(2,2,3)
	if (is.null(args$confint_sep)) args$confint_sep = "-"

	args = list(estimate_name = args$estimate_name,
							p_name = args$p_name,
							digits = args$digits,
							confint_sep = args$confint_sep)


	# Cox proprotional hazards model -----------------------------------------------------------
	# Summary table
	summary.out = suppressWarnings(
		summary_factorlist(.data, dependent=NULL, explanatory, fit_id=TRUE)
	)
	summary.out = summary.out[,-3] # Remove 'all' column with total counts

	# Univariable
	coxphuni_out = coxphuni(.data, dependent, explanatory)
	coxphuni_df =	do.call(fit2df, c(list(.data=coxphuni_out, estimate_suffix = " (univariable)"), args))

	# Multivariable
	if (is.null(explanatory_multi)){
		coxphmulti_out = coxphmulti(.data, dependent, explanatory)
	} else {
		coxphmulti_out = coxphmulti(.data, dependent, explanatory_multi)
	}
	coxphmulti_df = do.call(fit2df,
													c(list(.data=coxphmulti_out, estimate_suffix = " (multivariable)"), args)) #metrics=metrics
	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, coxphuni_df, estimate_name = args$estimate_name)

	# Multi
	df.out = finalfit_merge(df.out, coxphmulti_df, estimate_name = args$estimate_name)

	# # Multi
	# if (metrics == FALSE){
	# 	df.out = finalfit_merge(df.out, glmmulti_df)
	# } else {
	# 	df.out = finalfit_merge(df.out, glmmulti_df[[1]])
	# }
	#
	# if (is.null(random_effect)){
	# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multivariable)"
	# } else {
	# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multilevel)"
	# }

	# Label interactions
	interaction_row = grep(":", df.out$fit_id)
	df.out$label[interaction_row] = 	df.out$fit_id[interaction_row]
	df.out$levels[interaction_row] = "Interaction"

	# Tidy up
	index_fit_id = which(names(df.out)=="fit_id")
	index_index = which(names(df.out)=="index")
	df.out = df.out[,-c(index_fit_id, index_index)]

	# Add dependent name label
	if(add_dependent_label){
		names(df.out)[1] = 	paste0("Dependent: ", dependent_label(.data, dependent))
		names(df.out)[2] = ""
	}

	return(df.out)
	# Add metrics
	# if (metrics == TRUE){
	# 	return(list(df.out, glmmulti_df[[2]]))
	# } else {
	# 	return(df.out)
	# }
}
