#' Final output tables for common regression models
#'
#' An "all-in-one" function that takes a single dependent variable with a vector
#' of explanatory variable names (continuous or categorical variables) to
#' produce a final table for publication including summary statistics. The
#' appropriate model is selected on the basis of dependent variable and whether
#' a random effect is specified.
#'
#' @param .data Data frame or tibble.
#' @param dependent Character vector of length 1:  quoted name of dependent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: quoted name(s) of
#'   explanatory variables.
#' @param explanatory_multi Character vector of any length: quoted name(s) of a
#'   subset of explanatory variables to generate reduced multivariable model
#'   (must only contain variables contained in \code{explanatory}).
#' @param random_effect Character vector of length 1, either, (1) name of random
#'   intercept variable, e.g. "var1", (automatically convered to "(1 | var1)");
#'   or, (2) the full \code{lme4} specification, e.g. "(var1 | var2)". Note
#'   parenthesis MUST be included in (2) but NOT included in (1).
#' @param column Logical: Compute margins by column rather than row.
#' @param keep_models Logical: include full multivariable model in output when
#'   working with reduced multivariable model (\code{explanatory_multi}) and/or
#'   mixed effect models (\code{random_effect}).
#' @param  metrics Logical: include useful model metrics in output in
#'   publication format.
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table.
#' @param dependent_label_prefix Add text before dependent label.
#' @param dependent_label_suffix Add text after dependent label.
#' @param keep_fit_id Keep original model output coefficient label (internal).
#' @param ... Other arguments to pass to \code{\link{fit2df}}:
#'   \code{estimate_name, digits, confint_type, confint_level,
#'   confint_sep}.

#' @return Returns a data frame with the final model table.
#'
#' @family finalfit all-in-one functions
#' @export
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
#'   finalfit(dependent, explanatory)
#'
#' # Multivariable analysis with subset of explanatory
#' #   variable set used in univariable analysis
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   finalfit(dependent, explanatory, explanatory_multi)
#'
#' # Summary, univariable and multivariable analyses of the form:
#' # lme4::glmer(dependent ~ explanatory + (1 | random_effect), family="binomial")
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#' # colon_s %>%
#' #   finalfit(dependent, explanatory, explanatory_multi, random_effect)
#'
#' # Include model metrics:
#' colon_s %>%
#'   finalfit(dependent, explanatory, explanatory_multi,  metrics=TRUE)
#'
#' # Summary, univariable and multivariable analyses of the form:
#' # survival::coxph(dependent ~ explanatory)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#'
#' colon_s %>%
#'   finalfit(dependent, explanatory)
#'
#' # Rather than going all-in-one, any number of subset models can
#' # be manually added on to a summary_factorlist() table using finalfit.merge().
#' # This is particularly useful when models take a long-time to run or are complicated.
#'
#' # Note requirement for fit_id=TRUE.
#' # `fit2df` is a subfunction extracting most common models to a dataframe.
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'   finalfit(dependent, explanatory, metrics=TRUE)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = 'mort_5yr'
#'
#' # Separate tables
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary
#'
#' colon_s %>%
#'   glmuni(dependent, explanatory) %>%
#'   fit2df(estimate_suffix=" (univariable)") -> example.univariable
#'
#' colon_s %>%
#'   glmmulti(dependent, explanatory) %>%
#'   fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' # Edited as CRAN slow to run these
#' # colon_s %>%
#' #   glmmixed(dependent, explanatory, random_effect) %>%
#' #   fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#'   finalfit_merge(example.univariable) %>%
#'   finalfit_merge(example.multivariable, last_merge = TRUE)
#' # finalfit_merge(example.multilevel)

finalfit = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
										column=NULL, keep_models=FALSE, metrics=FALSE, add_dependent_label=TRUE,
										dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
										keep_fit_id = FALSE, ...){
	if(is.data.frame(.data)==FALSE) stop(".data is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")
	if(.data[ ,names(.data) %in% explanatory] %>% error_colon_fct_levels()){
		stop("Colons (:) not allowed in factor-level names. Check with ff_glimpse(.data). Recode: forcats::fct_recode()")
	}

	# Fix tibble issue
	if(any(class(.data) %in% c("tbl_df", "tbl")))  .data = data.frame(.data)

	# What is dependent variable
	d_variable = .data[,names(.data) %in% dependent]
	d_is.factor = is.factor(d_variable) |
		is.character(d_variable)
	d_is.surv = grepl("^Surv[(].*[)]", dependent)
	
	# Column proportions for CPH tables
	if(is.null(column)){
		if(d_is.surv){
			column = TRUE
		} else {
			column = FALSE
		}
	}

	# Arguments to send
	args = list(.data=.data, dependent=dependent, explanatory=explanatory,
							explanatory_multi=explanatory_multi,
							random_effect=random_effect, column = column,
							keep_models=keep_models,
							metrics=metrics,
							add_dependent_label = add_dependent_label,
							dependent_label_prefix=dependent_label_prefix,
							dependent_label_suffix=dependent_label_suffix, 
							keep_fit_id=keep_fit_id, ...=...)
	
	# Send to method
	if (!d_is.factor & !d_is.surv){
		do.call(finalfit.lm, args)
	} else if (d_is.factor & !d_is.surv){
		do.call(finalfit.glm, args)
	} else if (!d_is.factor & d_is.surv){
		do.call(finalfit.coxph, args)
	}
}


#' Final output tables for common regression models: lm method
#'
#' \code{finalfit.lm} method (not called directly)
#'
#' @rdname finalfit
#' @export
finalfit.lm = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
											 column = FALSE, keep_models=FALSE, metrics=FALSE, add_dependent_label = TRUE,
											 dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
											 keep_fit_id=FALSE, ...){

	args = list(...)

	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "Coefficient"
	if (is.null(args$confint_sep)) args$confint_sep = " to "

	# Linear regression model ------------------------------------------
	# Summary table
	summary.out = suppressWarnings(
		summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
											 column=column, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)
	)

	# Univariable
	lmuni.out = lmuni(.data, dependent, explanatory)
	lmuni.df = do.call(fit2df, c(list(.data=lmuni.out, estimate_suffix = " (univariable)"), args))

	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, lmuni.df, estimate_name = args$estimate_name)

	# This is standard approach, which is why written out in full but duplicated somewhat below
	## Come back and reduce the conditional logic here.
	## Fast though, as only required models are being made.
	if(keep_models == FALSE){
		# Multivariable/Mixed
		if (is.null(random_effect)){
			if (is.null(explanatory_multi)){
				lmmulti.out = lmmulti(.data, dependent, explanatory)
			} else {
				lmmulti.out = lmmulti(.data, dependent, explanatory_multi)
			}
			lmmulti.df = do.call(fit2df,
													 c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)"), args))
		} else if (is.null(random_effect) == FALSE){
			if (is.null(explanatory_multi)){
				lmmulti.out = lmmixed(.data, dependent, explanatory, random_effect)
			} else {
				lmmulti.out = lmmixed(.data, dependent, explanatory_multi, random_effect)
			}
			lmmulti.df = do.call(fit2df,
													 c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)"), args))
		}

		# Multi
		if (metrics == FALSE){
			df.out = finalfit_merge(df.out, lmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, lmmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = lmmulti.df[[2]]
		}

	}else if(keep_models == TRUE){
		lmmulti1.out = lmmulti(.data, dependent, explanatory)
		lmmulti1.df = do.call(fit2df,
													c(list(.data=lmmulti1.out, metrics=metrics,
																 estimate_suffix = " (multivariable)"), args))
		if (!is.null(explanatory_multi)){
			lmmulti2.out = lmmulti(.data, dependent, explanatory_multi)
			lmmulti2.df = do.call(fit2df,
														c(list(.data=lmmulti2.out, metrics=metrics,
																	 estimate_suffix = " (multivariable reduced)"), args))
		}
		if (!is.null(random_effect)){
			if (is.null(explanatory_multi)){
				lmmixed.out = lmmixed(.data, dependent, explanatory, random_effect)
			} else {
				lmmixed.out = lmmixed(.data, dependent, explanatory_multi, random_effect)
			}
			lmmixed.df = do.call(fit2df,
													 c(list(.data=lmmixed.out, metrics=metrics,
													 			 estimate_suffix = " (multilevel)"), args))
		}

		if (metrics == FALSE){
			if (is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df, estimate_name = args$estimate_name)
				}else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmulti2.df, estimate_name = args$estimate_name)
				}
			}else if(!is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmixed.df, estimate_name = args$estimate_name)
				} else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmulti2.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmixed.df, estimate_name = args$estimate_name)
				}
			}
		}
		if (metrics == TRUE){
			if (is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(lmmulti1.df[[2]])
				}else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmulti2.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(lmmulti1.df[[2]],
												 lmmulti2.df[[2]])
				}
			}else if(!is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmixed.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(lmmulti1.df[[2]],
												 lmmixed.df[[2]])
				} else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(lmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmulti2.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(lmmixed.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(lmmulti1.df[[2]],
												 lmmulti2.df[[2]],
												 lmmixed.df[[2]])
				}
			}
		}
	}

	# Label interactions
	interaction_row = grep(":", df.out$fit_id)
	df.out$label = as.character(df.out$label)
	df.out$levels = as.character(df.out$levels)
	df.out[,5] = as.character(df.out[,5])
	df.out$label[interaction_row] = 	df.out$fit_id[interaction_row]
	df.out$levels[interaction_row] = "Interaction"
	df.out[interaction_row, 5] = "-"

	# Tidy up
	index_fit_id = which(names(df.out)=="fit_id")
	index_index = which(names(df.out)=="index")
	if(keep_fit_id){
		df.out = df.out
	} else {
		df.out = df.out[,-c(index_fit_id, index_index)]
	}

	# Class
	class(df.out) = c("data.frame.ff", class(df.out))
	
	# Add dependent name label
	if(add_dependent_label){
		df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
														 prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}

	# Add metrics
	if (metrics == TRUE){
		return(list(df.out, df.metrics))
	} else {
		return(df.out)
	}
}



#' Final output tables for common regression models: glm method
#'
#' \code{finalfit.glm} method (not called directly)
#'
#' @rdname finalfit
#' @export
finalfit.glm = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
												column = FALSE, keep_models=FALSE, metrics=FALSE,  add_dependent_label=TRUE,
												dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
												keep_fit_id=FALSE, ...){

	args = list(...)

	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "OR"

	# Logistic regression ----
	# Summary table
	summary.out = summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
																	 column=column, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)

	# Univariable
	glmuni.out = glmuni(.data, dependent, explanatory)
	glmuni.df = do.call(fit2df, c(list(.data=glmuni.out, estimate_suffix = " (univariable)"), args))

	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, glmuni.df, estimate_name = args$estimate_name)

	# This is standard approach, which is why written out in full but duplicated somewhat below
	if(keep_models == FALSE){
		# Multivariable/Mixed
		if (is.null(random_effect)){
			if (is.null(explanatory_multi)){
				glmmulti.out = glmmulti(.data, dependent, explanatory)
			} else {
				glmmulti.out = glmmulti(.data, dependent, explanatory_multi)
			}
			glmmulti.df = do.call(fit2df,
														c(list(.data=glmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)"), args))
		} else if (is.null(random_effect) == FALSE){
			if (is.null(explanatory_multi)){
				glmmulti.out = glmmixed(.data, dependent, explanatory, random_effect)
			} else {
				glmmulti.out = glmmixed(.data, dependent, explanatory_multi, random_effect)
			}
			glmmulti.df = do.call(fit2df,
														c(list(.data=glmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)"), args))
		}

		# Multi
		if (metrics == FALSE){
			df.out = finalfit_merge(df.out, glmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, glmmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = glmmulti.df[[2]]
		}

	}else if(keep_models == TRUE){
		glmmulti1.out = glmmulti(.data, dependent, explanatory)
		glmmulti1.df = do.call(fit2df,
													 c(list(.data=glmmulti1.out, metrics=metrics,
													 			 estimate_suffix = " (multivariable)"), args))
		if (!is.null(explanatory_multi)){
			glmmulti2.out = glmmulti(.data, dependent, explanatory_multi)
			glmmulti2.df = do.call(fit2df,
														 c(list(.data=glmmulti2.out, metrics=metrics,
														 			 estimate_suffix = " (multivariable reduced)"), args))
		}
		if (!is.null(random_effect)){
			if (is.null(explanatory_multi)){
				glmmixed.out = glmmixed(.data, dependent, explanatory, random_effect)
			} else {
				glmmixed.out = glmmixed(.data, dependent, explanatory_multi, random_effect)
			}
			glmmixed.df = do.call(fit2df,
														c(list(.data=glmmixed.out, metrics=metrics,
																	 estimate_suffix = " (multilevel)"), args))
		}

		if (metrics == FALSE){
			if (is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df, estimate_name = args$estimate_name)
				}else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmulti2.df, estimate_name = args$estimate_name)
				}
			}else if(!is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmixed.df, estimate_name = args$estimate_name)
				} else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmulti2.df, estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmixed.df, estimate_name = args$estimate_name)
				}
			}
		}
		if (metrics == TRUE){
			if (is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(glmmulti1.df[[2]])
				}else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmulti2.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(glmmulti1.df[[2]],
												 glmmulti2.df[[2]])
				}
			}else if(!is.null(random_effect)){
				if (is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmixed.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(glmmulti1.df[[2]],
												 glmmixed.df[[2]])
				} else if(!is.null(explanatory_multi)){
					df.out = df.out %>%
						finalfit_merge(glmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmulti2.df[[1]], estimate_name = args$estimate_name) %>%
						finalfit_merge(glmmixed.df[[1]], estimate_name = args$estimate_name)
					df.metrics = c(glmmulti1.df[[2]],
												 glmmulti2.df[[2]],
												 glmmixed.df[[2]])
				}
			}
		}
	}

	# Label interactions
	interaction_row = grep(":", df.out$fit_id)
	df.out$label[interaction_row] = 	df.out$fit_id[interaction_row]
	df.out$levels[interaction_row] = "Interaction"
	df.out[,4] = as.character(df.out[,4])
	df.out[,5] = as.character(df.out[,5])
	df.out[interaction_row, 4] = "-"
	df.out[interaction_row, 5] = "-"

	# Tidy up
	index_fit_id = which(names(df.out)=="fit_id")
	index_index = which(names(df.out)=="index")
	if(keep_fit_id){
		df.out = df.out
	} else {
		df.out = df.out[,-c(index_fit_id, index_index)]
	}

	# Class
	class(df.out) = c("data.frame.ff", class(df.out))
	
	# Add dependent name label
	if(add_dependent_label){
		df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
														 prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}

	# Add metrics
	if (metrics){
		return(list(df.out, df.metrics))
	} else {
		return(df.out)
	}
}





#' Final output tables for common regression models: coxph method
#'
#' \code{finalfit.coxph} method (not called directly)
#'
#' @rdname finalfit
#' @export
finalfit.coxph = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
													column = TRUE, keep_models=FALSE, metrics=FALSE, add_dependent_label=TRUE,
													dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
													keep_fit_id=FALSE, ...){

	args = list(...)

	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "HR"

	# No frailty
	if(!is.null(random_effect)) stop("Random effects / frailty from package::coxme not currently implemented.
																	 Consider adding `cluster(var)` to explanatory list for GEE equivalent.")

	# Cox proprotional hazards model -----------------------------------------------------------
	# Summary table
	summary.out = suppressMessages(
		suppressWarnings(
			summary_factorlist(.data, dependent, explanatory, column = column, fit_id=TRUE)
		))
	
	# Previous removed, but now include.
	# summary.out = summary.out[,-3] # Remove 'all' column with total counts

	# Univariable
	coxphuni_out = coxphuni(.data, dependent, explanatory)
	coxphuni_df =	do.call(fit2df, c(list(.data=coxphuni_out, estimate_suffix = " (univariable)"), args))

	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, coxphuni_df, estimate_name = args$estimate_name)

	# Multivariable
	if(keep_models == FALSE){
		if (is.null(explanatory_multi)){
			coxphmulti.out = coxphmulti(.data, dependent, explanatory)
		} else {
			coxphmulti.out = coxphmulti(.data, dependent, explanatory_multi)
		}
		coxphmulti.df = do.call(fit2df,
														c(list(.data=coxphmulti.out, estimate_suffix = " (multivariable)"),
															metrics=metrics, args))


		if (metrics == FALSE){
			df.out = finalfit_merge(df.out, coxphmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, coxphmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = coxphmulti.df[[2]]
		}


	}else if(keep_models == TRUE){
		coxphmulti1.out = coxphmulti(.data, dependent, explanatory)
		coxphmulti1.df = do.call(fit2df,
														 c(list(.data=coxphmulti1.out, metrics=metrics,
														 			 estimate_suffix = " (multivariable)"), args))
		if (!is.null(explanatory_multi)){
			coxphmulti2.out = coxphmulti(.data, dependent, explanatory_multi)
			coxphmulti2.df = do.call(fit2df,
															 c(list(.data=coxphmulti2.out, metrics=metrics,
															 			 estimate_suffix = " (multivariable reduced)"), args))
		}

		if (metrics == FALSE){
			if (is.null(explanatory_multi)){
				df.out = df.out %>%
					finalfit_merge(coxphmulti1.df, estimate_name = args$estimate_name)
			}else if(!is.null(explanatory_multi)){
				df.out = df.out %>%
					finalfit_merge(coxphmulti1.df, estimate_name = args$estimate_name) %>%
					finalfit_merge(coxphmulti2.df, estimate_name = args$estimate_name)
			}
		}

		if (metrics == TRUE){
			if (is.null(explanatory_multi)){
				df.out = df.out %>%
					finalfit_merge(coxphmulti1.df[[1]], estimate_name = args$estimate_name)
				df.metrics = c(coxphmulti1.df[[2]])
			}else if(!is.null(explanatory_multi)){
				df.out = df.out %>%
					finalfit_merge(coxphmulti1.df[[1]], estimate_name = args$estimate_name) %>%
					finalfit_merge(coxphmulti2.df[[1]], estimate_name = args$estimate_name)
				df.metrics = c(coxphmulti1.df[[2]],
											 coxphmulti2.df[[2]])
			}
		}
	}

	# # Multi
	## Add frailty later
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
	if(keep_fit_id){
		df.out = df.out
	} else {
		df.out = df.out[,-c(index_fit_id, index_index)]
	}
	
	# Class
	class(df.out) = c("data.frame.ff", class(df.out))

	# Add dependent name label
	if(add_dependent_label){
		df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
														 prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}

	# Add metrics
	if (metrics == TRUE){
		return(list(df.out, df.metrics))
	} else {
		return(df.out)
	}
}
