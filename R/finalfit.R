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
#' @param formula an object of class "formula" (or one that can be coerced to 
#'   that class). Optional instead of standard dependent/explanatory format. 
#'   Do not include if using dependent/explanatory. 
#' @param model_args List. A list of arguments to pass to 
#' \code{\link{lm}}, \code{\link{glm}}, \code{\link{coxph}}.  
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

finalfit = function(.data, dependent = NULL, explanatory = NULL, explanatory_multi=NULL, random_effect=NULL,
										formula = NULL, model_args = list(), 
										column=NULL, keep_models=FALSE, metrics=FALSE, add_dependent_label=TRUE,
										dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
										keep_fit_id = FALSE, ...){
	
	# Formula interface -----------------
	## Added at request
	if(!is.null(formula) & (!is.null(dependent) | !is.null(explanatory))) stop("Formula OR dependent/explanatory terms, not both")
	if(!is.null(formula)){
		.terms = ff_parse_formula(formula)
		dependent = .terms$dependent
		explanatory = .terms$explanatory
		if("random_effect" %in% names(.terms)) random_effect = .terms$random_effect
	}
	
	# Checks
	if(!is.data.frame(.data)) stop(".data is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")
	if(.data[ ,names(.data) %in% explanatory] %>% error_colon_fct_levels()){
		stop("Colons (:) not allowed in factor-level names. Check with ff_glimpse(.data). Recode: forcats::fct_recode()")
	}
	if(keep_models & (is.null(random_effect) & is.null(explanatory_multi))){
		warning("If keep_models = TRUE, explanatory_multi or random_effect must not be null.")
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
							random_effect=random_effect, model_args = model_args, column = column,
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
											 model_args, column = FALSE, keep_models=FALSE, metrics=FALSE, add_dependent_label = TRUE,
											 dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
											 keep_fit_id=FALSE, ...){
	
	args = list(...)
	model_args_in = model_args
	
	# Define arguments
	model_args = c(list(.data=.data, dependent=dependent), model_args_in)
	if(!is.null(random_effect)) model_args = c(model_args, list(random_effect=random_effect)) 
	if(!is.null(explanatory_multi)){
		model_args = c(model_args, list(explanatory=explanatory_multi))
	} else {
		model_args = c(model_args, list(explanatory=explanatory))
	}
	
	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "OR"
	
	# Logistic regression ----
	# Summary table
	summary.out = summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
																	 column=column, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)
	
	# Univariable
	lmuni.out = do.call(lmuni, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
	lmuni.df = do.call(fit2df, c(list(.data=lmuni.out, estimate_suffix = " (univariable)"), args))
	
	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, lmuni.df, estimate_name = args$estimate_name)
	
	# Multivariable/Mixed
	if(is.null(random_effect)){
		lmmulti.out = do.call(lmmulti, model_args)
		lmmulti.df = do.call(fit2df,
												 c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)"), args))
	} else if(!is.null(random_effect)){
		lmmulti.out = do.call(lmmixed, model_args)
		lmmulti.df = do.call(fit2df,
												 c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)"), args))
	}
	
	# Single multivariable model table
	if(!keep_models){
		if(!metrics){
			df.out = finalfit_merge(df.out, lmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, lmmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = lmmulti.df[[2]]
		}
	} else if(keep_models){
		
		# Keep intermediate models
		lmmulti1.out = do.call(lmmulti, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
		lmmulti1.df = do.call(fit2df,
													c(list(.data=lmmulti1.out, metrics=metrics, estimate_suffix = " (multivariable full)"), args))
		if(!is.null(random_effect) & !is.null(explanatory_multi)){
			lmmulti2.out = do.call(
				lmmixed, c(list(.data=.data, dependent=dependent, explanatory=explanatory, random_effect=random_effect), model_args_in))
			lmmulti2.df = do.call(fit2df,
														c(list(.data=lmmulti2.out, metrics=metrics, estimate_suffix = " (multilevel full)"), args))
		}
		
		if(!metrics){
			df.out = df.out %>%
				finalfit_merge(lmmulti1.df, estimate_name = args$estimate_name) %>%
				{ if(!is.null(random_effect) & !is.null(explanatory_multi)){
					finalfit_merge(., lmmulti2.df, estimate_name = args$estimate_name)
				} else {
					.
				}
				} %>%
				finalfit_merge(lmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = df.out %>%
				finalfit_merge(lmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
				{ if(!is.null(random_effect) & !is.null(explanatory_multi)){
					finalfit_merge(., lmmulti2.df[[1]], estimate_name = args$estimate_name)
				} else {
					.
				}
				} %>% 
				finalfit_merge(lmmulti.df[[1]], estimate_name = args$estimate_name)
			
			df.metrics = c(lmmulti1.df[[2]],
										 lmmulti.df[[2]])
			
			if(!is.null(random_effect) & !is.null(explanatory_multi)){
				df.metrics = c(lmmulti1.df[[2]],
											 lmmulti2.df[[2]],
											 lmmulti.df[[2]])
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
	if (metrics){
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
												model_args, column = FALSE, keep_models=FALSE, metrics=FALSE,  add_dependent_label=TRUE,
												dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
												keep_fit_id=FALSE, ...){
	
	args = list(...)
	model_args_in = model_args
	
	# Define arguments
	model_args = c(list(.data=.data, dependent=dependent), model_args_in)
	if(!is.null(random_effect)) model_args = c(model_args, list(random_effect=random_effect)) 
	if(!is.null(explanatory_multi)){
		model_args = c(model_args, list(explanatory=explanatory_multi))
	} else {
		model_args = c(model_args, list(explanatory=explanatory))
	}
	
	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "OR"
	
	# Logistic regression ----
	# Summary table
	summary.out = summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
																	 column=column, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)
	
	# Univariable
	glmuni.out = do.call(glmuni, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
	glmuni.df = do.call(fit2df, c(list(.data=glmuni.out, estimate_suffix = " (univariable)"), args))
	
	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, glmuni.df, estimate_name = args$estimate_name)
	
	# Multivariable/Mixed
	if(is.null(random_effect)){
		glmmulti.out = do.call(glmmulti, model_args)
		glmmulti.df = do.call(fit2df,
													c(list(.data=glmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)"), args))
	} else if(!is.null(random_effect)){
		glmmulti.out = do.call(glmmixed, model_args)
		glmmulti.df = do.call(fit2df,
													c(list(.data=glmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)"), args))
	}
	
	# Single multivariable model table
	if(!keep_models){
		if(!metrics){
			df.out = finalfit_merge(df.out, glmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, glmmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = glmmulti.df[[2]]
		}
	} else if(keep_models){
		
		# Keep intermediate models
		glmmulti1.out = do.call(glmmulti, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
		glmmulti1.df = do.call(fit2df,
													 c(list(.data=glmmulti1.out, metrics=metrics, estimate_suffix = " (multivariable full)"), args))
		if(!is.null(random_effect) & !is.null(explanatory_multi)){
			glmmulti2.out = do.call(
				glmmixed, c(list(.data=.data, dependent=dependent, explanatory=explanatory, random_effect=random_effect), model_args_in))
			glmmulti2.df = do.call(fit2df,
														 c(list(.data=glmmulti2.out, metrics=metrics, estimate_suffix = " (multilevel full)"), args))
		}
		
		if(!metrics){
			df.out = df.out %>%
				finalfit_merge(glmmulti1.df, estimate_name = args$estimate_name) %>%
				{ if(!is.null(random_effect) & !is.null(explanatory_multi)){
					finalfit_merge(., glmmulti2.df, estimate_name = args$estimate_name)
				} else {
					.
				}
				} %>%
				finalfit_merge(glmmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = df.out %>%
				finalfit_merge(glmmulti1.df[[1]], estimate_name = args$estimate_name) %>%
				{ if(!is.null(random_effect) & !is.null(explanatory_multi)){
					finalfit_merge(., glmmulti2.df[[1]], estimate_name = args$estimate_name)
				} else {
					.
				}
				} %>% 
				finalfit_merge(glmmulti.df[[1]], estimate_name = args$estimate_name)
			
			df.metrics = c(glmmulti1.df[[2]],
										 glmmulti.df[[2]])
			
			if(!is.null(random_effect) & !is.null(explanatory_multi)){
				df.metrics = c(glmmulti1.df[[2]],
											 glmmulti2.df[[2]],
											 glmmulti.df[[2]])
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
													model_args, column = TRUE, keep_models=FALSE, metrics=FALSE, add_dependent_label=TRUE,
													dependent_label_prefix="Dependent: ", dependent_label_suffix="", 
													keep_fit_id=FALSE, ...){
	
	args = list(...)
	model_args_in = model_args
	
	# Define arguments
	model_args = c(list(.data=.data, dependent=dependent), model_args_in)
	if(!is.null(random_effect)) model_args = c(model_args, list(random_effect=random_effect)) 
	if(!is.null(explanatory_multi)){
		model_args = c(model_args, list(explanatory=explanatory_multi))
	} else {
		model_args = c(model_args, list(explanatory=explanatory))
	}
	
	# Defaults which can be modified via ...
	if (is.null(args$estimate_name)) args$estimate_name = "HR"
	
	# No frailty
	if(!is.null(random_effect)) stop("Random effects / frailty from package::coxme not currently implemented.
																	 Consider adding `cluster(var)` to explanatory list for GEE equivalent.")
	
	# Cox proportional hazards model -----------------------------------------------------------
	# Summary table
	summary.out = suppressMessages(
		suppressWarnings(
			summary_factorlist(.data, dependent, explanatory, column = column, fit_id=TRUE)
		))
	
	# Univariable
	coxphuni_out = do.call(coxphuni, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
	coxphuni_df =	do.call(fit2df, c(list(.data=coxphuni_out, estimate_suffix = " (univariable)"), args))
	
	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, coxphuni_df, estimate_name = args$estimate_name)
	
	# Multivariable
	coxphmulti.out = do.call(coxphmulti, model_args)
	coxphmulti.df = do.call(fit2df,
													c(list(.data=coxphmulti.out, estimate_suffix = " (multivariable)"),
														metrics=metrics, args))
	if(!keep_models){
		if (!metrics){
			df.out = finalfit_merge(df.out, coxphmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = finalfit_merge(df.out, coxphmulti.df[[1]], estimate_name = args$estimate_name)
			df.metrics = coxphmulti.df[[2]]
		}
		
		
	} else if (keep_models){
		coxphmulti1.out = do.call(coxphmulti, c(list(.data=.data, dependent=dependent, explanatory=explanatory), model_args_in))
		coxphmulti1.df = do.call(fit2df,
														 c(list(.data=coxphmulti1.out, metrics=metrics,
														 			 estimate_suffix = " (multivariable full)"), args))
		if(!metrics){
			df.out = df.out %>%
				finalfit_merge(coxphmulti1.df, estimate_name = args$estimate_name) %>%
				finalfit_merge(coxphmulti.df, estimate_name = args$estimate_name)
		} else {
			df.out = df.out %>%
				finalfit_merge(coxphmulti1.df[[1]], estimate_name = args$estimate_name) %>%
				finalfit_merge(coxphmulti.df[[1]], estimate_name = args$estimate_name)
			
			df.metrics = c(coxphmulti1.df[[2]],
										 coxphmulti.df[[2]])
		}
	}
	
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
	if (metrics){
		return(list(df.out, df.metrics))
	} else {
		return(df.out)
	}
}
