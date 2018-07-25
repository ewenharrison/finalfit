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
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table
#' @param dependent_label_prefix Add text before dependent label
#' @param dependent_label_suffix Add text after dependent label
#' @param ... Other arguments to pass to \code{\link{fit2df}}: estimate_name,
#'   p_name, digits, confint_sep.

#' @return Returns a dataframe with the final model table.
#'
#' @family \code{finalfit} all-in-one functions
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
#' 	select(-c(fit_id, index)) %>%
#' 	dependent_label(colon_s, dependent) -> example.final
#' example.final
#'

finalfit = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
                    metrics=FALSE, add_dependent_label=TRUE,
                    dependent_label_prefix="Dependent: ", dependent_label_suffix="", ...){
  if(is.data.frame(.data)==FALSE) stop(".data is not dataframe")
  if(is.null(explanatory)) stop("No explanatory variable(s) provided")
  if(is.null(dependent)) stop("No dependent variable provided")

  args = list(.data=.data, dependent=dependent, explanatory=explanatory, explanatory_multi=explanatory_multi,
              random_effect=random_effect, metrics=metrics,
              add_dependent_label = add_dependent_label,
              dependent_label_prefix=dependent_label_prefix,
              dependent_label_suffix=dependent_label_suffix, ...=...)

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


#' Final output tables for common regression models: lm method
#'
#' \code{finalfit.lm} method (not called directly)
#'
#' @rdname finalfit
#' @export
finalfit.lm = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL,
                       metrics=FALSE, add_dependent_label = TRUE,
                       dependent_label_prefix="Dependent: ", dependent_label_suffix="", ...){

  args = list(...)

  # Defaults which can be modified via ...
  if (is.null(args$estimate_name)) args$estimate_name = "Coefficient"
  if (is.null(args$p_name)) args$p_name="p"
  if (is.null(args$digits)) 	args$digits=c(2,2,3)
  if (is.null(args$confint_sep)) args$confint_sep = " to "

  args = list(estimate_name = args$estimate_name,
              p_name = args$p_name,
              digits = args$digits,
              confint_sep = args$confint_sep)

  # Linear regression model ------------------------------------------
  # Summary table
  summary.out = suppressWarnings(
    summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
                       column=TRUE, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)
  )

  # Univariable
  lmuni.out = lmuni(.data, dependent, explanatory)
  lmuni.df = do.call(fit2df, c(list(.data=lmuni.out, estimate_suffix = " (univariable)"), args))

  # Multivariable/Mixed
  if (is.null(random_effect)){
    if (is.null(explanatory_multi)){
      lmmulti.out = lmmulti(.data, dependent, explanatory)
    } else {
      lmmulti.out = lmmulti(.data, dependent, explanatory_multi)
    }
    lmmulti.df = do.call(fit2df,
                         c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)"), args))
  } else if (!is.null(random_effect)){
    if (is.null(explanatory_multi)){
      lmmulti.out = lmmixed(.data, dependent, explanatory, random_effect)
    } else {
      lmmulti.out = lmmixed(.data, dependent, explanatory_multi, random_effect)
    }
    lmmulti.df = do.call(fit2df,
                         c(list(.data=lmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)"), args))
  }

  # Merge dataframes
  # Uni
  df.out = finalfit_merge(summary.out, lmuni.df, estimate_name = args$estimate_name)

  # Multi
  if (metrics == FALSE){
    df.out = finalfit_merge(df.out, lmmulti.df, estimate_name = args$estimate_name)
  } else {
    df.out = finalfit_merge(df.out, lmmulti.df[[1]])
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
  df.out = df.out[,-c(index_fit_id, index_index)]

  # Add dependent name label
  if(add_dependent_label){
    df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
                             prefix=dependent_label_prefix, suffix = dependent_label_suffix)
  }

  # Add metrics
  if (metrics == TRUE){
    return(list(df.out, lmmulti.df[[2]]))
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
metrics=FALSE,  add_dependent_label=TRUE,
dependent_label_prefix="Dependent: ", dependent_label_suffix="", ...){

  args = list(...)

  # Defaults which can be modified via ...
  if (is.null(args$estimate_name)) args$estimate_name = "OR"
  if (is.null(args$p_name)) args$p_name="p"
  if (is.null(args$digits)) 	args$digits=c(2,2,3)
  if (is.null(args$confint_sep)) args$confint_sep = "-"

  args = list(estimate_name = args$estimate_name,
              p_name = args$p_name,
              digits = args$digits,
              confint_sep = args$confint_sep)

  # Logistic regression ----
  # Summary table
  summary.out = summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
                                   column=TRUE, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)

  # Univariable
  glmuni.out = glmuni(.data, dependent, explanatory)
  glmuni.df = do.call(fit2df, c(list(.data=glmuni.out, estimate_suffix = " (univariable)"), args))

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

  # Merge dataframes
  # Uni
  df.out = finalfit_merge(summary.out, glmuni.df, estimate_name = args$estimate_name)

  # Multi
  if (metrics == FALSE){
    df.out = finalfit_merge(df.out, glmmulti.df, estimate_name = args$estimate_name)
  } else {
    df.out = finalfit_merge(df.out, glmmulti.df[[1]], estimate_name = args$estimate_name)
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
  df.out = df.out[,-c(index_fit_id, index_index)]

  # Add dependent name label
  if(add_dependent_label){
    df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
                             prefix=dependent_label_prefix, suffix = dependent_label_suffix)
  }

  # Add metrics
  if (metrics){
    return(list(df.out, glmmulti.df[[2]]))
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
                          metrics=FALSE, add_dependent_label=TRUE,
                          dependent_label_prefix="Dependent: ", dependent_label_suffix="", ...){

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
                          c(list(.data=coxphmulti_out, estimate_suffix = " (multivariable)"),
                            metrics=metrics, args))
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
    df.out = dependent_label(df.out=df.out, .data=.data, dependent=dependent,
                             prefix=dependent_label_prefix, suffix = dependent_label_suffix)
  }

  return(df.out)
  # Add metrics
  # if (metrics == TRUE){
  # 	return(list(df.out, glmmulti_df[[2]]))
  # } else {
  # 	return(df.out)
  # }
}
