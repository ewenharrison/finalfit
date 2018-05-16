#' Extract \code{glmuni} and \code{glmmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' Takes output from \code{finalfit} model wrappers and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.glmlist} is the model extract method for \code{glmuni} and \code{glmmulti}.
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
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a list of two dataframes,
#'   one is model parameters, one is model metrics. length two
#'
#' @family \code{finalfit} model extractors
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable)")

fit2df.glmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                           explanatory_name = "explanatory",
                           estimate_name = "OR",
                           estimate_suffix = "",
                           p_name = "p",
                           digits=c(2,2,3), confint_sep = "-", ...){

  if (metrics==TRUE && length(.data)>1){
    stop("Metrics only generated for single models: multiple models supplied to function")
  }

  df.out = plyr::ldply(.data, .id = NULL, extract_fit, explanatory_name=explanatory_name,
                       estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                       p_name=p_name, digits=digits)

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
    x = .data[[1]]
    n_data = dim(x$data)[1]
    n_model = dim(x$model)[1]
    aic = round(x$aic, 1)
    auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
    metrics.out = paste0(
      "Number in dataframe = ", n_data,
      ", Number in model = ", n_model,
      ", Missing = ", n_data-n_model,
      ", AIC = ", aic,
      ", C-statistic = ", auc)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
}
