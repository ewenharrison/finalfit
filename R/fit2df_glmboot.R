#' Extract \code{glmboot} model fit results to dataframe: \code{finalfit} model extracters
#'
#' Takes output from \code{\link[stats]{glm}} with \code{boot::\link[boot]{boot}}
#' confidence intervals on fixed effect coefficients and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.glmboot} is the model extract method for \code{\link{glmmulti_boot}} models.
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
#' @return A dataframe of model parameters. When \code{metrics=TRUE} output is a
#'   list of two dataframes, one is model parameters, one is model metrics.
#'
#' @family \code{finalfit} model extracters
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti_boot(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable (BS CIs))")
#'

fit2df.glmboot = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                          explanatory_name = "explanatory",
                          estimate_name = "OR",
                          estimate_suffix = "",
                          p_name = "p",
                          digits=c(2,2,3), confint_sep = "-", ...){
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
