# Functions used in fit2df() ----

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit = function(.data, explanatory_name, estimate_name,
                       estimate_suffix,  p_name, digits, ...){
  UseMethod("extract_fit")
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.glm = function(.data, explanatory_name="explanatory", estimate_name="OR",
                           estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(coef(x))
  estimate = exp(coef(x))
  confint = exp(confint(x))
  p = summary(x)$coef[,"Pr(>|z|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate.
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.glmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
                                estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(lme4::fixef(x))
  estimate = exp(lme4::fixef(x))
  confint = exp(lme4::confint.merMod(x, method='Wald'))
  confint = confint[-grep("sig", rownames(confint)),]
  p = summary(x)$coef[,"Pr(>|z|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)

}


#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate.
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.lm = function(.data, explanatory_name="explanatory", estimate_name="Coefficient",
                          estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(coef(x))
  estimate = coef(x)
  confint = confint(x)
  p = summary(x)$coef[,"Pr(>|t|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate.
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.lmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
                               estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(lme4::fixef(x))
  estimate = exp(lme4::fixef(x))
  confint = exp(lme4::confint.merMod(x, method='Wald'))
  confint = confint[-grep("sig", rownames(confint)),]
  p = 1-pnorm(abs(summary(x)$coefficients[,3]))
  warning("P-value for lmer is estimate assuming t-distribution is normal. Bootstrap for final publication.")

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate.
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.coxph = function(.data, explanatory_name="explanatory", estimate_name="HR",
                             estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  results = summary(x)$conf.int
  explanatory = row.names(results)
  estimate = results[,1]
  confint_L = results[,3]
  confint_U = results[,4]
  p = summary(x)$coefficients[explanatory,
                              max(dim(summary(x)$coefficients)[2])] # Hack to get p fe and re
  df.out = data.frame(explanatory, estimate, confint_L, confint_U, p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}


#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param p_name Name given to p-value estimate.
#' @param X Design matrix from Stan modelling procedure.
#' @param ... Other arguments.
#'
#' @keywords internal

extract_fit.stanfit = function(.data, explanatory_name="explanatory", estimate_name="OR",
                               estimate_suffix = "",  p_name = "p", digits=c(2,2,3), X, ...){
  stanfit = .data
  pars = "beta"
  quantiles =  c(0.025, 0.50, 0.975)

  explanatory = attr(X, "dimnames")[[2]]
  results = rstan::summary(stanfit,
                           pars = pars,
                           probs = quantiles)$summary
  estimate = exp(results[, 1])
  confint_L = exp(results[, 4])
  confint_U = exp(results[, 6])

  # Determine a p-value based on two-sided examination of chains
  chains = rstan::extract(stanfit, pars=pars, permuted = TRUE, inc_warmup = FALSE,
                          include = TRUE)
  p1.out = apply(chains[[1]], 2, function(x)mean(x<0))
  p2.out = apply(chains[[1]], 2, function(x)mean(x>0))
  p1.out = p1.out*2
  p2.out = p2.out*2
  p.out = ifelse(p1.out < 1, p1.out, p2.out)
  p = round(p.out, 3)

  df.out = data.frame(explanatory, estimate, confint_L, confint_U, p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Condense model output dataframe for final tables
#'
#' Internal function, not called directly. Can only be used in conjunction with
#'   extract_fit
#'
#' @param .data Dataframe of five columns, must be this order, (1) explanatory
#'   variable names, (2) estimate, (3) confidence interval lower limit, (4)
#'   confidence interval upper limit, (5) p-value.
#' @param explanatory_name Name for this column in output
#' @param estimate_name Name for this column in output
#' @param estimate_suffix Appeneded to estimate name
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param confint_sep String to separate confidence intervals, typically "-" or
#'   " to ".
#'
#' @keywords internal

condense_fit = function(.data, explanatory_name="explanatory", estimate_name="OR",
                        estimate_suffix = "", p_name = "p",
                        digits=c(2,2,3), confint_sep = "-"){
  x = .data
  d.estimate = digits[1]
  d.confint = digits[2]
  d.p = digits[3]

  explanatory = x[,1]
  estimate = round_tidy(x[,2], d.estimate)
  confint_low = round_tidy(x[,3], d.confint)
  confint_high = round_tidy(x[,4], d.confint)
  p = p_tidy(x[,5], d.p)

  df.out = data.frame(
    explanatory,
    paste0(
      estimate, " (",
      confint_low, confint_sep,
      confint_high, ", ",
      p_name, p, ")"))

  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix)
  )
  return(df.out)
}

#' Round values but keep trailing zeros
#'
#' Internal function, not called directly
#'
#' e.g. for 3 decimal places I want 1.200, not 1.2.
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @return Vector of strings.
#'
#' @keywords internal

round_tidy = function(x, digits){
  sprintf.arg = paste0("%.", digits, "f")
  x.out = do.call(sprintf, list(sprintf.arg, x)) # keep trailing zeros
  return(x.out)
}

#' Round p-values but keep trailing zeros
#'
#' Internal function, not called directly
#'
#' e.g. for 3 decimal places I want 0.100, not 0.1. Note this function with
#' convert 0.000 to <0.001. All other values are prefixed with "=" by default
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @param prefix Appended in front of values for use with \code{condense_fit}.
#' @return Vector of strings.
#'
#' @keywords internal

p_tidy = function(x, digits, prefix="="){
  x.out = paste0(prefix, round_tidy(x, digits))
  all_zeros = paste0(prefix, round_tidy(0, digits))
  less_than = paste0("<", format(10^-digits, scientific=FALSE))
  x.out[x.out == all_zeros] = less_than
  return(x.out)
}

#' Remove intercept from model output
#'
#' Internal function, not called directly
#'
#' @param .data Numeric vector of values to round
#' @param intercept_name Name given to interept in model. Should never have to
#'   change from default.
#' @return Vector of strings.
#'
#' @keywords internal
#'

# Tried to do this with dplyr programming and failed miserably.
# quo() enquo() !! all a bit of a nightmare
# So let's square bracket away!
remove_intercept = function(.data, intercept_name = "(Intercept)"){
  .data = .data[-which(.data[,1] == intercept_name),]
  return(.data)
}

#' Remove duplicate levels within \code{\link{summary_factorlist}}: \code{finalfit} helper function
#'
#' Not called directly.
#'
#' @param factorlist A factorlist intermediary.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when \code{na_include=TRUE}.
#' @return Returns a \code{factorlist} dataframe.
#'
#' @keywords internal

rm_duplicate_labels = function(factorlist, na_to_missing = TRUE){
  x = factorlist
  duplicate_rows = duplicated(x$label)
  x$label = as.character(x$label)
  x$label[duplicate_rows] = ""
  if (any(names(x) %in% "pvalue")){
    x$pvalue[duplicate_rows] = ""
    x$pvalue[x$pvalue == "0.000"] = "<0.001"
  }
  if (na_to_missing == TRUE){
    x$levels = as.character(x$levels)
    x$levels[which(x$levels == "NA")] = "Missing"
  }
  return(x)
}



#' Make a label for the dependent variable
#'
#' Not usually called directly. Can be used to label final results dataframe.
#'
#' @param df.out Dataframe (results table) to be altered.
#' @param .data Original dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @return Returns the label for the dependent variable, if specified.
#' @examples
#' library(dplyr)
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
#' 	 glmmulti(dependent, explanatory) %>%
#' 	 fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#'   finalfit_merge(example.univariable) %>%
#'   finalfit_merge(example.multivariable) %>%
#' 	 finalfit_merge(example.multilevel) %>%
#' 	 select(-c(fit_id, index)) %>%
#' 	 dependent_label(colon_s, dependent) -> example.final
#'   example.final
dependent_label = function(df.out, .data, dependent, prefix = "Dependent: ", suffix=""){
  d_label = attr(.data[,which(names(.data) %in% dependent)], "label")

  if (is.null(d_label)){
    d_label = dependent
  } else {
    d_label = d_label
  }
  names(df.out)[1] = paste0(prefix, d_label, suffix)
  names(df.out)[2] = ""

  return(df.out)
}

#' Label plot title
#'
#' Not called directly.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @keywords internal
plot_title = function(.data, dependent, dependent_label, prefix = "", suffix=""){
  if (is.null(dependent_label)){
    d_label = attr(.data[,which(names(.data) %in% dependent)], "label")
    if (is.null(d_label)){
      d_label = dependent
    } else {
      d_label = d_label
    }
  } else {
    d_label = dependent_label
  }
  out = paste0(prefix, d_label, suffix)
  return(out)
}
