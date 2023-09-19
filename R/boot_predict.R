#' Bootstrap simulation for model prediction
#'
#' Generate model predictions against a specified set of explanatory levels with
#' bootstrapped confidence intervals. Add a comparison by difference or ratio of
#' the first row of \code{newdata} with all subsequent rows.
#'
#' To use this, first generate \code{newdata} for specified levels of
#' explanatory variables using \code{\link{finalfit_newdata}}. Pass model
#' objects from \code{lm}, \code{glm}, \code{\link{lmmulti}}, and
#' \code{\link{glmmulti}}. The comparison metrics are made on individual
#' bootstrap samples distribution returned as a mean with confidence intervals.
#' A p-value is generated on the proportion of values on the other side of the
#' null from the mean, e.g. for a ratio greater than 1.0, p is the number of
#' bootstrapped predictions under 1.0, multiplied by two so is two-sided.
#'
#' @param fit A model generated using \code{lm}, \code{glm},
#'   \code{\link{lmmulti}}, and \code{\link{glmmulti}}.
#' @param newdata Dataframe usually generated with
#'   \code{\link{finalfit_newdata}}.
#' @param type the type of prediction required, see
#'   \code{\link[stats]{predict.glm}}. The default for glm models is on the
#'   scale of the response variable. Thus for a binomial model the default
#'   predictions are predicted probabilities.
#' @param R Number of simulations. Note default R=100 is very low.
#' @param estimate_name Name to be given to prediction variable y-hat.
#' @param confint_level The confidence level to use for the confidence interval. 
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, 
#'   which corresponds to a 95 percent confidence interval
#' @param conf.method Passed to the type argument of boot::boot.ci(). 
#'   Defaults to "perc". The allowed types are "perc", "basic", "bca", and "norm". 
#'   Does not support "stud" or "all"
#' @param confint_sep String separating lower and upper confidence interval
#' @param condense Logical. FALSE gives numeric values, usually for plotting.
#'   TRUE gives table for final output.
#' @param boot_compare Include a comparison with the first row of \code{newdata}
#'   with all subsequent rows. See \code{\link{boot_compare}}.
#' @param compare_name Name to be given to comparison metric.
#' @param comparison Either "difference" or "ratio".
#' @param ref_symbol Reference level symbol
#' @param digits Rounding for estimate values and p-values, default c(2,3).
#' @return A dataframe of predicted values and confidence intervals, with the
#'   option of including a comparison of difference between first row and all
#'   subsequent rows of \code{newdata}.
#'
#' @seealso \link{finalfit_newdata}
#'
#' /code{finalfit} predict functions
#' @export
#' @importFrom broom tidy
#' @importFrom boot boot
#' @importFrom stats predict
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' # Predict probability of death across combinations of factor levels
#' explanatory = c("age.factor", "extent.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#'
#' # Generate combination of factor levels
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, newdata = list(
#'     c("<40 years",  "Submucosa", "No"),
#'     c("<40 years", "Submucosa", "Yes"),
#'     c("<40 years", "Adjacent structures", "No"),
#'     c("<40 years", "Adjacent structures", "Yes")
#'    )) -> newdata
#'
#' # Run simulation
#' colon_s %>%
#'   glmmulti(dependent, explanatory) %>%
#'   boot_predict(newdata, estimate_name = "Predicted probability of death",
#'     compare_name = "Absolute risk difference", R=100, digits = c(2,3))
#'
#' # Plotting
#' explanatory = c("nodes", "extent.factor", "perfor.factor")
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, rowwise = FALSE, newdata = list(
#'   rep(seq(0, 30), 4),
#'   c(rep("Muscle", 62), rep("Adjacent structures", 62)),
#'   c(rep("No", 31), rep("Yes", 31), rep("No", 31), rep("Yes", 31))
#' )) -> newdata
#'
#' colon_s %>%
#'   glmmulti(dependent, explanatory) %>%
#'   boot_predict(newdata, boot_compare = FALSE, R=100, condense=FALSE) -> plot
#'
#'   library(ggplot2)
#'   theme_set(theme_bw())
#'   plot %>%
#'     ggplot(aes(x = nodes, y = estimate, ymin = estimate_conf.low,
#'         ymax = estimate_conf.high, fill=extent.factor))+
#'       geom_line(aes(colour = extent.factor))+
#'       geom_ribbon(alpha=0.1)+
#'       facet_grid(.~perfor.factor)+
#'       xlab("Number of postive lymph nodes")+
#'       ylab("Probability of death")+
#'       labs(fill = "Extent of tumour", colour = "Extent of tumour")+
#'       ggtitle("Probability of death by lymph node count")

boot_predict = function (fit, newdata, type = "response", R = 100,
                         estimate_name = NULL, 
												 confint_level = 0.95, conf.method = "perc", 
                         confint_sep = " to ", condense=TRUE, boot_compare = TRUE,
                         compare_name = NULL, comparison = "difference", ref_symbol = "-",
                         digits = c(2, 3)){
  fit_class = attr(fit, "class")[1]

  # Ensure lmlist | glmlist objects are length == 1
  if(fit_class %in% c("lmlist", "glmlist") & length(fit) > 1){
    stop("Multiple models in fit, must be single model")}

  # Unlist lmlist & glmlist objects
  if(fit_class %in% c("lmlist", "glmlist")) fit = fit[[1]]
  fit_class = attr(fit, "class")[1]

  # Stop if not lm | glm
  if(!any(fit_class %in% c("lm", "glm"))) stop("fit must contain an lm or glm model")

  # Stop if newdata not dataframe
  if(!is.data.frame(newdata)) stop("Must provide dataframe with new data, see examples")

  if(is.null(estimate_name)) estimate_name = "estimate"

  formula = fit$terms
  family = fit$family$family
  link = fit$family$link
  if(is.null(family)) {
    family = substitute(gaussian)
  }else{
    family = match.fun(family)
    family = substitute(family(link=link))
  }
  .data = fit$model

  # Statistic function to bootstrap
  statistic = function(formula, family, .data, indices) {
    d = .data[indices, ]
    fit_boot = glm(formula=formula, family = eval(family), data = d)
    out = predict(fit_boot, newdata=newdata, type=type)
    return(out)
  }

  # Run bootstrap
  bs.out = boot::boot(data = .data, statistic = statistic, R = R,
                        formula = formula, family = family)

  bs.tidy = broom::tidy(bs.out, conf.int = TRUE, conf.level = conf.level, conf.method = conf.method)
  bs.tidy = data.frame(bs.tidy)


  if(condense == FALSE){
    df.out = bs.tidy[, c(1, 4,5)]
    colnames(df.out) = c(estimate_name, paste0(estimate_name, "_conf.low"), paste0(estimate_name, "_conf.high"))
  } else {
    bs.tidy %>%
      dplyr::mutate_all(round_tidy, digits = digits[1]) -> df.out
    df.out = data.frame(
      paste0(df.out$statistic, " (", df.out$conf.low, confint_sep, df.out$conf.high, ")"))
    colnames(df.out) = estimate_name
  }


  if(boot_compare){
    bc.out = boot_compare(bs.out, confint_sep = confint_sep, comparison = comparison,
                          condense = condense,
                          compare_name = compare_name, digits=digits,
                          ref_symbol = ref_symbol)
    df.out = cbind(df.out, bc.out)
  }


  # Final table
  if(condense == TRUE){
    labels = extract_variable_label(newdata)
    names(newdata) = labels
  }
  df.out = cbind(newdata, df.out)
  return(df.out)
}
