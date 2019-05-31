#' Binomial logistic regression multivariable models with bootstrapped
#' confidence intervals: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces a multivariable binomial logistic
#' regression models for a set of explanatory variables against a binary
#' dependent.
#'
#' Uses \code{\link[stats]{glm}} with \code{finalfit} modelling conventions.
#' \code{boot::\link[boot]{boot}} is used to draw bootstrapped confidence
#' intervals on fixed effect model coefficients. Output can be passed to
#' \code{\link{fit2df}}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector length 1:  name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param R Number of draws.
#' @return A multivariable \code{\link[stats]{glm}} fitted model with
#'   bootstrapped confidence intervals. Output is of class \code{glmboot}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' ## Note number of draws set to 100 just for speed in this example
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#'   glmmulti_boot(dependent, explanatory, R=100) %>%
#'   fit2df(estimate_suffix="(multivariable (BS CIs))")
#'

glmmulti_boot <- function(.data, dependent, explanatory, R=1000){
  formula <- paste(dependent, "~", paste(explanatory, collapse="+"))
  # function to get coefficients
  ci <- function(formula, data, indices) {
    d <- data[indices,]
    fit <- glm(formula, family="binomial", data=d)
    return(fit$coefficients)
  }
  bs.out <- boot::boot(data=.data, statistic=ci,
                       R=R, formula=formula)
  class(bs.out) = "glmboot"
  return(bs.out)
}
