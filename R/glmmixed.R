#' Mixed effects binomial logistic regression models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces mixed effects binomial logistic
#'   regression models for a set of explanatory variables against a binary dependent.
#'
#' Uses \code{lme4::\link[lme4]{glmer}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}. This is only currently set-up to take a single random effect
#'   as a random intercept. Can be updated in future to allow multiple random intercepts,
#'   random gradients and interactions on random effects if there is a need
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1, name of depdendent variable (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param random_effect Character vector of length 1, name of random effect variable.
#' @return A list of multivariable \code{lme4::\link[lme4]{glmer}} fitted model outputs.
#'   Output is of class \code{glmerMod}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family \code{finalfit} model wrappers
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")

glmmixed <- function(.data, dependent, explanatory, random_effect){
  lme4::glmer(paste0(dependent, "~", paste(explanatory, collapse="+"), " + (1|", random_effect, ")"),
              data=.data, family="binomial", control=lme4::glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=200000)))
}
