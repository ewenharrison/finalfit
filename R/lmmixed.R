#' Mixed effects linear regression models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces mixed effects linear
#'   regression models for a set of explanatory variables against a continuous dependent.
#'
#' Uses \code{lme4::\link[lme4]{lmer}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}. This is only currently set-up to take a single random effect
#'   as a random intercept. Can be updated in future to allow multiple random intercepts,
#'   random gradients and interactions on random effects if there is a need.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1, name of depdendent variable (must be continuous vector).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param random_effect Character vector of length 1, name of random effect variable.
#' @return A list of multivariable \code{lme4::\link[lme4]{lmer}} fitted model outputs.
#'   Output is of class \code{lmerMod}.
#'
#' @seealso \code{\link{fit2df}}
#' @family \code{finalfit} model wrappers
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel")

lmmixed <- function(.data, dependent, explanatory, random_effect){
  lme4::lmer(paste0(dependent, "~", paste(explanatory, collapse="+"), " + (1|", random_effect, ")"),
             data=.data)
}
