#' Binomial logistic regression multivariable models: \code{finalfit} model
#' wrapper
#'
#' Using \code{finalfit} conventions, produces a multivariable binomial
#' logistic regression model for a set of explanatory variables against a
#' binary dependent.
#'
#' Uses \code{\link[stats]{glm}} with \code{finalfit} modelling conventions.
#' Output can be passed to \code{\link{fit2df}}.
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1: name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param family Character vector quoted or unquoted of the error distribution
#'   and link function to be used in the model, see \code{\link[stats]{glm}}.
#' @param ... Other arguments to pass to \code{\link[stats]{glm}}.
#' @return A multivariable \code{\link[stats]{glm}} fitted model.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")
#' 
glmmulti <- function(.data, dependent, explanatory, family = "binomial", ...){
  ff_eval(
    glm(ff_formula(dependent, explanatory),
        data = .data, family = family, ...)
  )
}
