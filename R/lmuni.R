#' Linear regression univariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multiple univariable linear
#'   regression models for a set of explanatory variables against a continuous dependent.
#'
#' Uses \code{\link[stats]{lm}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1, name of depdendent variable (must be continuous vector).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param ... Other arguments to pass to \code{\link[stats]{lm}}.
#' @return A list of multivariable \code{\link[stats]{lm}} fitted model outputs.
#'   Output is of class \code{lmlist}.
#'
#' @seealso \code{\link{fit2df}}
#' @family \code{finalfit} model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "nodes"
#'
#' colon_s %>%
#'   lmuni(dependent, explanatory) %>%
#'   fit2df()
#'
lmuni <- function(.data, dependent, explanatory, ...){
  result <- list()
  for (i in 1:length(explanatory)){
    result[[i]] <- ff_eval(
    	lm(ff_formula(dependent, explanatory[i]), data = .data, ...)
    )
  }
  class(result) = "lmlist"
  return(result)
}
