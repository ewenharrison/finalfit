#' Binomial logistic regression univariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multiple univariable binomial logistic
#'   regression models for a set of explanatory variables against a binary dependent.
#'
#' Uses \code{\link[stats]{glm}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of depdendent variable (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @return A list of univariable \code{\link[stats]{glm}} fitted model outputs.
#'   Output is of class \code{glmlist}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family \code{finalfit} model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)")
#'
#'
glmuni <- function(.data, dependent, explanatory){
  result <- list()
  for (i in 1:length(explanatory)){
    result[[i]] <- glm(paste(dependent, "~", explanatory[i]), data=.data, family="binomial")
  }
  class(result) = "glmlist"
  return(result)
}
