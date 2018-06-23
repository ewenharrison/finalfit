#' Cox proprotional hazards univariable models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces multiple univariable Cox Proportional Hazard
#'   regression models for a set of explanatory variables against a survival object.
#'
#' Uses \code{\link[survival]{coxph}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of survival object in form \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @return A list of univariable \code{\link[survival]{coxph}} fitted model outputs.
#'   Output is of class \code{coxphlist}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family \code{finalfit} model wrappers
#' @export
#'
#' @examples
#' # Cox Proportional Hazards univariable analysis.
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#' colon_s %>%
#' 	coxphuni(dependent, explanatory) %>%
#' 	fit2df()

coxphuni <- function(.data, dependent, explanatory){
  result <- list()
  for (i in 1:length(explanatory)){
    result[[i]] <- survival::coxph(as.formula(paste0("survival::", dependent, "~", explanatory[i])), data=.data)
  }
  class(result) = "coxphlist"
  return(result)
}
