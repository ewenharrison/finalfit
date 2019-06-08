#' Permuate explanatory variables to produce multiple output tables for common
#' regression models
#'
#' @param .data Data frame or tibble. 
#' @param dependent Character vector of length 1:  quoted name of dependent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}.
#' @param explanatory_base Character vector of any length: quoted name(s) of
#'   base model explanatory variables.
#' @param explanatory_permute Character vector of any length: quoted name(s) of
#'   explanatory variables to permute through models.
#' @param ... Other arguments to \code{\link{finalfit}}
#'
#' @return Returns a list of data frame with the final model table.
#' @export
#'
#' @examples
#' explanatory_base = c("age.factor", "sex.factor")
#' explanatory_permute = c("obstruct.factor", "perfor.factor", "node4.factor")
#' 
#' # Linear regression
#' colon_s %>% 
#'   finalfit_permute("nodes", explanatory_base, explanatory_permute)
#' 
#' # Cox proportional hazards regression
#' colon_s %>% 
#'   finalfit_permute("Surv(time, status)", explanatory_base, explanatory_permute)
#'
#' # Logistic regression
#' colon_s %>% 
#'   finalfit_permute("mort_5yr", explanatory_base, explanatory_permute)
#' 
#' # Logistic regression with random effect (glmer)
#' colon_s %>% 
#'   finalfit_permute("mort_5yr", explanatory_base, explanatory_permute, 
#'     random_effect = "hospital")
ff_permute <- function(.data, dependent = NULL, 
														 explanatory_base = NULL, explanatory_permute = NULL,
														 ...){
	args = list(...)
	explanatory_permute %>% 
		purrr::map(~ c(.x, explanatory_base))  %>% 
		purrr::map(~ do.call(finalfit, c(list(.data, dependent, explanatory = .x), args)))
}

#' @rdname ff_permute
#' @export
finalfit_permute = ff_permute