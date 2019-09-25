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
#' @param multiple_tables Logical. Multiple model tables as a list, or a single
#'   table including multiple models.
#' @param include_base_model Logical. Include model using \code{explanatory_base}
#' variables only.
#' @param include_full_model Logical. Include model using all \code{explanatory_base}
#' and \code{explanatory_permute} variables.
#' @param base_on_top Logical. Base variables at top of table, or bottom of
#'   table.
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
#' # colon_s %>%
#' #   finalfit_permute("mort_5yr", explanatory_base, explanatory_permute,
#' #     random_effect = "hospital")
ff_permute <- function(.data, dependent = NULL, 
											 explanatory_base = NULL, explanatory_permute = NULL,
											 multiple_tables = FALSE, 
											 include_base_model = TRUE,
											 include_full_model = TRUE,
											 base_on_top = TRUE, ...){
	args = list(...)
	
	if(base_on_top){
		explanatory = explanatory_permute %>% 
			purrr::map(~ c(explanatory_base, .x))
	} else {
		explanatory = explanatory_permute %>% 
			purrr::map(c, explanatory_base)
	}
	
	if(include_base_model){
		explanatory = c(list(explanatory_base), explanatory)
	}
	
	fits = explanatory %>% 
		purrr::map(~ do.call(finalfit, c(list(.data, dependent, explanatory = .x, keep_fit_id = TRUE), 
																		 args)))
	
	if(base_on_top){
		explanatory = c(explanatory_base, explanatory_permute)
	} else {
		explanatory = c(explanatory_permute, explanatory_base)
	}
	
	if(include_full_model){
		fits = c(fits,
						 list(
						 	finalfit(.data, dependent, explanatory, keep_fit_id = TRUE, ...)
						 )
		)
	}
	
	# Multiple tables ----
	if(multiple_tables){
		out = fits %>% 
			purrr::map(dplyr::select, -fit_id)
		return(out)
	}
	
	# Single table ----
	uni = finalfit(.data, dependent, explanatory, keep_fit_id = TRUE, 
								 add_dependent_label = FALSE, ...) %>% 
		dplyr::select(-length(.)) # remove last column
	
	## multivariable only
	fits = fits %>% 
		purrr::map(dplyr::select, c(1, length(.[[1]]))) # first and last columns
	
	## number of models
	n_fits = 1:length(fits)
	
	## paste incremental integer to model name
	fits = fits %>% 
		purrr::map(~ names(.x)[2]) %>% 
		purrr::map2(n_fits, ~ paste(.x, .y)) %>% 
		purrr::map2(fits, ~ dplyr::rename(.y, !!.x := 2))
	
	## create final table
	out = fits %>% 
		purrr::reduce(dplyr::full_join, by = "fit_id") %>% 
		dplyr::left_join(uni, ., by = "fit_id") %>% 
		dplyr::mutate_all(~ ifelse(is.na(.), "-", .)) %>% 
		dplyr::select(-fit_id, -index) %>% 
		dependent_label(.data = .data, dependent = dependent)
	return(out)
}

#' @rdname ff_permute
#' @export
finalfit_permute = ff_permute
