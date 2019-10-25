#' Remove regression reference level row from table
#'
#' This looks for a column with a name including "Coefficient", "OR", or "HR"
#' (\code{\link{finalfit}} defaults) and removes any rows with "-" (the default
#' for the reference level). Can also be combined to produce an
#' \code{\link{or_plot}}, see below.
#'
#' @param .data Output from \code{\link{finalfit}} or similar.
#' @param only_binary Logical. Remove reference level only for two-level
#'   factors. When set to false, reference level for all factors removed.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' # Table example
#' explanatory = c("age.factor", "age", "sex.factor", "nodes", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#' 	 finalfit(dependent, explanatory, add_dependent_label = FALSE) %>%
#' 	 ff_remove_ref() %>%
#' 	 dependent_label(colon_s, dependent)
#'
#' # Plot example
#' explanatory = c("age.factor", "age", "sex.factor", "nodes", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory, total_col = TRUE, fit_id=TRUE) %>%
#'   ff_merge(
#'     glmuni(colon_s, dependent, explanatory) %>%
#'     fit2df()) %>%
#'   ff_remove_ref() %>%
#'   dplyr::select(-`OR`) -> factorlist_plot
#'
#' colon_s %>%
#'    or_plot(dependent, explanatory, factorlist = factorlist_plot)
ff_remove_ref <- function(.data, only_binary = TRUE){
	if(!any(names(.data) == "label")) stop("finalfit function must include: add_dependent_label = FALSE")
	# estimate_col = grep("Coefficient*|OR*|HR*", names(.data), value=TRUE)[1]
	df.out = .data %>% 
		dplyr::mutate(label = ifelse(label == "", NA, label)) %>% 
		tidyr::fill(label) %>% 
		dplyr::group_by(label)
	if(only_binary){
		df.out = df.out %>% 
			dplyr::filter(levels %in% c("Mean (SD)", "Median (IQR)") | 
											dplyr::row_number() != 1 |
											dplyr::n() > 2)
	} else {
		df.out = df.out %>% 
			dplyr::filter(levels %in% c("Mean (SD)", "Median (IQR)") | 
											dplyr::row_number() != 1)
	}
	df.out %>% 
		as.data.frame() %>% 
		rm_duplicate_labels()
}

#' @rdname ff_remove_ref
#' @export
finalfit_remove_ref = ff_remove_ref


#' Remove p-value from output
#'
#' This will work with \code{\link{finalfit}} and any \code{\link{fit2df}}
#' output.
#'
#' @param .data Output from \code{\link{finalfit}} or similar.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'   finalfit(dependent, explanatory) %>%
#'   ff_remove_p()
ff_remove_p <- function(.data){
	.data %>% 
		dplyr::mutate_all(~ gsub(", p[=<][0123456789.]*", "", .)) 
}

#' @rdname ff_remove_p
#' @export
finalfit_remove_p = ff_remove_p



#' Include only percentages for factors in \code{\link{summary_factorlist}} output
#'
#' @param .data Output from \code{\link{finalfit}} or similar.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory) %>%
#'   ff_percent_only()
ff_percent_only <- function(.data){
	if(!any(names(.data) == "label")) stop("summary_factorlist() must include: add_dependent_label = FALSE")
	.data %>% 
		dplyr::mutate_at(vars(-one_of("label", "levels", "p")), ~ dplyr::case_when(
			!levels %in% c("Mean (SD)", "Median (IQR)") ~ stringr::str_extract(., "(?<=\\().+?(?=\\))"), 
			TRUE ~ .))
}

#' @rdname ff_percent_only
#' @export
finalfit_percent_only = ff_percent_only
