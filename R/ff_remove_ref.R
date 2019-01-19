#' Remove regression reference level row from table
#'
#' This looks for a column with a name including "Coefficient", "OR", or "HR"
#' (\code{\link{finalfit}} defaults) and removes any rows with "-" (the default
#' for the reference level). Can also be combined to produce an
#' \code{\link{or_plot}}, see below.
#'
#' @param .data Output from \code{\link{finalfit}} or similar.
#'
#' @return Dataframe.
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
ff_remove_ref <- function(.data){
	if(!any(names(.data) == "label")) stop("finalfit function must include: add_dependent_label = FALSE")
	estimate_col = grep("Coefficient*|OR*|HR*", names(.data), value=TRUE)[1]
	.data %>% 
		dplyr::mutate(label = ifelse(label == "", NA, label)) %>% 
		tidyr::fill(label) %>% 
		dplyr::filter_at(dplyr::vars(estimate_col), dplyr::any_vars(. != "-")) %>% 
		rm_duplicate_labels()
}

#' @rdname ff_remove_ref
#' @export
finalfit_remove_ref = ff_remove_ref