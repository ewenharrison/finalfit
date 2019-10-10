#' Add column totals to \code{summary_factorlist()} output
#'
#' @param df.in \code{summary_factorlist()} output.
#' @param .data Data frame used to create \code{summary_factorlist()}.
#' @param dependent Character. Name of dependent variable. 
#' @param label Character. Label for total row. 
#' @param prefix Character. Prefix for column totals. 
#'
#' @return Data frame. 
#' @export
#'
#' @examples
#' library(finalfit)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>% 
#'  summary_factorlist(dependent, explanatory) %>% 
#'  ff_column_totals(colon_s, dependent)
#' 
#' # Ensure works with missing data in dependent
#' colon_s = colon_s %>% 
#'  dplyr::mutate(
#'   mort_5yr = forcats::fct_explicit_na(mort_5yr)
#'  )
#'  colon_s %>% 
#'  summary_factorlist(dependent, explanatory) %>% 
#'  ff_column_totals(colon_s, dependent)
ff_column_totals <- function(df.in, .data, dependent, label = "Total N", prefix = "N="){
	if(!any(names(df.in) == "label")) stop("finalfit function must include: add_dependent_label = FALSE")
	if(.data %>% 
		 dplyr::pull(dependent) %>% 
		 is.na() %>% 
		 any()) {message("Dependent includes missing data. This is dropped. Consider forcats::fct_explicit_na")}
	
	totals = .data %>% 
		tidyr::drop_na(dependent) %>% 
		dplyr::group_by(!! dplyr::sym(dependent)) %>% 
		dplyr::count() %>% 
		tidyr::pivot_wider(names_from = dependent, values_from = n) %>% 
		as.data.frame() %>% 
		dplyr::mutate(label = label, 
									levels= "") %>% 
		dplyr::mutate_if(is.integer, ~ paste0(prefix, .)) %>% 
		dplyr::select(label, levels, dplyr::everything()) 
	
	
	df.out = dplyr::bind_rows(totals, df.in)
	df.out[1, is.na(df.out[1, ])] = "" # For neatness change NA to "" in top row
	
	if(any(names(df.out) == "Total")){
	df.out[1, "Total"] = 
		sapply(df.out[1, ], stringr::str_extract, pattern = "[1234567890]+") %>% 
		as.numeric() %>% 
		sum(na.rm = TRUE) %>% 
		paste0(prefix, .)
	}
	return(df.out)
}

#' @rdname ff_column_totals
#' @export
finalfit_column_totals = ff_column_totals
