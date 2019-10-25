#' Add column totals to \code{summary_factorlist()} output
#'
#' @param df.in \code{summary_factorlist()} output.
#' @param .data Data frame used to create \code{summary_factorlist()}.
#' @param dependent Character. Name of dependent variable. 
#' @param percent Logical. Include percentage. 
#' @param digits Integer length 1. Number of digits for percentage. 
#' @param label Character. Label for total row. 
#' @param prefix Character. Prefix for column totals, e.g "N=". 
#'
#' @return Data frame. 
#' @export
#'
#' @examples
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
ff_column_totals <- function(df.in, .data, dependent, percent = TRUE, digits = 1, label = NULL, prefix = ""){
	if(!any(names(df.in) == "label")) stop("finalfit function must include: add_dependent_label = FALSE")
	if(.data %>% 
		 dplyr::pull(dependent) %>% 
		 is.na() %>% 
		 any()) {message("Dependent includes missing data. These are dropped. Consider forcats::fct_explicit_na")}
	
	# Create column totals
	totals = .data %>% 
		tidyr::drop_na(dependent) %>% 
		dplyr::group_by(!! dplyr::sym(dependent)) %>% 
		dplyr::count() %>% 
		dplyr::group_by() %>% 
		dplyr::mutate(
			grand_total = sum(n, na.rm = TRUE),
			percent = 100 * n / grand_total
		)
	grand_total = totals %>% dplyr::pull(grand_total) %>% unique()
	
	if(percent){
		totals = totals %>% 
			dplyr::mutate(
				n = paste0(prefix, format_n_percent(n, percent, digits))
			)
	} else {
		totals = totals %>% 
			dplyr::mutate(
				n = paste0(prefix, n)
			)
	}
	
	if(is.null(label) & percent) label = "Total N (%)"
	if(is.null(label) & !percent) label = "Total N"
	
	# Pivot and add
	totals = totals %>% 
		dplyr::select(-c(grand_total, percent)) %>% 
		tidyr::pivot_wider(names_from = dependent, values_from = n) %>% 
		as.data.frame() %>% 
		dplyr::mutate(label = label, 
									levels= "") %>% 
		dplyr::select(label, levels, dplyr::everything()) 
	
	df.out = dplyr::bind_rows(totals, df.in)
	df.out[1, is.na(df.out[1, ])] = "" # For neatness change NA to "" in top row
	
	# Make total
	if(any(names(df.out) == "Total")){
		df.out[1, "Total"] = paste0(prefix, grand_total)
	}
	return(df.out)
}

#' @rdname ff_column_totals
#' @export
finalfit_column_totals = ff_column_totals


#' Add row totals to \code{summary_factorlist()} output
#'
#' This adds a total and missing count to variables. This is useful for
#' continuous variables. Compare this to \code{summary_factorlist(total_col =
#' TRUE)} which includes a count for each dummy variable as a factor and mean
#' (sd) or median (iqr) for continuous variables.
#'
#' @param df.in \code{summary_factorlist()} output.
#' @param .data Data frame used to create \code{summary_factorlist()}.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param na_include Logical. Include a column of counts of missing data. 
#' @param total_name Character. Name of total column.
#' @param na_name Character. Name of missing column.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'  summary_factorlist(dependent, explanatory) %>%
#' 	ff_row_totals(colon_s, explanatory)
ff_row_totals <- function(df.in, .data, explanatory, na_include = TRUE, 
													total_name = "Total N", na_name= "Missing N"){
	if(!any(names(df.in) == "label")) 
		stop("summary_factorlist function must include: add_dependent_label = FALSE")
	
	df.out = df.in %>%
		dplyr::left_join(
			missing_glimpse(.data, explanatory) %>% 
				dplyr::mutate(label = as.character(label)),
		) %>%
		dplyr::mutate(            # Rename, change to character, remove "NAs"
			!! total_name := as.character(n) %>% dplyr::coalesce("")
		)
	if(na_include){
		df.out = df.out %>% 
			dplyr::mutate(
				!! na_name := as.character(missing_n) %>% dplyr::coalesce("")
			) %>%  # Reorder columns, remove unwanted columns
			dplyr::select(label, !! total_name, !! na_name, dplyr::everything(), 
										-c(n, missing_n, var_type, missing_percent))
	} else {
	df.out = df.out %>%
		dplyr::select(label, !! total_name, dplyr::everything(), 
									-c(n, missing_n, var_type, missing_percent))
	}
	return(df.out)
}

#' @rdname ff_row_totals
#' @export
finalfit_row_totals = ff_row_totals
