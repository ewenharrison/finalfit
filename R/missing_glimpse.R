#' Summary of missing values
#'
#' @param .data Data frame.
#' @param dependent Optional character vector: name(s) of depdendent
#'   variable(s).
#' @param explanatory Optional character vector: name(s) of explanatory
#'   variable(s).
#' @param digits Number of decmial places to show for percentage missing.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' colon_s %>%
#' 	missing_glimpse()
missing_glimpse <- function(.data, dependent=NULL, explanatory=NULL, digits = 1){
	if(is.null(dependent) && is.null(explanatory)){
		df.in = .data
	}else{
		keep = names(.data) %in% c(dependent, explanatory)
		df.in = .data[keep]
	}

	df.in %>%
		purrr::map_df(function(x){
			obs = length(x)
			missing_n = sum(is.na(x))
			n = obs-missing_n
			missing_percent = round_tidy(100*missing_n/obs, digits=digits)
			dplyr::data_frame(n, missing_n, missing_percent)
		}) -> df.out1

	df.in %>%
		extract_variable_label() %>%
		data.frame(label=.) -> df.out2

	df.out = data.frame(df.out2, df.out1)
	return(df.out)
}
