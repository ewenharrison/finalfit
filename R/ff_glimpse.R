#' Descriptive statistics for dataframe
#'
#' Everyone has a funcion like this, str, glimpse, glance etc. This one is
#' specifically designed for use with \code{finalfit} language. It is different
#' in dividing variables by numeric vs factor, the former using
#' \code{link[psych]{describe}}.
#'
#' @param .data Dataframe.
#' @param dependent Optional character vector: name(s) of depdendent
#'   variable(s).
#' @param explanatory Optional character vector: name(s) of explanatory
#'   variable(s).
#' @param digits Significant digits for continuous variable summaries
#' @param levels_cut Max number of factor levels to include in factor levels
#'   summary (in order to avoid the long printing of variables with many
#'   factors).
#'
#' @return Dataframe on summary data.
#' @export
#' @importFrom stats median
#'
#' @examples
#' library(finalfit)
#' dependent = 'mort_5yr'
#' explanatory = c("age.factor", "extent.factor", "perfor.factor")
#' colon_s %>%
#'   finalfit_glimpse(dependent, explanatory)

ff_glimpse <- function(.data, dependent=NULL, explanatory=NULL, digits = 1,
											 levels_cut = 5){
	if(is.null(dependent) && is.null(explanatory)){
		df.in = .data
	}else{
		keep = names(.data) %in% c(dependent, explanatory)
		df.in = .data[keep]
	}

	# Continuous
	df.in %>%
		dplyr::select_if(is.numeric) -> df.numeric

	if(dim(df.numeric)[2]!=0){
		df.numeric %>%
			missing_glimpse(digits=digits) -> df.numeric.out1

		df.numeric %>%
			purrr::map_df(function(x){
				mean = mean(x, na.rm = TRUE)
				sd = sd(x, na.rm = TRUE)
				min = min(x, na.rm = TRUE)
				quartile_25 = quantile(x, probs = 0.25, na.rm = TRUE)
				median =  median(x, na.rm = TRUE)
				quartile_75 = quantile(x, probs = 0.75, na.rm = TRUE)
				max = max(x, na.rm = TRUE)
				df.out = data.frame(mean, sd, min, quartile_25, median, quartile_75, max) %>%
					dplyr::mutate_all(round_tidy, digits=digits)
			}) -> df.numeric.out2

		df.numeric.out = data.frame(df.numeric.out1, df.numeric.out2)

	}else{
		df.numeric.out = df.numeric
	}

	# Factors
	df.in %>%
		dplyr::select_if(Negate(is.numeric)) -> df.factors

	if(dim(df.factors)[2]!=0){

		df.factors %>%
			missing_glimpse(digits=digits) -> df.factors.out1

		fac2char = function(., cut = levels_cut) {
			length(levels(.)) > cut
		}

		df.factors %>%
			dplyr::mutate_if(fac2char, as.character) -> df.factors


		df.factors %>%
			purrr::map_df(function(x){
				levels_n = length(levels(as.factor(x)))
				levels = ifelse(is.factor(x),
												forcats::fct_explicit_na(x) %>%
													levels() %>%
													paste0("\"", ., "\"", collapse = ", "),
												"-")
				levels_count = ifelse(is.factor(x),
															summary(x) %>%
																paste(collapse = ", "),
															"-")
				levels_percent = ifelse(is.factor(x),
																summary(x) %>%
																	prop.table() %>%
																	`*`(100) %>%
																	format(digits = 2) %>%
																	paste(collapse=", "),
																"-")
				df.out = dplyr::data_frame(levels_n, levels, levels_count, levels_percent) %>% data.frame()
			}) -> df.factors.out2

		df.factors.out = data.frame(df.factors.out1, df.factors.out2)

	}else{
		df.factors.out = df.factors
	}

	cat("Numerics\n")
	print(df.numeric.out, row.names = TRUE)
	cat("\nFactors\n")
	print(df.factors.out, row.names = TRUE)

	return(invisible(
		list(
			numerics = df.numeric.out,
			factors = df.factors.out))
	)
}


#' @rdname ff_glimpse
#' @export
finalfit_glimpse <- ff_glimpse
