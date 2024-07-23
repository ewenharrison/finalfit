#' Generate newdata for simulations
#'
#' Generate newdata while respecting the variable types and factor levels in the
#' primary data frame used to run model.
#'
#' Generate model predictions against a specified set of explanatory levels with
#' bootstrapped confidence intervals. Add a comparison by difference or ratio of
#' the first row of \code{newdata} with all subsequent rows.
#'
#' @param .data Dataframe.
#' @param dependent Optional character vector of length 1:  name of depdendent
#'   variable. Not usually specified in bootstrapping model predictions.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param rowwise Logical. Format \code{newdata} is provided in.
#' @param newdata A list of rows or columns coresponding exactly to the order of
#'   explanatory variables. Useful errors generated if requirements not fulfilled
#' @return A list of multivariable \code{\link[stats]{glm}} fitted model
#'   outputs. Output is of class \code{glmlist}.
#'
#' @seealso \code{\link{boot_predict}} \code{\link{boot_compare}}
#' @export
#'
#' @examples
#' # See boot_predict.
#' library(finalfit)
#' library(dplyr)
#'
#' # Predict probability of death across combinations of factor levels
#' explanatory = c("age.factor", "extent.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#'
#' # Generate combination of explanatory variable levels rowwise
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, newdata = list(
#'     c("<40 years",  "Submucosa", "No"),
#'     c("<40 years", "Submucosa", "Yes"),
#'     c("<40 years", "Adjacent structures", "No"),
#'     c("<40 years", "Adjacent structures", "Yes")
#'    )) -> newdata
#'
#' # Generate combination of explanatory variable levels colwise.
#' explanatory = c("nodes", "extent.factor", "perfor.factor")
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, rowwise = FALSE, newdata = list(
#'   rep(seq(0, 30), 4),
#'   c(rep("Muscle", 62), rep("Adjacent structures", 62)),
#'   c(rep("No", 31), rep("Yes", 31), rep("No", 31), rep("Yes", 31))
#' )) -> newdata
#'

ff_newdata <- function(.data, dependent=NULL, explanatory=NULL,  rowwise=TRUE, newdata){
	if(is.null(dependent) && is.null(explanatory)){
		df.out = .data %>% dplyr::slice(0)
	}else{
		.data %>%
			dplyr::select(dependent, explanatory) %>%
			dplyr::slice(0) -> df.out
	}

	if(rowwise){
		is_numeric = sapply(df.out, is.numeric)
		df.new = do.call(rbind, newdata) %>% data.frame(stringsAsFactors = FALSE) %>% 
			dplyr::mutate_if(is_numeric, as.numeric)
	}else{
		df.new = do.call(data.frame, newdata)
	}

	df.out[1:dim(df.new)[1],] = df.new
	return(df.out)
}

#' @rdname ff_newdata
#' @export
finalfit_newdata = ff_newdata



#' Return the most frequent level in a factor
#' 
#' When producing conditional estimates from a regression model, it is often useful
#' to set variables not of interest to their mode when creating the newdata object. 
#'
#' @param ... Unquoted factor names.
#'
#' @return The most frequent level in a factor.
#' @export
#' @seealso \code{\link{summary_df}} \code{\link{ff_expand}}
#'
#' @examples
#' library(dplyr)
#' colon_s %>% 
#'   summarise(age.factor = ff_mode(age.factor))
#'   
#' colon_s %>% 
#'   select(sex.factor, rx.factor, obstruct.factor, perfor.factor) %>% 
#'   summarise(across(everything(), ff_mode))
#'   
#' colon_s %>% 
#'   reframe(across(where(is.factor), ff_mode))
#'   # Note, 4 rows is returned in this example because 4 factor levels within `hospital` 
#'   # have the same frequency. 
ff_mode <- function(...){
	forcats::fct_count(...) %>% dplyr::slice_max(n) %>% dplyr::pull(f)
}

#' @rdname ff_mode
#' @export
finalfit_mode = ff_mode


#' Summarise with mode for factors and mean/median for numeric variables
#' 
#' When producing conditional estimates from a regression model, it is often useful
#' to set variables not of interest to their mode for factors and mean or median for numerics
#' when creating the newdata object. 
#'
#' @param .data A data frame or tibble. 
#' @param cont One of "mean" or "median": the summary estimate for continuous variables. 
#'
#' @return A data frame or tibble with the mode for factors and mean/median 
#' for continuous variables. 
#' @export 
#' @seealso \code{\link{ff_mode}} \code{\link{ff_expand}}
#' @examples
#' library(dplyr)
#' colon_s %>% 
#'   select(age, sex.factor, obstruct.factor, perfor.factor) %>% 
#'   summary_df()
#'   
#'   colon_s %>% 
#'     select(age, sex.factor, obstruct.factor, perfor.factor) %>% 
#'     summary_df(cont = "median")
summary_df <- function(.data, cont = "mean"){
	names(.data) %>% 
		purrr::map_dfc(
			~ if(.data %>% pull(.x) %>% {!is.numeric(.)}){
				.data %>% 
					dplyr::summarise(!! sym(.x) := ff_mode(!! sym(.x)))
			} else if(cont == "mean"){
				.data %>% 
					dplyr::summarise(!! sym(.x) := mean(!! sym(.x), na.rm = TRUE))
			} else if(cont == "median"){
				.data %>% 
					dplyr::summarise(!! sym(.x) := median(!! sym(.x), na.rm = TRUE))
			}
		)
}

#' Summarise with mode and mean/median and expand given factors
#' 
#' When producing conditional estimates from a regression model, it is often useful
#' to set variables not of interest to their mode for factors and mean or median for numerics
#' when creating the newdata object, and combine these with all levels for factors of interest. 
#'
#' @param .data A data frame or tibble.
#' @param ... Factors to expand. 
#' @param cont One of "mean" or "median": the summary estimate for continuous variables. 
#'
#' @return A data frame or tibble with the mode for factors and mean/median 
#' for continuous variables, with given factors expanded to include all levels. 
#' @export
#' @seealso \code{\link{ff_mode}} \code{\link{summary_df}}
#'
#' @examples
#' library(dplyr)
#' colon_s %>% 
#' select(-hospital) %>% 
#' ff_expand(age.factor, sex.factor)
ff_expand <- function(.data, ..., cont = "mean"){
	.data %>% 
		summary_df(cont = cont) %>% 
		tidyr::complete(...) %>% 
		tidyr::fill(dplyr::everything(), .direction = "downup")
}

#' @rdname ff_expand
#' @export
finalfit_expand = ff_expand