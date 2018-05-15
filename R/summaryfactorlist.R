#' Summarise a set of factors (or continuous variables) by a dependent variable
#'
#' A function that takes a single dependent variable with a vector of
#' explanatory variable names (continuous or categorical variables) to produce a
#' summary table.
#'
#' This function is mostly a wrapper for \code{Hmisc:::summary.formula(...,
#' method = "reverse")} but produces a publication-ready table the way we like
#' them. It usually takes a categorical dependent variable (with two to five
#' levels) to produce a cross table of counts and proportions expressed as
#' percentages. However, it will take a continuous dependent variable to produce
#' mean (standard deviation) or median (interquartile range) for use with linear
#' regression models.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of depdendent variable
#'   (2 to 5 factor levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param cont Summary for continuous variables: mean (standard deviation) or
#'   median (interquartile range).
#' @param p Logical: Include statistical test (see
#'   \code{\link[Hmisc]{summary.formula}}).
#' @param na_include Logical: include missing data in summary (\code{NA}).
#' @param column Logical: Compute margins by column rather than row.
#' @param total_col Logical: include a total column summing across factor
#'   levels.
#' @param orderbytotal Logical: order final table by total column high to low.
#' @param fit_id Logical: not used directly, allows merging via
#'   \code{\link{finalfit_merge}}.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when
#'   \code{na_include=TRUE}.
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table
#' @return Returns a \code{factorlist} dataframe.
#'
#' @family finalfit wrappers
#' @seealso \code{\link{fit2df}}
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' # Load example dataset, modified version of survival::colon
#' data(colon_s)
#'
#' # Table 1 - Patient demographics ----
#' explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
#' dependent = "perfor.factor"
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory, p=TRUE)
#'
#' # summary.factorlist() is also commonly used to summarise any number of
#' # variables by an outcome variable (say dead yes/no).
#'
#' # Table 2 - 5 yr mortality ----
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory)

summary_factorlist <- function(.data, dependent=NULL, explanatory, cont="mean", p=FALSE, na_include=FALSE,
															 column=FALSE, total_col=FALSE, orderbytotal=FALSE, fit_id=FALSE,
															 na_to_missing=TRUE, add_dependent_label=FALSE){
	if(is.data.frame(.data)==FALSE) stop(".data is not dataframe")
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data) # tbl work different, convert to data.frame
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)){
		warning("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		.data$all = factor(1, labels="all")
	}

	args = list(.data=.data, dependent=dependent, explanatory=explanatory, cont=cont, p=p, na_include=na_include,
							column=column, total_col=total_col, orderbytotal=orderbytotal, fit_id=fit_id,
							na_to_missing=na_to_missing, add_dependent_label=add_dependent_label)

	# Survival object
	d_is.surv = grepl("Surv[(].*[)]", dependent)

	if(d_is.surv){
		warning("Dependent variable is a survival object")
		.data$all = factor(1, labels="all")
		suppressWarnings(
			do.call(summary_factorlist1, args=list(.data=.data, dependent = "all",  explanatory=explanatory, fit_id=fit_id))
		)
	} else {

		# Extract dependent variable
		d_variable = .data[,names(.data) %in% dependent]

		if(length(d_variable)==0){
			stop("Dependent variable length is 0")
		}

		# Logical is.factor
		d_is.factor = is.factor(d_variable) |
			is.character(d_variable)

		# Number of levels of dependent
		d.len = length(levels(d_variable))

		# Non-factor case
		if(!d_is.factor){
			warning("Dependent is not a factor and will be treated as a continuous variable")
			do.call(summary_factorlist0, args)
		} else {

			# Factor case
			if (d.len == 1){
				do.call(summary_factorlist1, args)
			} else if (d.len == 2){
				do.call(summary_factorlist2, args)
			} else if (d.len == 3){
				do.call(summary_factorlist3, args)
			} else if (d.len == 4){
				do.call(summary_factorlist4, args)
			} else if (d.len == 5){
				do.call(summary_factorlist5, args)
			}
		}
	}
}
