#' Produce a table and plot
#'
#' Wraps \code{\link{or_plot}}, \code{\link{hr_plot}}, and
#' \code{\link{coefficient_plot}} and sends to the appropriate method depending
#' on the dependent variable type.
#' @param .data Data frame. 
#' @param dependent Character vector of length 1. 
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param ... Pass arguments \code{\link{or_plot}}, \code{\link{hr_plot}}, or
#' \code{\link{coefficient_plot}}
#'
#' @return A table and a plot using \code{\link{ggplot2}}
#' @export
#' @family finalfit plot functions
#' @examples
#' # Coefficient plot
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "nodes"
#' colon_s %>%
#' 	ff_plot(dependent, explanatory)
#' 
#' # Odds ratio plot
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	ff_plot(dependent, explanatory)
#' 
#' 	# Hazard ratio plot	
#' dependent = "Surv(time, status)"
#' colon_s %>%
#' 	ff_plot(dependent, explanatory, dependent_label = "Survival")
ff_plot <- function(.data, dependent, explanatory, ...){
	if(is.data.frame(.data)==FALSE) stop(".data is not dataframe")
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)) stop("No dependent variable provided")
	
	# Args
	args = list(.data, dependent, explanatory, ...)
	
	# What is dependent variable
	d_variable = .data[,names(.data) %in% dependent]
	if(is.survival(dependent)){
		d_type = "survival"
	}else{
		d_type = variable_type(d_variable)
	}
	# Send to method
	if (d_type == "survival"){
		do.call(hr_plot, args)
	} else if (d_type == "factor" || d_type == "character" || d_type == "logical"){
		do.call(or_plot, args)
	} else if (d_type == "numeric"){
		do.call(coefficient_plot, args)
	} else {
		stop("Plotting not support for this dependent variable type")
	}
}

#' @rdname ff_plot
#' @export
finalfit_plot = ff_plot
