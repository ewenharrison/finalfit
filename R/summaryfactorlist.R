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
#' @param dependent Character vector of length 1:  name of dependent variable
#'   (2 to 5 factor levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param cont Summary for continuous variables: "mean" (standard deviation) or
#'   "median" (interquartile range).
#' @param cont_cut Numeric: number of unique values in continuous variable at which to consider it a factor.
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
#' @param dependent_label_prefix Add text before dependent label
#' @param dependent_label_suffix Add text after dependent label
#' @return Returns a \code{factorlist} dataframe.
#'
#' @family finalfit wrappers
#' @seealso \code{\link{fit2df}}
#' @export
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
#'   summary_factorlist(dependent, explanatory, p=TRUE)
#'
#' # summary.factorlist() is also commonly used to summarise any number of
#' # variables by an outcome variable (say dead yes/no).
#'
#' # Table 2 - 5 yr mortality ----
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory)

summary_factorlist <- function(.data, dependent = NULL, explanatory, cont = "mean", cont_cut = 5,
															 p = FALSE, na_include = FALSE,
															 column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
															 na_to_missing = TRUE, add_dependent_label = FALSE,
															 dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){
	if(is.data.frame(.data) == FALSE) stop(".data is not dataframe")
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data) # tbl work different, convert to data.frame
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)){
		warning("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		.data$all = factor(1, labels="all")
	}

	args = list(.data = .data, dependent = dependent, explanatory = explanatory,
							cont = cont, cont_cut = cont_cut, p = p, na_include = na_include,
							column = column, total_col = total_col, orderbytotal = orderbytotal, fit_id = fit_id,
							na_to_missing = na_to_missing, add_dependent_label = add_dependent_label,
							dependent_label_prefix = dependent_label_prefix,
							dependent_label_suffix = dependent_label_suffix)

	# Survival object
	d_is.surv = grepl("Surv[(].*[)]", dependent)

	if(d_is.surv){
		warning("Dependent variable is a survival object")
		.data$all = factor(1, labels="all")
		suppressWarnings(
			do.call(summary_factorlist1, args=list(.data = .data, dependent = "all",  explanatory = explanatory, fit_id = fit_id))
		)
	} else {

		# Extract dependent variable
		d_variable = .data[,names(.data) %in% dependent]

		if(length(d_variable) == 0){
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





#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist0 <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix=""){

	s = summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse="+"))), data = .data,
											overall = FALSE, method = "response", na.include = na_include, continuous = cont_cut, g = 1,
											fun=function(x) {
												mean = mean(x)
												sd = sd(x)
												L = quantile(x, probs=c(0.25))[[1]]
												median = quantile(x, probs=c(0.5))[[1]]
												U = quantile(x, probs=c(0.75))[[1]]
												return(data.frame(mean, sd, L, median, U))
											}
	)

	# Dataframe
	df.out = data.frame(label=attr(s, "vlabel"), levels=attr(s, "dimnames")[[1]])

	# Add in lm level names, this needs hacked in given above methodology
	if (fit_id){
		vname = attr(s, "vname")
		vname_logical = (vname == "")
		for (i in 1:length(vname)){
			if(vname_logical[i]) vname[i] = vname[i-1]
		}
		levels = as.character(df.out$levels)

		# Error with continuous vs continuous variables and fit_id, fix:
		regex_sqbracket = "^(\\[).*(\\])$"
		drop = grepl(regex_sqbracket, levels)
		levels[drop] = ""
		df.out$fit_id = paste0(vname, levels)
		df.out$index = 1:dim(df.out)[1]
	}

	if (cont=="mean"){
		mean.out = sprintf("%.1f", matrix(s[,2]))
		sd.out = sprintf("%.1f", matrix(s[,3]))
		result.out = data.frame(paste0(mean.out, " (", sd.out, ")"))
		colnames(result.out) = "Mean (sd)"
	}

	if (cont=="median"){
		median.out = sprintf("%.1f", matrix(s[,5]))
		L_IQR = sprintf("%.1f", matrix(s[,4]))
		U_IQR = sprintf("%.1f", matrix(s[,6]))
		result.out = data.frame(paste0(median.out, " (", L_IQR, " to ", U_IQR, ")"))
		colnames(result.out) = "Median (IQR)"
	}

	df.out = cbind(df.out, result.out)
	
	if (total_col){
		total.out = matrix(s[,1])
		df.out = cbind(df.out, "Total" = total.out)
	}

	# Add dependent name label
	if(add_dependent_label){
		df.out = dependent_label(df.out=df.out, .data=.data, dependent,
														 prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}


	return(df.out)
}



#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal
summary_factorlist1 <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){

	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = FALSE,
											 test = TRUE,na.include = na_include, continuous = cont_cut)
	df.out = plyr::ldply(s$stats, function(x){
		if(dim(x)[2] == 13){ #hack to get continuous vs categorical. Wouldn't work for factor with 13 levels
			# Continuous variables
			# Mean (SD)
			if (cont == "mean"){
				a = paste0(round(x[1,12], 1), " (", round(x[1,13], 1), ")")
				row1_name = dimnames(x)[[2]][12]
				row2_name = dimnames(x)[[2]][13]
			}

			# Median (IQR)
			if (cont == "median"){
				a = paste0(x[1,6], " (", x[1,8]-x[1,4], ")")
				row1_name = "Median"
				row2_name = "IQR"}

			col1_name = dimnames(x)[[1]][1]
			df.out = data.frame(paste0(row1_name, " (", row2_name, ")"), a)
			names(df.out) = c("levels", col1_name)
			return(df.out)

		} else {
			# Factor variables
			row_name = dimnames(x)$w
			col1_name = dimnames(x)$g[1]
			col1 = x[,1]
			total = col1
			if (column == FALSE) {
				col1_prop = (col1/apply(x, 1, sum))*100 # row margin
			} else {
				col1_prop = (col1/sum(col1))*100 # column margin
				total_prop = (total/sum(total))*100
			}
			a = paste0(col1, " (", sprintf("%.1f", round(col1_prop, 1)), ")") #sprintf to keep trailing zeros
			if (total_col == FALSE){
				df.out = data.frame(row_name, a)
				names(df.out) = c("levels", col1_name)
			} else if (total_col == TRUE & column == FALSE) {
				df.out = data.frame(row_name, a, total, total)
				names(df.out) = c("levels", col1_name, "Total", "index_total")
			} else if (total_col == TRUE & column == TRUE) {
				df.out = data.frame(row_name, a, paste0(total, " (", sprintf("%.1f", round(total_prop, 1)), ")"), total)
				names(df.out) = c("levels", col1_name, "Total", "index_total")
			}
		}
		return(df.out)
	})
	# Keep original order
	df.out$index = 1:dim(df.out)[1]

	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) sprintf("%.3f",round(x[[1]], 3)))
		names(a) = c(".id", "p")
		df.out = merge(df.out, a, by=".id")
	}

	# Add back in actual labels
	df.labels = data.frame(".id" = names(s$stats), "label" = s$labels)
	df.out.labels = merge(df.out, df.labels, by = ".id")
	if (orderbytotal==FALSE){
		df.out.labels = df.out.labels[order(df.out.labels$index),] # reorder columns
	} else {
		df.out.labels = df.out.labels[order(-df.out.labels$index_total),] # reorder columns
	}

	# Reorder columns
	label_index = which(names(df.out.labels) == "label")
	not_label_index = which(names(df.out.labels) != "label")
	df.out.labels = df.out.labels[,c(label_index,not_label_index)]

	# Add glm levels name
	if (fit_id){
		levels = as.character(df.out.labels$levels)
		levels[levels == "Mean (SD)"] = ""
		levels[levels == "Median (IQR)"] = ""
		df.out.labels$fit_id = paste0(df.out.labels$.id, levels)
	}

	# Remove
	if (!fit_id) {
		index_index = which(names(df.out.labels) == "index")
	}else{
		index_index = 0
	}
	id_index = which(names(df.out.labels) == ".id")
	index_total_index = which(names(df.out.labels) == "index_total")
	df.out.labels = df.out.labels[,-c(id_index, index_total_index, index_index)]

	# Remove duplicate labels
	df.out.labels = rm_duplicate_labels(df.out.labels, na_to_missing = na_to_missing)

	# Add dependent name label
	if(add_dependent_label){
		df.out.labels = dependent_label(df.out=df.out.labels, .data=.data, dependent,
																		prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}

	return(df.out.labels)
}




#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist2 <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){

	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = FALSE,
											 test = TRUE, na.include = na_include, continuous = cont_cut)
	df.out = plyr::ldply(s$stats, function(x){
		if(dim(x)[2] == 13){ #hack to get continuous vs categorical. Wouldn't work for factor with 13 levels
			# Continuous variables
			# Mean (SD)
			if (cont == "mean"){
				a = paste0(round(x[1,12], 1), " (", round(x[1,13], 1), ")")
				b = paste0(round(x[2,12], 1), " (", round(x[2,13], 1), ")")
				row1_name = dimnames(x)[[2]][12]
				row2_name = dimnames(x)[[2]][13]
			}

			# Median (IQR)
			if (cont == "median"){
				a = paste0(x[1,6], " (", x[1,8]-x[1,4], ")")
				b = paste0(x[2,6], " (", x[2,8]-x[2,4], ")")
				row1_name = "Median"
				row2_name = "IQR"}

			col1_name = dimnames(x)[[1]][1]
			col2_name = dimnames(x)[[1]][2]
			df.out = data.frame(paste0(row1_name, " (", row2_name, ")"), a, b)
			names(df.out) = c("levels", col1_name, col2_name)
			return(df.out)
		} else {
			# Factor variables
			row_name = dimnames(x)$w
			col1_name = dimnames(x)$g[1]
			col2_name = dimnames(x)$g[2]
			col1 = x[,1]
			col2 = x[,2]
			total = col1 + col2
			if (column == FALSE) {
				col1_prop = (col1/apply(x, 1, sum))*100 # row margin
				col2_prop = (col2/apply(x, 1, sum))*100
			} else {
				col1_prop = (col1/sum(col1))*100 # column margin
				col2_prop = (col2/sum(col2))*100
				total_prop = (total/sum(total))*100
			}
			a = paste0(col1, " (", sprintf("%.1f", round(col1_prop, 1)), ")") #sprintf to keep trailing zeros
			b = paste0(col2, " (", sprintf("%.1f", round(col2_prop, 1)), ")")
			if (total_col == FALSE){
				df.out = data.frame(row_name, a, b)
				names(df.out) = c("levels", col1_name, col2_name)
			} else if (total_col == TRUE & column == FALSE) {
				df.out = data.frame(row_name, a, b, total, total)
				names(df.out) = c("levels", col1_name, col2_name, "Total", "index_total")
			} else if (total_col == TRUE & column == TRUE) {
				df.out = data.frame(row_name, a, b, paste0(total, " (", sprintf("%.1f", round(total_prop, 1)), ")"), total)
				names(df.out) = c("levels", col1_name, col2_name, "Total", "index_total")
			}
		}
		return(df.out)
	})
	# Keep original order
	df.out$index = 1:dim(df.out)[1]

	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) sprintf("%.3f",round(x[[1]], 3)))
		names(a) = c(".id", "p")
		df.out = merge(df.out, a, by=".id")
	}

	# Add back in actual labels
	df.labels = data.frame(".id" = names(s$stats), "label" = s$labels)
	df.out.labels = merge(df.out, df.labels, by = ".id")
	if (orderbytotal==FALSE){
		df.out.labels = df.out.labels[order(df.out.labels$index),] # reorder columns
	} else {
		df.out.labels = df.out.labels[order(-df.out.labels$index_total),] # reorder columns
	}

	# Reorder columns
	label_index = which(names(df.out.labels) == "label")
	not_label_index = which(names(df.out.labels) != "label")
	df.out.labels = df.out.labels[,c(label_index,not_label_index)]

	# Add glm levels name
	if (fit_id){
		levels = as.character(df.out.labels$levels)
		levels[levels == "Mean (SD)"] = ""
		levels[levels == "Median (IQR)"] = ""
		df.out.labels$fit_id = paste0(df.out.labels$.id, levels)
	}

	# Remove
	if (fit_id == FALSE) {
		index_index = which(names(df.out.labels) == "index")
	}else{
		index_index = 0
	}
	id_index = which(names(df.out.labels) == ".id")
	index_total_index = which(names(df.out.labels) == "index_total")
	df.out.labels = df.out.labels[,-c(id_index, index_total_index, index_index)]

	# Remove duplicate labels
	df.out.labels = rm_duplicate_labels(df.out.labels, na_to_missing = na_to_missing)

	# Add dependent name label
	if(add_dependent_label){
		df.out.labels = dependent_label(df.out=df.out.labels, .data=.data, dependent,
																		prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}

	return(df.out.labels)
}


#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist3 <- function(.data, dependent, explanatory, cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){
	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = FALSE,
											 test = TRUE,na.include = na_include, continuous = cont_cut)
	# Column vs row proportions for factor variables
	df.out = plyr::ldply(s$stats, function(x){
		if(dim(x)[2] == 13){ #hack to get continuous vs categorical. Wouldn't work for factor with 13 levels
			# Continuous variables
			# Mean (SD)
			if (cont == "mean"){
				a = paste0(round(x[1,12], 1), " (", round(x[1,13], 1), ")")
				b = paste0(round(x[2,12], 1), " (", round(x[2,13], 1), ")")
				c = paste0(round(x[3,12], 1), " (", round(x[3,13], 1), ")")
				row1_name = dimnames(x)[[2]][12]
				row2_name = dimnames(x)[[2]][13]}

			# Median (IQR)
			if (cont == "median"){
				a = paste0(x[1,6], " (", x[1,8]-x[1,4], ")")
				b = paste0(x[2,6], " (", x[2,8]-x[2,4], ")")
				c = paste0(x[3,6], " (", x[3,8]-x[3,4], ")")
				row1_name = "Median"
				row2_name = "IQR"}

			col1_name = dimnames(x)[[1]][1]
			col2_name = dimnames(x)[[1]][2]
			col3_name = dimnames(x)[[1]][3]
			df.out = data.frame(paste0(row1_name, " (", row2_name, ")"), a, b, c)
			names(df.out) = c("levels", col1_name, col2_name, col3_name)
		} else {
			# Factor variables
			row_name = dimnames(x)$w
			col1_name = dimnames(x)$g[1]
			col2_name = dimnames(x)$g[2]
			col3_name = dimnames(x)$g[3]
			col1 = x[,1]
			col2 = x[,2]
			col3 = x[,3]
			total = col1 + col2 + col3
			if (column == FALSE) {
				col1_prop = (col1/apply(x, 1, sum))*100 # row margin
				col2_prop = (col2/apply(x, 1, sum))*100
				col3_prop = (col3/apply(x, 1, sum))*100
			} else {
				col1_prop = (col1/sum(col1))*100 # column margin
				col2_prop = (col2/sum(col2))*100
				col3_prop = (col3/sum(col3))*100
				total_prop = (total/sum(total))*100
			}
			a = paste0(col1, " (", sprintf("%.1f", round(col1_prop, 1)), ")") #sprintf to keep trailing zeros
			b = paste0(col2, " (", sprintf("%.1f", round(col2_prop, 1)), ")")
			c = paste0(col3, " (", sprintf("%.1f", round(col3_prop, 1)), ")")
			if (total_col == FALSE){
				df.out = data.frame(row_name, a, b, c)
				names(df.out) = c("levels", col1_name, col2_name, col3_name)
			} else if (total_col == TRUE & column == FALSE) {
				df.out = data.frame(row_name, a, b, c, total, total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, "Total", "index_total")
			} else if (total_col == TRUE & column == TRUE) {
				df.out = data.frame(row_name, a, b, c, paste0(total, " (", sprintf("%.1f", round(total_prop, 1)), ")"), total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, "Total", "index_total")
			}
		}
		return(df.out)
	})
	# Keep original order
	df.out$index = 1:dim(df.out)[1]

	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) sprintf("%.3f",round(x[[1]], 3)))
		names(a) = c(".id", "p")
		df.out = merge(df.out, a, by=".id")
	}

	# Add back in actual labels
	df.labels = data.frame(".id" = names(s$stats), "label" = s$labels)
	df.out.labels = merge(df.out, df.labels, by = ".id")
	if (orderbytotal==FALSE){
		df.out.labels = df.out.labels[order(df.out.labels$index),] # reorder columns
	} else {
		df.out.labels = df.out.labels[order(-df.out.labels$index_total),] # reorder columns
	}

	# Reorder columns and remove unnecessary columns
	# Reorder
	label_index = which(names(df.out.labels) == "label")
	not_label_index = which(names(df.out.labels) != "label")
	df.out.labels = df.out.labels[,c(label_index,not_label_index)]

	# Remove
	id_index = which(names(df.out.labels) == ".id")
	index_index = which(names(df.out.labels) == "index")
	index_total_index = which(names(df.out.labels) == "index_total")
	df.out.labels = df.out.labels[,-c(id_index, index_index, index_total_index)]

	# Remove duplicate labels
	df.out.labels = rm_duplicate_labels(df.out.labels, na_to_missing = na_to_missing)

	# Add dependent name label
	if(add_dependent_label){
		df.out.labels = dependent_label(df.out=df.out.labels, .data=.data, dependent,
																		prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}


	return(df.out.labels)
}



#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist4 <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){
	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = FALSE,
											 test = TRUE,na.include = na_include, continuous = cont_cut)
	# Column vs row proportions for factor variables
	df.out = plyr::ldply(s$stats, function(x){
		if(dim(x)[2] == 13){ #hack to get continuous vs categorical. Wouldn't work for factor with 13 levels
			# Continuous variables
			# Mean (SD)
			if (cont == "mean"){
				a = paste0(round(x[1,12], 1), " (", round(x[1,13], 1), ")")
				b = paste0(round(x[2,12], 1), " (", round(x[2,13], 1), ")")
				c = paste0(round(x[3,12], 1), " (", round(x[3,13], 1), ")")
				d = paste0(round(x[4,12], 1), " (", round(x[4,13], 1), ")")
				row1_name = dimnames(x)[[2]][12]
				row2_name = dimnames(x)[[2]][13]
			}

			# Median (IQR)
			if (cont == "median"){
				a = paste0(x[1,6], " (", x[1,8]-x[1,4], ")")
				b = paste0(x[2,6], " (", x[2,8]-x[2,4], ")")
				c = paste0(x[3,6], " (", x[3,8]-x[3,4], ")")
				d = paste0(x[4,6], " (", x[4,8]-x[4,4], ")")
				row1_name = "Median"
				row2_name = "IQR"}

			col1_name = dimnames(x)[[1]][1]
			col2_name = dimnames(x)[[1]][2]
			col3_name = dimnames(x)[[1]][3]
			col4_name = dimnames(x)[[1]][4]
			df.out = data.frame(paste0(row1_name, " (", row2_name, ")"), a, b, c, d)
			names(df.out) = c("levels", col1_name, col2_name, col3_name,col4_name)
		} else {
			# Factor variables
			row_name = dimnames(x)$w
			col1_name = dimnames(x)$g[1]
			col2_name = dimnames(x)$g[2]
			col3_name = dimnames(x)$g[3]
			col4_name = dimnames(x)$g[4]
			col1 = x[,1]
			col2 = x[,2]
			col3 = x[,3]
			col4 = x[,4]
			total = col1 + col2 + col3 + col4
			if (column == FALSE) {
				col1_prop = (col1/apply(x, 1, sum))*100 # row margin
				col2_prop = (col2/apply(x, 1, sum))*100
				col3_prop = (col3/apply(x, 1, sum))*100
				col4_prop = (col4/apply(x, 1, sum))*100
			} else {
				col1_prop = (col1/sum(col1))*100 # column margin
				col2_prop = (col2/sum(col2))*100
				col3_prop = (col3/sum(col3))*100
				col4_prop = (col4/sum(col4))*100
				total_prop = (total/sum(total))*100
			}
			a = paste0(col1, " (", sprintf("%.1f", round(col1_prop, 1)), ")") #sprintf to keep trailing zeros
			b = paste0(col2, " (", sprintf("%.1f", round(col2_prop, 1)), ")")
			c = paste0(col3, " (", sprintf("%.1f", round(col3_prop, 1)), ")")
			d = paste0(col4, " (", sprintf("%.1f", round(col4_prop, 1)), ")")
			if (total_col == FALSE){
				df.out = data.frame(row_name, a, b, c, d)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name)
			} else if (total_col == TRUE & column == FALSE) {
				df.out = data.frame(row_name, a, b, c, d, total, total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name, "Total", "index_total")
			} else if (total_col == TRUE & column == TRUE) {
				df.out = data.frame(row_name, a, b, c, d, paste0(total, " (", sprintf("%.1f", round(total_prop, 1)), ")"), total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name, "Total", "index_total")
			}
		}
		return(df.out)
	})
	# Keep original order
	df.out$index = 1:dim(df.out)[1]

	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) sprintf("%.3f",round(x[[1]], 3)))
		names(a) = c(".id", "p")
		df.out = merge(df.out, a, by=".id")
	}

	# Add back in actual labels
	df.labels = data.frame(".id" = names(s$stats), "label" = s$labels)
	df.out.labels = merge(df.out, df.labels, by = ".id")
	if (orderbytotal==FALSE){
		df.out.labels = df.out.labels[order(df.out.labels$index),] # reorder columns
	} else {
		df.out.labels = df.out.labels[order(-df.out.labels$index_total),] # reorder columns
	}

	# Reorder columns and remove unnecessary columns
	# Reorder
	label_index = which(names(df.out.labels) == "label")
	not_label_index = which(names(df.out.labels) != "label")
	df.out.labels = df.out.labels[,c(label_index,not_label_index)]

	# Remove
	id_index = which(names(df.out.labels) == ".id")
	index_index = which(names(df.out.labels) == "index")
	index_total_index = which(names(df.out.labels) == "index_total")
	df.out.labels = df.out.labels[,-c(id_index, index_index, index_total_index)]

	# Remove duplicate labels
	df.out.labels = rm_duplicate_labels(df.out.labels, na_to_missing = na_to_missing)

	# Add dependent name label
	if(add_dependent_label){
		df.out.labels = dependent_label(df.out=df.out.labels, .data=.data, dependent,
																		prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}


	return(df.out.labels)
}


#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist5 <- function(.data, dependent, explanatory, cont = "mean", cont_cut = 5, p = FALSE, na_include = FALSE,
																column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																na_to_missing = TRUE, add_dependent_label = FALSE,
																dependent_label_prefix = "Dependent: ", dependent_label_suffix = ""){
	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = FALSE,
											 test = TRUE,na.include = na_include, continuous = cont_cut)
	# Column vs row proportions for factor variables
	df.out = plyr::ldply(s$stats, function(x){
		if(dim(x)[2] == 13){ #hack to get continuous vs categorical. Wouldn't work for factor with 13 levels
			# Continuous variables
			# Mean (SD)
			if (cont == "mean"){
				a = paste0(round(x[1,12], 1), " (", round(x[1,13], 1), ")")
				b = paste0(round(x[2,12], 1), " (", round(x[2,13], 1), ")")
				c = paste0(round(x[3,12], 1), " (", round(x[3,13], 1), ")")
				d = paste0(round(x[4,12], 1), " (", round(x[4,13], 1), ")")
				e = paste0(round(x[5,12], 1), " (", round(x[5,13], 1), ")")
				row1_name = dimnames(x)[[2]][12]
				row2_name = dimnames(x)[[2]][13]
			}

			# Median (IQR)
			if (cont == "median"){
				a = paste0(x[1,6], " (", x[1,8]-x[1,4], ")")
				b = paste0(x[2,6], " (", x[2,8]-x[2,4], ")")
				c = paste0(x[3,6], " (", x[3,8]-x[3,4], ")")
				d = paste0(x[4,6], " (", x[4,8]-x[4,4], ")")
				e = paste0(x[5,6], " (", x[5,8]-x[5,4], ")")
				row1_name = "Median"
				row2_name = "IQR"}

			col1_name = dimnames(x)[[1]][1]
			col2_name = dimnames(x)[[1]][2]
			col3_name = dimnames(x)[[1]][3]
			col4_name = dimnames(x)[[1]][4]
			col5_name = dimnames(x)[[1]][5]
			df.out = data.frame(paste0(row1_name, " (", row2_name, ")"), a, b, c, d, e)
			names(df.out) = c("levels", col1_name, col2_name, col3_name,col4_name, col5_name)
		} else {
			# Factor variables
			row_name = dimnames(x)$w
			col1_name = dimnames(x)$g[1]
			col2_name = dimnames(x)$g[2]
			col3_name = dimnames(x)$g[3]
			col4_name = dimnames(x)$g[4]
			col5_name = dimnames(x)$g[5]
			col1 = x[,1]
			col2 = x[,2]
			col3 = x[,3]
			col4 = x[,4]
			col5 = x[,5]
			total = col1 + col2 + col3 + col4 + col5
			if (column == FALSE) {
				col1_prop = (col1/apply(x, 1, sum))*100 # row margin
				col2_prop = (col2/apply(x, 1, sum))*100
				col3_prop = (col3/apply(x, 1, sum))*100
				col4_prop = (col4/apply(x, 1, sum))*100
				col5_prop = (col5/apply(x, 1, sum))*100
			} else {
				col1_prop = (col1/sum(col1))*100 # column margin
				col2_prop = (col2/sum(col2))*100
				col3_prop = (col3/sum(col3))*100
				col4_prop = (col4/sum(col4))*100
				col5_prop = (col5/sum(col5))*100
				total_prop = (total/sum(total))*100
			}
			a = paste0(col1, " (", sprintf("%.1f", round(col1_prop, 1)), ")") #sprintf to keep trailing zeros
			b = paste0(col2, " (", sprintf("%.1f", round(col2_prop, 1)), ")")
			c = paste0(col3, " (", sprintf("%.1f", round(col3_prop, 1)), ")")
			d = paste0(col4, " (", sprintf("%.1f", round(col4_prop, 1)), ")")
			e = paste0(col5, " (", sprintf("%.1f", round(col5_prop, 1)), ")")
			if (total_col == FALSE){
				df.out = data.frame(row_name, a, b, c, d, e)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name, col5_name)
			} else if (total_col == TRUE & column == FALSE) {
				df.out = data.frame(row_name, a, b, c, d, e, total, total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name, col5_name, "Total", "index_total")
			} else if (total_col == TRUE & column == TRUE) {
				df.out = data.frame(row_name, a, b, c, d, e, paste0(total, " (", sprintf("%.1f", round(total_prop, 1)), ")"), total)
				names(df.out) = c("levels", col1_name, col2_name, col3_name, col4_name, col5_name, "Total", "index_total")
			}
		}
		return(df.out)
	})
	# Keep original order
	df.out$index = 1:dim(df.out)[1]

	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) sprintf("%.3f",round(x[[1]], 3)))
		names(a) = c(".id", "p")
		df.out = merge(df.out, a, by=".id")
	}

	# Add back in actual labels
	df.labels = data.frame(".id" = names(s$stats), "label" = s$labels)
	df.out.labels = merge(df.out, df.labels, by = ".id")
	if (orderbytotal==FALSE){
		df.out.labels = df.out.labels[order(df.out.labels$index),] # reorder columns
	} else {
		df.out.labels = df.out.labels[order(-df.out.labels$index_total),] # reorder columns
	}

	# Reorder columns and remove unnecessary columns
	# Reorder
	label_index = which(names(df.out.labels) == "label")
	not_label_index = which(names(df.out.labels) != "label")
	df.out.labels = df.out.labels[,c(label_index,not_label_index)]

	# Remove
	id_index = which(names(df.out.labels) == ".id")
	index_index = which(names(df.out.labels) == "index")
	index_total_index = which(names(df.out.labels) == "index_total")
	df.out.labels = df.out.labels[,-c(id_index, index_index, index_total_index)]

	# Remove duplicate labels
	df.out.labels = rm_duplicate_labels(df.out.labels, na_to_missing = na_to_missing)

	# Add dependent name label
	if(add_dependent_label){
		df.out.labels = dependent_label(df.out=df.out.labels, .data=.data, dependent,
																		prefix=dependent_label_prefix, suffix = dependent_label_suffix)
	}


	return(df.out.labels)
}
