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
#' @param dependent Character vector of length 1:  name of dependent variable (2
#'   to 5 factor levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param cont Summary for continuous variables: "mean" (standard deviation) or
#'   "median" (interquartile range).
#' @param cont_cut Numeric: number of unique values in continuous variable at
#'   which to consider it a factor.
#' @param p Logical: Include statistical test (see
#'   \code{\link[Hmisc]{summary.formula}}).
#' @param digits Number of digits to round to (1) mean/median, (2) standard
#'   deviation / interquartile range, (3) p-value, (4) count percentage.
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
#'   left of table.
#' @param dependent_label_prefix Add text before dependent label.
#' @param dependent_label_suffix Add text after dependent label.
#' @param ... Pass other arguments to \code{\link[Hmisc]{summary.formula}}),
#'   e.g. \code{catTest = catTestfisher}.
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
															 p = FALSE, digits = c(1, 1, 3, 1), na_include = FALSE,
															 column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
															 na_to_missing = TRUE, add_dependent_label = FALSE,
															 dependent_label_prefix = "Dependent: ", dependent_label_suffix = "", ...) {
	if(!is.data.frame(.data)) stop(".data is not dataframe")
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data) # tbl work different, convert to data.frame
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)){
		message("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		.data$all = factor(1, labels="all")
	}
	if(cont == "geometric") { # For requested geometric mean function, log continuous variables for ease below
		log_vars = sapply(.data, is.numeric) & names(.data) %in% explanatory
		col_zeros = sapply(.data[log_vars], function(x) any(x == 0))
		col_names = names(which(col_zeros))
		if(any(col_zeros)){
			stop(paste0("Geometric mean called when variable(s) `", paste0(col_names, collapse = "` `"), "` contain(s) zero values"))
		}
		.data = .data %>% dplyr::mutate_if(log_vars, log)
	}
	
	args = list(.data = .data, dependent = dependent, explanatory = explanatory,
							cont = cont, cont_cut = cont_cut, p = p, digits = digits, 
							na_include = na_include,
							column = column, total_col = total_col, orderbytotal = orderbytotal, fit_id = fit_id,
							na_to_missing = na_to_missing, add_dependent_label = add_dependent_label,
							dependent_label_prefix = dependent_label_prefix,
							dependent_label_suffix = dependent_label_suffix, ...)
	
	# Survival object
	d_is.surv = grepl("Surv[(].*[)]", dependent)
	
	if(d_is.surv){
		message("Dependent variable is a survival object")
		.data$all = factor(1, labels="all")
		args$.data = .data
		args$dependent = "all"
		
		# Remove strata and cluster terms
		drop = grepl("cluster[(].*[)]", explanatory) |
			grepl("strata[(].*[)]", explanatory) |
			grepl("frailty[(].*[)]", explanatory)
		args$explanatory = args$explanatory[!drop]
		
		suppressWarnings(
			do.call(summary_factorlist_groups, args)
		)
	} else {
		
		# Extract dependent variable
		d_variable = .data[ ,names(.data) %in% dependent]
		
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
			message("Dependent is not a factor and will be treated as a continuous variable")
			do.call(summary_factorlist0, args)
		} else {
			do.call(summary_factorlist_groups, args)
		}
	}
}


#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal

summary_factorlist0 <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, p = FALSE, 
																digits = c(1, 1, 3, 1),
																na_include = FALSE,
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
	df.out = data.frame(label=attr(s, "vlabel"), levels=attr(s, "dimnames")[[1]],
											stringsAsFactors = FALSE)
	
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
		mean.out = round_tidy(matrix(s[,2]), digits[1])
		sd.out = round_tidy(matrix(s[,3]), digits[2])
		result.out = data.frame(paste0(mean.out, " (", sd.out, ")"), 
														stringsAsFactors = FALSE)
		colnames(result.out) = "Mean (sd)"
	}
	
	if (cont=="median"){
		median.out = round_tidy(matrix(s[,5]), digits[1])
		L_IQR = round_tidy(matrix(s[,4]), digits[2])
		U_IQR = round_tidy(matrix(s[,6]), digits[2])
		result.out = data.frame(paste0(median.out, " (", L_IQR, " to ", U_IQR, ")"), 
														stringsAsFactors = FALSE)
		colnames(result.out) = "Median (IQR)"
	}
	
	if (cont=="geometric"){
		mean.out = round_tidy(exp(matrix(s[,2])), digits[1])
		sd.out = round_tidy(exp(matrix(s[,3])), digits[2])
		result.out = data.frame(paste0(mean.out, " (", sd.out, ")"), 
														stringsAsFactors = FALSE)
		colnames(result.out) = "Geometric mean (sd)"
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
summary_factorlist_groups <- function(.data, dependent, explanatory,  cont = "mean", cont_cut = 5, 
																			p = FALSE, digits = c(1, 1, 3, 1), na_include = FALSE,
																			column = FALSE, total_col = FALSE, orderbytotal = FALSE, fit_id = FALSE,
																			na_to_missing = TRUE, add_dependent_label = FALSE,
																			dependent_label_prefix = "Dependent: ", dependent_label_suffix = "", ...){
	
	s <- summary_formula(as.formula(paste(dependent, "~", paste(explanatory, collapse = "+"))), data = .data,
											 method = "reverse", overall = TRUE,
											 test = TRUE, na.include = na_include, continuous = cont_cut, ...)
	df.out = plyr::ldply(1:length(s$stats), function(index) {
		x = s$stats[[index]]
		is_continuous = s$type[index] == 2
		
		if (is_continuous) {
			df.out = summarise_continuous(x, cont = cont, total_col = total_col, digits = digits)
		} else {
			# Factor variables
			df.out = summarise_categorical(x, column = column, total_col = total_col, digits = digits)
		}
		df.out[[".id"]] = names(s$stats)[index]
		
		return(df.out)
	}, .id = NULL)
	
	# Keep original order
	df.out$index = 1:dim(df.out)[1]
	
	if (p == TRUE){
		a = plyr::ldply(s$testresults, function(x) round_tidy(x[[1]], digits[3]))
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


#' Helper function to generate the summary for a continuous variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal
summarise_continuous = function(x, cont, total_col, digits) {
	if (cont == "mean") {
		df_out = x %>%
			as.data.frame() %>%
			dplyr::mutate(
				Label = rownames(.),
				Formatted = paste0(round_tidy(Mean, digits[1]), " (",
													 round_tidy(SD, digits[2]), ")"),
				levels = "Mean (SD)"
			)
	} else if (cont == "median") {
		df_out = x %>%
			as.data.frame() %>%
			dplyr::rename(Median = 6,
										Q3 = 8,
										Q1 = 4) %>%
			dplyr::mutate(
				Label = rownames(.),
				IQR = Q3 - Q1,
				Formatted = paste0(round_tidy(Median, digits[1]), " (",
													 round_tidy(IQR, digits[2]), ")"),
				levels = "Median (IQR)"
			)
	}	else if (cont == "geometric") {
		df_out = x %>%
			as.data.frame() %>%
			dplyr::mutate(
				Label = rownames(.),
				Formatted = paste0(round_tidy(exp(Mean), digits[1]), " (",
													 round_tidy(exp(SD), digits[2]), ")"),
				levels = "Geometric mean (SD)"
			)
	}
	df_out = df_out %>%
		dplyr::select(levels, Label, Formatted) %>%
		tidyr::spread(Label, Formatted) %>%
		dplyr::select(-Combined, Total = Combined)
	if(total_col){
		return(df_out)
	} else {
		df_out = df_out %>% 
			dplyr::select(-Total)
		return(df_out)
	}
}

#' Helper function to generate the summary for a categorical variable
#'
#' Internal function, not called directly.
#'
#' @keywords internal
summarise_categorical = function(x, column, total_col, digits) {
	# Calculate totals
	df = x %>%
		as.data.frame() %>%
		tibble::rownames_to_column("w") %>%
		dplyr::mutate(w = factor(w, levels = w)) %>% # Needed to keep order
		tidyr::gather(g, Freq, -w, -Combined) %>% 
		dplyr::mutate(g = factor(g, levels = unique(g))) %>% 
		dplyr::rename(Total = Combined) %>% 
		dplyr::mutate(index_total = Total) %>% 
		dplyr::group_by(g) %>%
		dplyr::mutate(total_prop = Total / sum(Total) * 100)
	# Calculate percentage: row-wise or column-wise
	if (column) {
		df = df %>%
			dplyr::group_by(g) %>%
			dplyr::mutate(Prop = Freq / sum(Freq) * 100,
										Total = format_n_percent(Total, total_prop, digits[4]))
	} else {
		df = df %>%
			dplyr::group_by(w) %>%
			dplyr::mutate(Prop = Freq / sum(Freq) * 100)
	}
	
	# Finalize and reshape
	df = df %>%
		dplyr::ungroup() %>%
		dplyr::mutate(Formatted = format_n_percent(Freq, Prop, digits[4])) %>%
		dplyr::select(levels = w, g, Formatted, Total, index_total) %>%
		tidyr::spread(g, Formatted)
	
	# Drop totals if not required
	if (total_col) {
		df = df %>%
			# dplyr trick to move column to the end
			dplyr::select(-Total, -index_total, Total, index_total)
	} else {
		df = df %>%
			dplyr::select(-Total, -index_total)
	}
	
	return(df)
}
