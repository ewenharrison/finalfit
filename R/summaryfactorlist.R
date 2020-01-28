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
#' @param cont Summary for continuous explanatory variables: "mean" (standard
#'   deviation) or "median" (interquartile range). If "median" then
#'   non-parametric hypothesis test performed (see below).
#' @param cont_nonpara Numeric vector of form e.g. \code{c(1,2)}. Specify which
#'   variables to perform non-parametric hypothesis tests on and summarise with
#'   "median".
#' @param cont_cut  Numeric: number of unique values in continuous variable at
#'   which to consider it a factor.
#' @param cont_range Logical. Median is show with 1st and 3rd quartiles.
#' @param p Logical: Include null hypothesis statistical test.
#' @param p_cont_para Character. Continuous variable parametric test. One of
#'   either "aov" (analysis of variance) or "t.test" for Welch two sample
#'   t-test. Note continuous non-parametric test is always Kruskal Wallis
#'   (kruskal.test) which in two-group setting is equivalent to Mann-Whitney U
#'   /Wilcoxon rank sum test.
#' @param p_cat Character. Categorical variable test. One of either "chisq" or
#'   "fisher".
#' @param column Logical: Compute margins by column rather than row.
#' @param total_col Logical: include a total column summing across factor
#'   levels.
#' @param orderbytotal Logical: order final table by total column high to low.
#' @param digits Number of digits to round to (1) mean/median, (2) standard
#'   deviation / interquartile range, (3) p-value, (4) count percentage.
#' @param na_include Logical: include missing data in summary (\code{NA}).
#' @param na_to_p Logical: include missing as group in statistical test.
#' @param fit_id Logical: allows merging via \code{\link{finalfit_merge}}.
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table.
#' @param dependent_label_prefix Add text before dependent label.
#' @param dependent_label_suffix Add text after dependent label.
#' @param add_col_totals Logical. Include column total n.
#' @param include_col_totals_percent Include column percentage of total.
#' @param col_totals_rowname Logical. Row name for column totals.
#' @param col_totals_prefix Character. Prefix to column totals, e.g. "N=".
#' @param add_row_totals Logical. Include row totals. Note this differs from
#'   \code{total_col} above particularly for continuous explanatory variables.
#' @param include_row_missing_col Logical. Include missing data total for each row.  
#' @param row_totals_colname Character. Column name for row totals. 
#' @param row_missing_colname Character. Column name for missing data totals for each row. 
#'
#' @return Returns a \code{factorlist} dataframe.
#'
#' @family finalfit wrappers
#' @seealso \code{\link{fit2df}} \code{\link{ff_column_totals}}
#'   \code{\link{ff_row_totals}} \code{\link{ff_label}} \code{\link{ff_glimpse}}
#'   \code{\link{ff_percent_only}}
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
summary_factorlist <- function(.data, 
															 dependent = NULL, explanatory, 
															 cont = "mean", cont_nonpara = NULL, cont_cut = 5, cont_range = FALSE, 
															 p = FALSE, p_cont_para = "aov", p_cat = "chisq",
															 column = TRUE, total_col = FALSE, orderbytotal = FALSE,
															 digits = c(1, 1, 3, 1), 
															 na_include = FALSE, na_to_p = FALSE,
															 fit_id = FALSE,
															 add_dependent_label = FALSE,  
															 dependent_label_prefix = "Dependent: ", dependent_label_suffix = "",
															 add_col_totals = FALSE, include_col_totals_percent = TRUE,
															 col_totals_rowname = NULL, col_totals_prefix = "",
															 add_row_totals = FALSE, include_row_missing_col = TRUE,
															 row_totals_colname = "Total N", row_missing_colname = "Missing N"){
	
	
	# Warnings/Checks --------------
	if(!is.data.frame(.data)) stop(".data is not dataframe")
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data)
	if(is.null(explanatory)) stop("No explanatory variable(s) provided")
	if(is.null(dependent)){
		message("No dependent variable(s) provided; defaulting to single-level factor")
		dependent = "all"
		.data$all = factor(1, labels="all")
	}
	# Extract explanatory terms (to support using * and :)
	explanatory = explanatory %>% 
		paste("~", ., collapse = "+") %>% 
		formula() %>% 
		all.vars()
	
	if(dependent %in% explanatory) stop("Cannot have dependent variable in explanatory list.")
	
	if(!is.null(cont_nonpara) && max(cont_nonpara) > length(explanatory)) {
		stop("cont_nonpara cannot include values greater than the number of explanatory variables")
	}
	
	# Definitions ----------
	## Dependent as survival object handling
	d_is.surv = grepl("Surv[(].*[)]", dependent)
	
	if(d_is.surv){
		message("Dependent variable is a survival object")
		.data$all = factor(1, labels="all")
		dependent = "all"
		
		# Remove strata and cluster terms
		drop = grepl("cluster[(].*[)]", explanatory) |
			grepl("strata[(].*[)]", explanatory) |
			grepl("frailty[(].*[)]", explanatory)
		explanatory = explanatory[!drop]
	}    
	
	## Active dataset
	.data = .data %>% 
		dplyr::select(dependent, explanatory)
	
	## Continous data to categorical if unique values below threshold
	cont_distinct = .data %>%
		dplyr::select(explanatory) %>% 
		dplyr::summarise_if(is.numeric, dplyr::n_distinct) %>% 
		purrr::keep(~ .x < cont_cut) %>% 
		names()
	
	.data = .data %>% 
		dplyr::mutate_at(cont_distinct, as.factor)
	
	## Explanatory variable type
	explanatory_type = .data %>% 
		dplyr::select(explanatory) %>% 
		purrr::map(is.numeric)
	
	# Non-parametric variables
	explanatory_nonpara = vector(length = length(explanatory))
	explanatory_nonpara[cont_nonpara] = TRUE
	if(cont == "median") explanatory_nonpara = TRUE
	
	## Labels
	var_labels = .data %>% 
		dplyr::select(explanatory) %>% 
		extract_variable_label()
	
	## Missing data handling
	if(#!na_include &
		.data %>% 
		dplyr::pull(dependent) %>% 
		is.na() %>% 
		any()) {message("Note: dependent includes missing data. These are dropped.")}
	
	if(na_include){
		# Do we want to include missing in dependent ever? :
		# df.in = .data %>%
		#   dplyr::mutate_if(is.factor, forcats::fct_explicit_na)
		df.in = .data %>% 
			dplyr::mutate_if(names(.) %in% unlist(explanatory) & 
											 	sapply(., is.factor),
											 forcats::fct_explicit_na
			) %>% 
			tidyr::drop_na()
	} else {
		df.in = .data %>% 
			tidyr::drop_na()
	}
	
	## Missing data to p-tests or not 
	if(na_to_p){
		df.p = df.in
	} else {
		df.p = .data
	}
	if(p && na_to_p){
		message("Explanatory variable(s) missing data included in hypothesis test (p-value).")
	}
	
	## Dependent is numeric
	d_is.numeric = .data %>% 
		dplyr::pull(dependent) %>% 
		is.numeric()
	
	# Continous dependent ---------------------------------------------------------------------------------
	if(d_is.numeric){
		
		## Hypothesis tests ---------
		if(p){
			p_tests =  purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
														 ~ if(!..2 && !..3){
														 	df.p %>%
														 		{ if(p_cont_para == "aov"){
														 			summary(aov(as.formula(paste(dependent, "~", ..1)), df.p))[[1]][["Pr(>F)"]][[1]] %>% 
														 				p_tidy(digits[3], "")
														 		} else if (p_cont_para == "t.test"){
														 			t.test(as.formula(paste(dependent, "~", ..1)), df.p)$p.value %>% 
														 				p_tidy(digits[3], "")
														 		}}
														 } else if (!..2 & ..3){
														 	kruskal.test(as.formula(paste(dependent, "~", ..1)), df.p)$p.value %>% 
														 		p_tidy(digits[3], "") 
														 } else if (..2){
														 	"-"
														 }
			)
		}  
		
		summary_cont_name = rep("Mean (sd)", length(explanatory_nonpara))
		if(!is.null(cont_nonpara)) summary_cont_name[explanatory_nonpara] = "Median (IQR)"
		
		## Output table  --------------  
		df.out = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara, summary_cont_name), 
												 ~ if(!..2){
												 	df.in %>% 
												 		dplyr::group_by(!! sym(..1)) %>%  
												 		dplyr::summarise(value_mean = mean(!! sym(dependent), na.rm = TRUE),
												 										 value_sd = sd(!! sym(dependent), na.rm = TRUE),
												 										 value_median = median(!! sym(dependent), na.rm = TRUE),
												 										 value_q1 =quantile(!! sym(dependent), 0.75, na.rm = TRUE),
												 										 value_q3 = quantile(!! sym(dependent), 0.75, na.rm = TRUE),
												 										 Total = dplyr::n()) %>% 
												 		dplyr::mutate(
												 			label = ..1,
												 			unit = ..4,
												 		)  %>% 
												 		dplyr::rename(levels = 1) %>% 
												 		{ if(! ..3){
												 			dplyr::mutate(., 
												 										value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																	 value_sd %>% round_tidy(digits[1]), ")")
												 			)
												 		} else {
												 			{ if(cont_range){
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_q1 %>% round_tidy(digits[1]), " to ",
												 																		 value_q3 %>% round_tidy(digits[1]), ")")
												 				) 
												 			} else {
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 {value_q3 - value_q1} %>% round_tidy(digits[1]), ")")
												 				)
												 			}}
												 			
												 		}} %>% 
												 		{if(total_col){
												 			dplyr::select(., label, levels, unit, value, Total)
												 		} else {
												 			dplyr::select(., label, levels, unit, value)
												 		}}  %>% 
												 		dplyr::mutate_all(as.character)
												 } else if(..2){
												 	df.in %>% 
												 		dplyr::summarise(value_mean = mean(!! sym(dependent), na.rm = TRUE),
												 										 value_sd = sd(!! sym(dependent), na.rm = TRUE),
												 										 value_median = median(!! sym(dependent), na.rm = TRUE),
												 										 value_q1 =quantile(!! sym(dependent), 0.25, na.rm = TRUE),
												 										 value_q3 = quantile(!! sym(dependent), 0.75, na.rm = TRUE),
												 										 value_min = min(!! sym(..1), na.rm = TRUE),
												 										 value_max = max(!! sym(..1), na.rm = TRUE),
												 										 Total = dplyr::n()) %>% 
												 		dplyr::mutate(
												 			label = ..1,
												 			levels = paste0("[", value_min, ",", value_max, "]"),
												 			unit = ..4
												 		) %>% 
												 		{ if(! ..3){
												 			dplyr::mutate(., 
												 										value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																	 value_sd %>% round_tidy(digits[1]), ")")
												 			)
												 		} else {
												 			{ if(cont_range){
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_q1 %>% round_tidy(digits[1]), " to ",
												 																		 value_q3 %>% round_tidy(digits[1]), ")")
												 				) 
												 			} else {
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 {value_q3 - value_q1} %>% round_tidy(digits[1]), ")")
												 				)
												 			}}
												 			
												 		}} %>% 
												 		{if(total_col){
												 			dplyr::select(., label, levels, unit, value, Total)
												 		} else{
												 			dplyr::select(., label, levels, unit, value)
												 		}}  %>% 
												 		dplyr::mutate_all(as.character)
												 }
		)
	} else {
		
		# Categorical dependent -----------------------------------------------------------------------------
		
		## Hypothesis tests ---------
		if(p){
			p_tests =  purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
														 ~ if(!..2){
														 	df.p %>%
														 		{ if(p_cat == "chisq"){
														 			dplyr::summarise(., chisq.test(!! sym(..1), !! sym(dependent))$p.value) %>% 
														 				p_tidy(digits[3], "")
														 		} else if (p_cat == "fisher"){
														 			dplyr::summarise(., fisher.test(!! sym(..1), !! sym(dependent))$p.value) %>% 
														 				p_tidy(digits[3], "")
														 		}}
														 } else if (..2 & !..3){
														 	{if (p_cont_para == "aov"){
														 		summary(aov(as.formula(paste(..1, "~", dependent)), df.p))[[1]][["Pr(>F)"]][[1]] %>% 
														 			p_tidy(digits[3], "")
														 	} else if (p_cont_para == "t.test"){
														 		t.test(as.formula(paste(..1, "~", dependent)), df.p)$p.value %>% 
														 			p_tidy(digits[3], "")
														 	}}
														 } else if (..2 & ..3){
														 	kruskal.test(as.formula(paste(..1, "~", dependent)), df.p)$p.value %>% 
														 		p_tidy(digits[3], "") 
														 }
			)
		}
		
		
		## Output table  --------------  
		df.out = purrr::pmap(list(explanatory, explanatory_type, explanatory_nonpara), 
												 ~ if(!..2){
												 	df.in %>% 
												 		#mutate_if(is.factor, as.character) %>% # To remove bind factor warnings
												 		dplyr::group_by(!! sym(dependent)) %>% 
												 		dplyr::count(!! sym(..1)) %>% 
												 		dplyr::ungroup() %>% 
												 		dplyr::mutate(grand_total = sum(n)) %>% 
												 		dplyr::group_by_at(2) %>% 
												 		dplyr::mutate(row_total = sum(n),
												 									col_total_prop = 100 * row_total / grand_total) %>% 
												 		{ if(column) {
												 			dplyr::group_by(., !! sym(dependent)) %>% 
												 				dplyr::mutate(
												 					col_total = sum(n),
												 					prop = 100 * n / col_total,
												 					Total = format_n_percent(row_total, col_total_prop, digits[[4]])
												 				) %>% 
												 				dplyr::select(-col_total)
												 		} else { 
												 			dplyr::group_by_at(., 2) %>% 
												 				dplyr::mutate(
												 					prop = 100 * n / row_total,
												 					Total = paste0(row_total, " (100)")
												 				)
												 		}
												 		} %>% 
												 		dplyr::ungroup() %>% 
												 		dplyr::mutate(
												 			value = format_n_percent(n, prop, digits[4])
												 		) %>%
												 		dplyr::select(-prop, -n, -grand_total, -col_total_prop) %>% 
												 		tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 		dplyr::mutate(
												 			label = names(.)[1]
												 		) %>%
												 		dplyr::rename(levels = 1) %>% 
												 		{if(orderbytotal){
												 			dplyr::arrange(., -row_total) 
												 		} else {
												 			.
												 		}} %>% 
												 		dplyr::select(-row_total) %>%     
												 		dplyr::select(label, levels, dplyr::everything()) %>% 
												 		dplyr::select(-Total, dplyr::everything()) %>% 
												 		dplyr::mutate_all(as.character) %>% 
												 		# Total column
												 		{ if(total_col){
												 			.
												 		} else {
												 			dplyr::select(., -Total)
												 		}
												 		}
												 } else {
												 	df.in %>% 
												 		dplyr::mutate(
												 			value_mean_total = mean(!! sym(..1), na.rm = TRUE),
												 			value_sd_total = sd(!! sym(..1), na.rm = TRUE),
												 			value_median_total = median(!! sym(..1), na.rm = TRUE),
												 			value_q1_total = quantile(!! sym(..1), 0.25, na.rm = TRUE),
												 			value_q3_total = quantile(!! sym(..1), 0.75, na.rm = TRUE)
												 		) %>%
												 		dplyr::group_by(!! sym(dependent)) %>% 
												 		dplyr::summarise(
												 			value_mean = mean(!! sym(..1), na.rm = TRUE),
												 			value_sd = sd(!! sym(..1), na.rm = TRUE),
												 			value_median = median(!! sym(..1), na.rm = TRUE),
												 			value_q1 = quantile(!! sym(..1), 0.25, na.rm = TRUE),
												 			value_q3 = quantile(!! sym(..1), 0.75, na.rm = TRUE),
												 			value_iqr = value_q3 - value_q1,
												 			value_mean_total = unique(value_mean_total),
												 			value_sd_total = unique(value_sd_total),
												 			value_median_total = unique(value_median_total),
												 			value_q1_total = unique(value_q1_total),
												 			value_q3_total = unique(value_q3_total),
												 			value_iqr_total = value_q3_total - value_q1_total,
												 			
												 		) %>% 
												 		{ if(! ..3) {
												 			dplyr::mutate(., 
												 										value = paste0(value_mean %>% round_tidy(digits[1]), " (", 
												 																	 value_sd %>%  round_tidy(digits[2]), ")") ,
												 										Total = paste0(value_mean_total %>% round_tidy(digits[1]), " (", 
												 																	 value_sd_total %>%  round_tidy(digits[2]), ")") 
												 			) %>% 
												 				dplyr::select(dependent, value, Total) %>% 
												 				tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 				dplyr::mutate(
												 					label = .x,
												 					levels = "Mean (SD)"
												 				) 
												 		} else if (..3){
												 			{if(cont_range){
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_q1 %>% round_tidy(digits[2]), " to ",
												 																		 value_q3 %>% round_tidy(digits[2]), ")"), 
												 											Total = paste0(value_median_total %>% round_tidy(digits[1]), " (", 
												 																		 value_q1_total %>% round_tidy(digits[2]), " to ",
												 																		 value_q3_total %>% round_tidy(digits[2]), ")") 
												 				)
												 			} else {
												 				dplyr::mutate(., 
												 											value = paste0(value_median %>% round_tidy(digits[1]), " (", 
												 																		 value_iqr %>% round_tidy(digits[2]), ")"), 
												 											Total = paste0(value_median_total %>% round_tidy(digits[1]), " (", 
												 																		 value_iqr_total %>% round_tidy(digits[2]), ")") 
												 				)
												 			}} %>% 
												 				dplyr::select(dependent, value, Total) %>% 
												 				tidyr::pivot_wider(names_from = !! dependent, values_from = value) %>% 
												 				dplyr::mutate(
												 					label = .x,
												 					levels = "Median (IQR)"
												 				) 
												 		}
												 		} %>% 
												 		dplyr::select(label, levels, dplyr::everything()) %>% 
												 		dplyr::select(-Total, dplyr::everything()) %>% 
												 		# Total column
												 		{ if(total_col){
												 			.
												 		} else {
												 			dplyr::select(., -Total)
												 		}
												 		}
												 }
		) 
	}
	df.out %>% 
		# Add hypothesis test
		{ if(p){
			purrr::map2_df(., p_tests,
										 ~ dplyr::mutate(.x,
										 								p = .y)
			)} else {
				dplyr::bind_rows(.)
			}} %>%
		dplyr::select(label, levels, dplyr::everything()) %>% 
		as.data.frame() %>%
		{ if(fit_id){
			levels_id = .$levels
			# Catagorical outcome, continous explanatory
			drop = levels_id %in% c("Mean (SD)", "Median (IQR)")
			levels_id[drop] = ""
			# Continuous outcome, continuous explanatory
			regex_sqbracket = "^(\\[).*(\\])$"
			drop = grepl(regex_sqbracket, levels_id)
			levels_id[drop] = ""
			dplyr::mutate(., 
										fit_id = paste0(label, levels_id),
										index = 1:dim(.)[1])
		} else {
			.
		}} %>% 
		
		# Recode variable names to labels where available 
		dplyr::mutate(
			label = dplyr::recode(label,  !!! var_labels)
		) %>% 
		
		# Remove duplicate variables/p-values
		rm_duplicate_labels() %>% 
		
		# Add column totals
		{ if(add_col_totals){
			ff_column_totals(., .data, dependent, na_include, 
											 percent = include_col_totals_percent, 
											 digits = digits[4], label = col_totals_rowname, 
											 prefix = col_totals_prefix)
		} else {
			.
		}} %>% 
		
		# Add row totals
		{ if(add_row_totals){
			ff_row_totals(., .data, explanatory, missing_column = include_row_missing_col, 
										na_include, 
										total_name = row_totals_colname, na_name = row_missing_colname)
		} else {
			.
		}} %>% 
		
		
		# Add dependent label
		{ if(add_dependent_label){
			dependent_label(., .data, dependent, 
											prefix=dependent_label_prefix, suffix = dependent_label_suffix)
		} else {
			.
		}} #%>% 
	# This is breaking fit_id, add back in after workout what's wrong
	# 
	# # Replace any missing values with "", e.g. in (Missing) column
	# dplyr::mutate_all(.,
	#                   ~ dplyr::if_else(is.na(.), "", .)
	# )
}
