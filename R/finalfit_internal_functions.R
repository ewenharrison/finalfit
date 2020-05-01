#' Eval for `lm` and `glm` model wrappers
#'
#' Internal function, not called directly. This is in reponse to a long running
#' issue of the best way to pass `weights` to `lm()` and `glm()`. See here
#' https://stackoverflow.com/questions/54383414/passing-weights-to-glm-using-rlang
#'
#' @param .
#'
#' @keywords internal
ff_eval <- function(.) {
	eval(rlang::enexpr(.), rlang::caller_env())
}

#' Print methods for finalfit data frames
#'
#' @param .data Data frame
#' @return Data frame with no line numbers
#' 
#' @rdname print
#' @method print data.frame.ff
#' @export
#' 
#' @keywords internal
#'
print.data.frame.ff <- function(x, ...){
	print.data.frame(x, row.names = FALSE, ...)
}

#' Extract model output to dataframe
#'
#' Internal function, not usually called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate
#' @param confint_type One of \code{c("profile", "default")} for GLM
#'   models or \code{c("profile", "Wald", "boot")} for \code{glmer/lmer} models.
#'   Not implemented for \code{lm, coxph or coxphlist}.
#' @param confint_level The confidence level required.
#' @param ... Other arguments.
#'
#' @keywords internal
#' @export

extract_fit = function(...){
	UseMethod("extract_fit")
}

#' Extract model output to dataframe
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit glm
#' @export

extract_fit.glm = function(.data, explanatory_name="explanatory", estimate_name="OR",
													 estimate_suffix = "",  p_name = "p", exp = TRUE,
													 confint_type = "profile", confint_level = 0.95, ...){
	x=.data
	explanatory = names(coef(x))
	estimate = coef(x)
	if (confint_type == "profile"){
		confint = confint(x, level = confint_level)
	}else if (confint_type == "default"){
		confint = confint.default(x, level = confint_level)
	}
	p_col = dimnames(summary(x)$coef)[[2]] %in% c("Pr(>|t|)", "Pr(>|z|)")
	p = summary(x)$coef[ ,p_col]
	L_confint_name = paste0("L", confint_level*100)
	U_confint_name = paste0("U", confint_level*100)
	
	df.out = dplyr::tibble(explanatory, estimate, confint[,1], confint[,2], p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix),
											 L_confint_name, U_confint_name, p_name)
	if(exp){
		df.out[, 2:4] = df.out[, 2:4] %>% exp() # mutate_at not working here
	}
	if(confint_level != 0.95){
		df.out = df.out %>% dplyr::select(-p_name)
	}
	df.out = data.frame(df.out)
	return(df.out)
}

#' Extract model output to dataframe
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit glmerMod
#' @export

extract_fit.glmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
																estimate_suffix = "",  p_name = "p", exp = TRUE, 
																confint_type = "Wald", confint_level = 0.95, ...){
	x=.data
	if(confint_type == "default") confint_type = "Wald"
	explanatory = names(lme4::fixef(x))
	estimate = lme4::fixef(x)
	confint = lme4::confint.merMod(x, level = confint_level, method = confint_type)
	confint = confint[-grep("sig", rownames(confint)),]
	p = summary(x)$coef[,"Pr(>|z|)"]
	L_confint_name = paste0("L", confint_level*100)
	U_confint_name = paste0("U", confint_level*100)
	
	df.out = dplyr::tibble(explanatory, estimate, confint[,1], confint[,2], p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix),
											 L_confint_name, U_confint_name, p_name)
	
	if(exp){
		df.out[, 2:4] = df.out[, 2:4] %>% exp() # mutate_at not working here
	}
	
	if(confint_level != 0.95){
		df.out = df.out %>% dplyr::select(-p_name)
	}
	df.out = data.frame(df.out)
	return(df.out)
}


#' Extract model output to dataframe
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit lm
#' @export

extract_fit.lm = function(.data, explanatory_name="explanatory", estimate_name="Coefficient",
													estimate_suffix = "",  p_name = "p",
													confint_level = 0.95, ...){
	x=.data
	explanatory = names(coef(x))
	estimate = coef(x)
	confint = confint(x)
	p = summary(x)$coef[,"Pr(>|t|)"]
	L_confint_name = paste0("L", confint_level*100)
	U_confint_name = paste0("U", confint_level*100)
	
	df.out = dplyr::tibble(explanatory, estimate, confint[,1], confint[,2], p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix),
											 L_confint_name, U_confint_name, p_name)
	if(confint_level != 0.95){
		df.out = df.out %>% dplyr::select(-p_name)
	}
	df.out = data.frame(df.out)
	return(df.out)
}

#' Extract model output to dataframe
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit lmerMod
#' @export

extract_fit.lmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
															 estimate_suffix = "",  p_name = "p",
															 confint_type = "Wald", confint_level = 0.95, ...){
	x=.data
	if(confint_type == "default") confint_type = "Wald"
	explanatory = names(lme4::fixef(x))
	estimate = lme4::fixef(x)
	confint = lme4::confint.merMod(x, method = confint_type)
	confint = confint[-grep("sig", rownames(confint)),]
	p = 1-pnorm(abs(summary(x)$coefficients[,3]))
	message("P-value for lmer is estimate assuming t-distribution is normal. Bootstrap for final publication.")
	
	L_confint_name = paste0("L", confint_level*100)
	U_confint_name = paste0("U", confint_level*100)
	
	df.out = dplyr::tibble(explanatory, estimate, confint[,1], confint[,2], p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix),
											 L_confint_name, U_confint_name, p_name)
	if(confint_level != 0.95){
		df.out = df.out %>% dplyr::select(-p_name)
	}
	df.out = data.frame(df.out)
	return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit coxph
#' @export

extract_fit.coxph = function(.data, explanatory_name="explanatory", estimate_name="HR",
														 estimate_suffix = "",
														 p_name = "p", ...){
	x = .data
	results = summary(x)$conf.int
	# Below is required to cope with difference in output when `frailty()` included
	explanatory = row.names(summary(x)$coefficients)[
		row.names(summary(x)$coefficients) %in% row.names(summary(x)$conf.int)
		]
	estimate = results[explanatory, 1]
	confint_L = results[explanatory, 3]
	confint_U = results[explanatory, 4]
	
	p = summary(x)$coefficients[explanatory,
															dim(summary(x)$coefficients)[2]]
	
	df.out = dplyr::tibble(explanatory, estimate, confint_L, confint_U, p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
	df.out = data.frame(df.out)
	return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit crr
#' @export

extract_fit.crr = function(.data, explanatory_name="explanatory", estimate_name="HR",
													 estimate_suffix = "",
													 p_name = "p", ...){
	x=.data
	results = summary(x)$conf.int
	explanatory = row.names(results)
	estimate = results[,1]
	confint_L = results[,3]
	confint_U = results[,4]
	p = summary(x)$coef[explanatory,
											max(dim(summary(x)$coef)[2])] # Hack to get p fe and re
	df.out = dplyr::tibble(explanatory, estimate, confint_L, confint_U, p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
	df.out = data.frame(df.out)
	return(df.out)
}



#' Extract model output to dataframe
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit coxme
#' @export

extract_fit.coxme = function(.data, explanatory_name="explanatory", estimate_name="HR",
																estimate_suffix = "",  p_name = "p",
																confint_level = 0.95, ...){
	
	extract_coxme_table <- function (fit){
		beta <- fit$coefficients #$fixed is not needed
		nvar <- length(beta)
		nfrail <- nrow(fit$var) - nvar
		se <- sqrt(diag(fit$var)[nfrail + 1:nvar])
		z <- round(beta/se, 2)
		p <- signif(1 - pchisq((beta/se)^2, 1), 2)
		table=data.frame(cbind(beta,se,z,p))
		return(table)
	}
	
	results = extract_coxme_table(.data)
	explanatory = row.names(results)
	estimate = exp(results$beta)
	confint_results = confint(fit, level = confint_level) %>% exp()
	confint_L = confint_results[, 1]
	contint_U = confint_results[, 2]
	p = results$p
	df.out = dplyr::tibble(explanatory, estimate, confint_L, confint_U, p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
	
	if(confint_level != 0.95){
		df.out = df.out %>% dplyr::select(-p_name)
	}
	df.out = data.frame(df.out)
	return(df.out)
}


#' Extract model output to dataframe
#'
#' @param X Design matrix from Stan modelling procedure.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit stanfit
#' @export

extract_fit.stanfit = function(.data, explanatory_name="explanatory", estimate_name="OR",
															 estimate_suffix = "",  p_name = "p", digits=c(2,2,3), X, ...){
	stanfit = .data
	pars = "beta"
	quantiles =  c(0.025, 0.50, 0.975)
	
	explanatory = dimnames(X)[[2]]
	results = rstan::summary(stanfit,
													 pars = pars,
													 probs = quantiles)$summary
	estimate = exp(results[, 1])
	confint_L = exp(results[, 4])
	confint_U = exp(results[, 6])
	
	# Determine a p-value based on two-sided examination of chains
	chains = rstan::extract(stanfit, pars=pars, permuted = TRUE, inc_warmup = FALSE,
													include = TRUE)
	p1.out = apply(chains[[1]], 2, function(x)mean(x<0))
	p2.out = apply(chains[[1]], 2, function(x)mean(x>0))
	p1.out = p1.out*2
	p2.out = p2.out*2
	p.out = ifelse(p1.out < 1, p1.out, p2.out)
	p = round(p.out, 3)
	
	df.out = data.frame(explanatory, estimate, confint_L, confint_U, p)
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
	return(df.out)
}

#' Condense model output dataframe for final tables
#'
#' Internal function, not called directly. Can only be used in conjunction with
#'   extract_fit
#'
#' @param .data Dataframe of four or five columns, must be this order, (1) explanatory
#'   variable names, (2) estimate, (3) confidence interval lower limit, (4)
#'   confidence interval upper limit, (5) p-value (optional).
#' @param explanatory_name Name for this column in output
#' @param estimate_name Name for this column in output
#' @param estimate_suffix Appeneded to estimate name
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param confint_sep String to separate confidence intervals, typically "-" or
#'   " to ".
#'
#' @keywords internal
#' @export

condense_fit = function(.data, explanatory_name="explanatory", estimate_name=NA,
												estimate_suffix = "", p_name = "p",
												digits=c(2,2,3), confint_sep = "-"){
	x = .data
	d.estimate = digits[1]
	d.confint = digits[2]
	d.p = digits[3]
	if(is.na(estimate_name)){
		estimate_name = names(x)[2]
	}
	
	explanatory = x[,1]
	estimate = round_tidy(x[,2], d.estimate)
	L_confint = round_tidy(x[,3], d.confint)
	U_confint = round_tidy(x[,4], d.confint)
	if(dim(x)[2] == 5){  #p-value not included when CI != 95%
		p = p_tidy(x[,5], d.p)
		
		df.out = data.frame(
			explanatory,
			paste0(
				estimate, " (",
				L_confint, confint_sep,
				U_confint, ", ",
				p_name, p, ")"))
	}else{
		df.out = data.frame(
			explanatory,
			paste0(
				estimate, " (",
				L_confint, confint_sep,
				U_confint, ")"))
	}
	colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix)
	)
	return(df.out)
}

#' Round values but keep trailing zeros
#'
#' e.g. for 3 decimal places I want 1.200, not 1.2.
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @return Vector of strings.
#'
#' @export
#' 
#' @examples
#' round_tidy(0.01023, 3)

round_tidy = function(x, digits){
	sprintf.arg = paste0("%.", digits, "f")
	x.out = do.call(sprintf, list(sprintf.arg, x)) # keep trailing zeros
	return(x.out)
}

#' Round p-values but keep trailing zeros
#'
#' Internal function, not called directly
#'
#' e.g. for 3 decimal places I want 0.100, not 0.1. Note this function with
#' convert 0.000 to <0.001. All other values are prefixed with "=" by default
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @param prefix Appended in front of values for use with \code{condense_fit}.
#' @return Vector of strings.
#'
#' @export

p_tidy = function(x, digits, prefix="="){
	x.out = paste0(prefix, round_tidy(x, digits))
	all_zeros = paste0(prefix, round_tidy(0, digits))
	less_than = paste0("<", format(10^-digits, scientific=FALSE))
	x.out[x.out == all_zeros] = less_than
	return(x.out)
}


#' Format n and percent as a character
#'
#' Internal, function, not called directly
#'
#' @param n Value
#' @param percent Value
#' @param digits Value
#'
#' @export
#'
format_n_percent = function(n, percent, digits) {
	percent = round_tidy(percent, digits)
	paste0(n, " (", percent, ")")
}

#' Remove intercept from model output
#'
#' Internal function, not called directly
#'
#' @param .data Numeric vector of values to round
#' @param intercept_name Name given to interept in model. Should never have to
#'   change from default.
#' @return Vector of strings.
#'
#' @keywords internal
#' @export
# Tried to do this with dplyr programming and failed miserably.
# quo() enquo() !! all a bit of a nightmare
# So let's square bracket away!
remove_intercept = function(.data, intercept_name = "(Intercept)"){
	.data %>% 
		dplyr::filter_at(.vars = 1, dplyr::any_vars(. != intercept_name))
}

#' Remove duplicate levels within \code{\link{summary_factorlist}}: \code{finalfit} helper function
#'
#' Not called directly.
#'
#' @param factorlist A factorlist intermediary.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when \code{na_include=TRUE}.
#' @return Returns a \code{factorlist} dataframe.
#'
#' @keywords internal
#' @export

rm_duplicate_labels = function(factorlist, na_to_missing = TRUE){
	x = factorlist
	duplicate_rows = duplicated(x$label)
	x$label = as.character(x$label)
	x$label[duplicate_rows] = ""
	if (any(names(x) %in% "p")){
		x$p[duplicate_rows] = ""
		x$p[x$p == "0.000"] = "<0.001"
	}
	if (na_to_missing == TRUE){
		x$levels = as.character(x$levels)
		x$levels[which(x$levels == "NA")] = "Missing"
	}
	return(x)
}

#' Make a label for the dependent variable
#'
#' Can be add dependent label to final results dataframe.
#'
#' @param df.out Dataframe (results table) to be altered.
#' @param .data Original dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @return Returns the label for the dependent variable, if specified.
#' @export
#' @examples
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = 'mort_5yr'
#'
#' # Separate tables
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)") -> example.univariable
#'
#' colon_s %>%
#' 	 glmmulti(dependent, explanatory) %>%
#' 	 fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#'   finalfit_merge(example.univariable) %>%
#'   finalfit_merge(example.multivariable) %>%
#' 	 finalfit_merge(example.multilevel) %>%
#' 	 select(-c(fit_id, index)) %>%
#' 	 dependent_label(colon_s, dependent) -> example.final
#'   example.final
dependent_label = function(df.out, .data, dependent, prefix = "Dependent: ", suffix=""){
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data)
	d_label = attr(.data[,which(names(.data) %in% dependent)], "label")
	
	if (is.null(d_label)){
		d_label = dependent
	} else {
		d_label = d_label
	}
	names(df.out)[which(names(df.out) == "label")] = paste0(prefix, d_label, suffix)
	names(df.out)[which(names(df.out) == "levels")] = " "
	
	return(df.out)
}

#' Label plot title
#'
#' Not called directly.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1: quoted name of dependent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @keywords internal
#' @export
plot_title = function(.data, dependent, dependent_label, prefix = "", suffix=""){
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data)
	if (is.null(dependent_label)){
		d_label = attr(.data[,which(names(.data) %in% dependent)], "label")
		if (is.null(d_label)){
			d_label = dependent
		} else {
			d_label = d_label
		}
	} else {
		d_label = dependent_label
	}
	out = paste0(prefix, d_label, suffix)
	return(out)
}


#' Extract variable labels and names
#'
#' @param .data Data frame.
#'
#' @return A data frame with three columns: first (vname), variabe names; second
#'   (vlabel), variables labels; third (vfill), variable labels and when null
#'   variable names.
#' @export
#' @keywords internal
#'
#' @examples
#' colon_s %>%
#'   extract_labels()
extract_labels = function(.data){
	# Struggled to make this work and look elegant!
	# Works but surely there is a better way.
	df.out = lapply(.data, function(x) {
		vlabel = attr(x, "label")
		list(vlabel = vlabel)}) %>%
		do.call(rbind, .)
	df.out = data.frame(vname = rownames(df.out), vlabel = unlist(as.character(df.out)),
											stringsAsFactors = FALSE)
	df.out$vfill = df.out$vlabel
	df.out$vfill[df.out$vlabel == "NULL"] = df.out$vname[df.out$vlabel=="NULL"]
	return(df.out)
}


#' Help making stratified summary_factorlist tables
#'
#' @param df.out Output from \code{summary_factorlist}
#' @param .data Original data frame used for \code{summary_factorlist}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor")
#' dependent = "perfor.factor"
#'
#' # Pick option below
#' split = "rx.factor"
#' split = c("rx.factor", "node4.factor")
#'
#' # Piped function to generate stratified crosstabs table
#' colon_s %>%
#'   group_by(!!! syms(split)) %>% #Looks awkward, but avoids unquoted var names
#'   group_modify(~ summary_factorlist(.x, dependent, explanatory)) %>%
#'   ff_stratify_helper(colon_s)
ff_stratify_helper <- function(df.out, .data){
	# Get df labels
	lookup = extract_variable_label(.data)
	
	# Relabel label column
	df.out$label = df.out$label %>% 
		purrr::map_chr( ~ lookup[.x])
	
	# Get groups
	.cols = attributes(df.out)$groups %>% 
		names()
	.cols = .cols[!.cols == ".rows"]
	
	# Relabel column headings with labels. 
	names(df.out)[names(df.out) %in% .cols] = 
		names(df.out)[names(df.out) %in% .cols] %>% 
		purrr::map_chr( ~ lookup[.x])
	
	# Remove NAs for neatness
	df.out = df.out %>%
		dplyr::ungroup() %>%
		dplyr::mutate_if(is.factor, as.character) %>% 
		as.data.frame() %>% 
		dplyr::mutate_all(.,
											~ ifelse(is.na(.), "", .)
		)
	
	class(df.out) = c("data.frame.ff", class(df.out))
	return(df.out)
}

#' Generate formula as character string
#'
#' Useful when passing finalfit dependent and explanatory lists to base R
#' functions
#'
#' @param dependent Optional character vector: name(s) of depdendent
#'   variable(s).
#' @param explanatory Optional character vector: name(s) of explanatory
#'   variable(s).
#' @param random_effect Optional character vector: name(s) of random effect
#'   variable(s).
#'
#' @return Character vector
#' @export
#'
#' @examples
#' explanatory = c("age", "nodes", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' ff_formula(dependent, explanatory)
#' 
#' explanatory = c("age", "nodes", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' random_effect = "(age.factor | hospital)"
#' ff_formula(dependent, explanatory)
ff_formula = function(dependent, explanatory, random_effect = NULL){
	if(!is.null(random_effect)){
		if(!grepl("\\|", random_effect)) random_effect = paste0("(1 | ", random_effect, ")")
		out = paste0(dependent, "~", paste(explanatory, collapse="+"), " + ", random_effect)
	} else {
		out = paste(dependent, "~", paste(explanatory, collapse = "+"))	
	}
	return(out)
}
#' @rdname ff_formula
#' @export
finalfit_formula <- ff_formula


#' Determine type/class of a variable
#'
#' @param .var A vector, data frame column, or equivalent. 
#'
#' @return One of "factor", "character", "numeric", "logical", "date". 
#' @export
#' @keywords internal
#'
#' @examples
#' var_d = as.Date("12.03.18", "%d.%m.%y")
#' var_f = factor(c("yes", "no"))
#' var_c = c("yes", "no")
#' var_n = 1:10
#' var_l = as.logical(c("true", "false"))
#' variable_type(var_d)
#' variable_type(var_f)
#' variable_type(var_c)
#' variable_type(var_n)
#' variable_type(var_l)
variable_type <- function(.var){
	if(is.factor(.var)){
		out = "factor"
	}else if(is.character(.var)){
		out = "character"
	}else if(is.numeric(.var)){
		out = "numeric"
	}else if(is.logical(.var)){
		out = "logical"
	}else if(inherits(.var, 'Date')){
		out = "date"
	}
	return(out)
}


#' Test character describes survival object
#'
#' @param .name Character string to test
#'
#' @return Logical
#' @export
#' @keywords internal
#'
#' @examples
#' var_s = "Surv(mort, time)"
#' is.survival(var_s) #TRUE
#' var_s = "Sur(mort, time)"
#' is.survival(var_s) #FALSE
is.survival <- function(.name){
	grepl("^Surv[(].*[)]", .name)
}


# Specify global variables
globalVariables(c("L95", "U95", "fit_id", "Total", "dependent",
									"OR", "HR", "Coefficient", ".", ".id", "var", "value",
									":=", "Mean", "SD", "Median", "Q3", "Q1", "IQR", "Formatted", 
									"w", "Freq", "g", "total_prop", "Prop", "index_total", "vname", "Combined",
									"2.5 %", "97.5 %", "p.value", "estimate", "index", "n", "missing_n", "var_type",
									"missing_percent", "var1", "var2", "keep", "label", "rowid", "term",
									"confint_L", "confint_U", "explanatory", "p"))


# Workaround ::: as summary.formula not (yet) exported from Hmisc
`%:::%` = function (pkg, name){
	pkg <- as.character(substitute(pkg))
	name <- as.character(substitute(name))
	get(name, envir = asNamespace(pkg), inherits = FALSE)
}


#' Call to mice:::summary.mipo
#'
#' Not called directly.
#'
#' @keywords internal
#' @import mice
summary_mipo = 'mice' %:::% 'summary.mipo'


#' Errors: colon in factor levels
#'
#' @param .data Data frame.
#'
#' @return Logical
#' @keywords internal
error_colon_fct_levels <- function(.data){
	.data %>% 
		purrr::map(~ levels(.x)) %>%
		purrr::map(~ grepl(":", .x)) %>% 
		purrr::map(~ any(.x)) %>% 
		unlist() %>% 
		any()
}

#' Deprecated catTest from Hmisc for reverse dependencies
#'
#' @param . Null
#' @keywords internal
#' @export
catTestfisher = function(.){}

