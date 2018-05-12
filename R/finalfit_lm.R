finalfit.lm = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL, metrics=FALSE){

	# Linear regression model ------------------------------------------
	# Summary table
	summary.out = suppressWarnings(
		summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
											 column=TRUE, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)
	)

	# Univariable
	lmuni.out = lmuni(.data, dependent, explanatory)
	lmuni.df = fit2df(lmuni.out, estimate_suffix = " (univariable)")

	# Multivariable/Mixed
	if (is.null(random_effect)){
		if (is.null(explanatory_multi)){
			lmmulti.out = lmmulti(.data, dependent, explanatory)
		} else {
			lmmulti.out = lmmulti(.data, dependent, explanatory_multi)
		}
		lmmulti.df = fit2df(lmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)")
	} else if (!is.null(random_effect)){
		if (is.null(explanatory_multi)){
			lmmulti.out = lmmixed(.data, dependent, explanatory, random_effect)
		} else {
			lmmulti.out = lmmixed(.data, dependent, explanatory_multi, random_effect)
		}
		lmmulti.df = fit2df(lmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)")
	}

	# Merge dataframes
	# Uni
	df.out = finalfit_merge(summary.out, lmuni.df)

	# Multi
	if (metrics == FALSE){
		df.out = finalfit_merge(df.out, lmmulti.df)
	} else {
		df.out = finalfit_merge(df.out, lmmulti.df[[1]])
	}

	# Label interactions
	na_label = which(is.na(df.out$label))
	df.out$label[na_label] = df.out$fit_id[na_label]
	df.out$levels = as.character(df.out$levels)
	df.out$levels[na_label] = "Interaction"

	# Tidy up
	index_fit_id = which(names(df.out)=="fit_id")
	index_index = which(names(df.out)=="index")
	df.out = df.out[,-c(index_fit_id, index_index)]

	# Add metrics
	if (metrics == TRUE){
		return(list(df.out, lmmulti.df[[2]]))
	} else {
		return(df.out)
	}
}
