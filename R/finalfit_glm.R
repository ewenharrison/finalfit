finalfit.glm = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL, metrics=FALSE){

	# Logistic regression ----
		# Summary table
		summary.out = summary_factorlist(.data, dependent, explanatory, p=FALSE, na_include=FALSE,
																		 column=TRUE, total_col=FALSE, orderbytotal=FALSE, fit_id=TRUE)

		# Univariable
		glmuni.out = glmuni(.data, dependent, explanatory)
		glmuni.df = fit2df(glmuni.out, estimate_suffix = " (univariable)")

		# Multivariable/Mixed
		if (is.null(random_effect)){
			if (is.null(explanatory_multi)){
				glmmulti.out = glmmulti(.data, dependent, explanatory)
			} else {
				glmmulti.out = glmmulti(.data, dependent, explanatory_multi)
			}
			glmmulti.df = fit2df(glmmulti.out, metrics=metrics, estimate_suffix = " (multivariable)")
		} else if (is.null(random_effect) == FALSE){
			if (is.null(explanatory_multi)){
				glmmulti.out = glmmixed(.data, dependent, explanatory, random_effect)
			} else {
				glmmulti.out = glmmixed(.data, dependent, explanatory_multi, random_effect)
			}
			glmmulti.df = fit2df(glmmulti.out, metrics=metrics, estimate_suffix = " (multilevel)")
		}

		# Merge dataframes
		# Uni
		df.out = finalfit_merge(summary.out, glmuni.df)

		# Multi
		if (metrics == FALSE){
			df.out = finalfit_merge(df.out, glmmulti.df)
		} else {
			df.out = finalfit_merge(df.out, glmmulti.df[[1]])
		}

		# Label interactions
		na_label = which(is.na(df.out$label))
		df.out$label[na_label] = df.out$fit_id[na_label]
		df.out$levels[na_label] = "Interaction"

		# Tidy up
		index_fit_id = which(names(df.out)=="fit_id")
		index_index = which(names(df.out)=="index")
		df.out = df.out[,-c(index_fit_id, index_index)]

		# Add metrics
		if (metrics == TRUE){
			return(list(df.out, glmmulti.df[[2]]))
		} else {
			return(df.out)
		}
}
