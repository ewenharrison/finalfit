finalfit.coxph = function(.data, dependent, explanatory, explanatory_multi=NULL, random_effect=NULL, metrics=FALSE){

		# Cox proprotional hazards model -----------------------------------------------------------
		# Summary table
		summary.out = suppressWarnings(
			summary_factorlist(.data, dependent=NULL, explanatory, fit_id=TRUE)
		)
		summary.out = summary.out[,-3] # Remove 'all' column with total counts

		# Univariable
		coxphuni_out = coxphuni(.data, dependent, explanatory)
		coxphuni_df = fit2df(coxphuni_out, estimate_suffix = " (univariable)")

		# Multivariable
		if (is.null(explanatory_multi)){
			coxphmulti_out = coxphmulti(.data, dependent, explanatory)
		} else {
			coxphmulti_out = coxphmulti(.data, dependent, explanatory_multi)
		}
		coxphmulti_df = fit2df(coxphmulti_out, estimate_suffix = " (multivariable)") #, metrics=metrics)

		# Merge dataframes
		# Uni
		df.out = finalfit_merge(summary.out, coxphuni_df)

		# Multi
		df.out = finalfit_merge(df.out, coxphmulti_df)

		# # Multi
		# if (metrics == FALSE){
		# 	df.out = finalfit_merge(df.out, glmmulti_df)
		# } else {
		# 	df.out = finalfit_merge(df.out, glmmulti_df[[1]])
		# }
		#
		# if (is.null(random_effect)){
		# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multivariable)"
		# } else {
		# 	names(df.out)[which(names(df.out)=="OR")] = "OR (multilevel)"
		# }

		# Label interactions
		na_label = which(is.na(df.out$label))
		df.out$label[na_label] = df.out$fit_id[na_label]
		df.out$levels[na_label] = "Interaction"

		# Tidy up
		index_fit_id = which(names(df.out)=="fit_id")
		index_index = which(names(df.out)=="index")
		df.out = df.out[,-c(index_fit_id, index_index)]
		return(df.out)
		# Add metrics
		# if (metrics == TRUE){
		# 	return(list(df.out, glmmulti_df[[2]]))
		# } else {
		# 	return(df.out)
		# }
	}
