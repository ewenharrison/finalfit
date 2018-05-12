#' Extract \code{glmboot} model fit results to dataframe: \code{finalfit} model extracters
#'
#' Takes output from \code{\link[stats]{glm}} with \code{boot::\link[boot]{boot}}
#' confidence intervals on fixed effect coefficients and extracts to a dataframe,
#' convenient for further processing in preparation for final results table.
#'
#' \code{fit2df.glmboot} is the model extract method for \code{\link{glmmulti_boot}} models.
#'
#' @param fit Output from \code{\link{glmmulti_boot}}.
#' @param condense Logical: when true, effect estimates, confidence intervals and p-values
#'   are pasted conveniently together in single cell.
#' @param metrics Logical: when true, useful model metrics are extracted (not currently implemented).
#' @param estimate_suffix Character vector of length one specifying string to be appended
#'   to output column name
#' @param ... Other arguments (not used).
#' @return A dataframe of model parameters.
#'
#' @family \code{finalfit} model extracters
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	glmmulti_boot(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable (BS CIs))")
#'

fit2df.glmboot = function(fit, condense=TRUE, metrics=NULL, estimate_suffix="", ...){
	if(!is.null(metrics)) warning("Metrics not currently available for this model")
	boot_results = fit
	df.out = data.frame(OR=exp(boot_results$t0))
	R = dim(boot_results$t)[1]
	for (i in 1:dim(df.out)[1]){
		df.out$L95[i] = exp(sort(boot_results$t[,i]))[floor(R*0.025)]
		df.out$U95[i] = exp(sort(boot_results$t[,i]))[floor((R*0.975)+1)]
		df.out$p[i] = ifelse(boot_results$t0[i] >= 0, mean(boot_results$t[,i]<0)*2, mean(boot_results$t[,i]>0)*2)
	}

	if (condense==TRUE){
		df.out = data.frame(
			"explanatory" = row.names(df.out),
			"OR" = paste0(sprintf("%.2f", df.out$OR), " (", sprintf("%.2f", df.out$L95), "-",
										sprintf("%.2f", df.out$U95), ", p=", sprintf("%.3f", df.out$p), ")"))
		colnames(df.out) = c("explanatory", paste0("OR", estimate_suffix))
	}

	# Remove intercept
	df.out = df.out[-which(df.out$explanatory =="(Intercept)"),]

	return(df.out)
}
