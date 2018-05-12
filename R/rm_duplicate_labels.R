#' Remove duplicate levels within \code{\link{summary_factorlist}}: \code{finalfit} helper function
#'
#' Not called directly.
#'
#' @param factorlist A factorlist intermediary.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when \code{na_include=TRUE}.
#' @return Returns a \code{factorlist} dataframe.
#'

# Remove duplicate levels from summary.factorlist ----
rm_duplicate_labels = function(factorlist, na_to_missing = TRUE){
	x = factorlist
	duplicate_rows = duplicated(x$label)
	x$label = as.character(x$label)
	x$label[duplicate_rows] = ""
	if (any(names(x) %in% "pvalue")){
		x$pvalue[duplicate_rows] = ""
		x$pvalue[x$pvalue == "0.000"] = "<0.001"
	}
	if (na_to_missing == TRUE){
		x$levels = as.character(x$levels)
		x$levels[which(x$levels == "NA")] = "Missing"
	}
	return(x)
}
