#' Fisher's exact test for use with \code{\link{summary_factorlist}}
#'
#' @param tab A frequency table such as that created by \code{table()}.
#'
#' @export
#' @importFrom stats fisher.test
#'
#' @examples
#' explanatory = c("sex.factor", "obstruct.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory, p = TRUE, catTest = catTestfisher)

catTestfisher <- function (tab) 
{
	st <- if (!is.matrix(tab) || nrow(tab) < 2 || ncol(tab) < 
						2) 
		list(p.value = NA, statistic = NA, parameter = NA)
	else {
		rowcounts <- tab %*% rep(1, ncol(tab))
		tab <- tab[rowcounts > 0, ]
		if (!is.matrix(tab)) 
			list(p.value = NA, statistic = NA, parameter = NA)
		else fisher.test(tab)
	}
	list(P = st$p.value, stat = st$statistic, df = st$parameter, 
			 testname = "Pearson", statname = "Chi-square", namefun = "chisq", 
			 latexstat = "\\chi^{2}_{df}", plotmathstat = "chi[df]^2")
}
