#' Produce a coefficient table and plot
#'
#' Produce a coefficient and plot from a \code{lm()} model.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of depdendent variable
#'   (must be numeric/continuous).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param random_effect Character vector of length 1, name of random effect
#'   variable.
#' @param factorlist Option to provide output directly from
#'   \code{\link{summary_factorlist}()}.
#' @param lmfit Option to provide output directly from \code{\link{lmmulti}()}
#'   and \code{\link{lmmixed}()}.
#' @param confint_type For for \code{lmer} models, one of \code{c("default",
#'   "Wald", "profile", "boot")} Note "default" == "Wald".
#' @param remove_ref Logical. Remove reference level for factors.
#' @param breaks Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space Adjust table column spacing.
#' @param dependent_label Main label for plot.
#' @param prefix Plots are titled by default with the dependent variable. This
#'   adds text before that label.
#' @param suffix Plots are titled with the dependent variable. This adds text
#'   after that label.
#' @param table_text_size Alter font size of table text.
#' @param title_text_size Alter font size of title text.
#' @param plot_opts A list of arguments to be appended to the ggplot call by
#'   "+".
#' @param table_opts A list of arguments to be appended to the ggplot table call
#'   by "+".
#' @param ... Other parameters.
#' @return Returns a table and plot produced in \code{ggplot2}.
#'
#' @family finalfit plot functions
#' @export
#' 
#' @import ggplot2
#' 
#' @examples
#' library(finalfit)
#' library(ggplot2)
#'
#' # Coefficient plot
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "nodes"
#' colon_s %>%
#'   coefficient_plot(dependent, explanatory)
#'
#' colon_s %>%
#'   coefficient_plot(dependent, explanatory, table_text_size=4, title_text_size=14,
#'     plot_opts=list(xlab("Beta, 95% CI"), theme(axis.title = element_text(size=12))))

coefficient_plot = function(.data, dependent, explanatory, random_effect = NULL,
														factorlist=NULL, lmfit=NULL,
														confint_type = "default", remove_ref = FALSE,
														breaks=NULL, column_space=c(-0.5, -0.1, 0.5),
														dependent_label = NULL,
														prefix = "", suffix = ": Coefficient, 95% CI, p-value)",
														table_text_size = 5,
														title_text_size = 18,
														plot_opts = NULL, table_opts = NULL, ...){
	
	requireNamespace("ggplot2")
	
	# Generate or format factorlist object
	if(!is.null(factorlist)){
		if(is.null(factorlist$Total)) stop("summary_factorlist function must include total_col=TRUE")
		if(is.null(factorlist$fit_id)) stop("summary_factorlist function must include fit_id=TRUE")
	}
	
	if(is.null(factorlist)){
		factorlist = summary_factorlist(.data, dependent, explanatory, total_col=TRUE, fit_id=TRUE)
	}
	
	# Extract total
	factorlist$Total = sub("([[:digit:]]+).*", "\\1", factorlist$Total) %>% as.numeric()
	
	# For continuous variables, remove level label
	# drop = grepl("Mean \\(SD\\)|Median \\(IQR\\)", factorlist$levels)
	# factorlist$levels[drop] = "-"
	
	if(remove_ref){
		factorlist = factorlist %>%  
			dplyr::mutate(label = ifelse(label == "", NA, label)) %>% 
			tidyr::fill(label) %>% 
			dplyr::group_by(label) %>%
			dplyr::filter(dplyr::row_number() != 1 | 
											dplyr::n() > 2 |
											levels %in% c("Mean (SD)", "Median (IQR)")
			)%>% 
			rm_duplicate_labels()
	}
	
	if(is.null(breaks)){
		breaks = scales::pretty_breaks()
	}
	
	# Generate or format lm
	if(is.null(lmfit) && is.null(random_effect)){
		lmfit = lmmulti(.data, dependent, explanatory)
		lmfit_df_c = fit2df(lmfit, condense = TRUE, estimate_suffix = " (multivariable)",
												 confint_type = confint_type, ...)
	} else if(is.null(lmfit) && !is.null(random_effect)){
		lmfit = lmmixed(.data, dependent, explanatory, random_effect)
		lmfit_df_c = fit2df(lmfit, condense = TRUE, estimate_suffix = " (multilevel)",
												 confint_type = confint_type, ...)
	}
	
	lmfit_df = fit2df(lmfit, condense = FALSE, confint_type = confint_type,  ...)

	# Merge
	df.out = finalfit_merge(factorlist, lmfit_df_c)
	df.out = finalfit_merge(df.out, lmfit_df, ref_symbol = "0")
	
	# Fill in total for continuous variables (NA by default)
	df.out$Total[is.na(df.out$Total)] = dim(.data)[1]
	df.out$Total = as.numeric(df.out$Total)
	
	# Remove unwanted lines, where there are more variables in model than wish to display.
	# These not named in factorlist, creating this problem. Interactions don't show on plot.
	if (any(
		is.na(df.out$label)
	)
	){
		remove_rows = which(is.na(df.out$label)) # This row doesn't work when is.na == FALSE, hence if()
		df.out = df.out[-remove_rows,]
	} else {
		df.out
	}
	
	# Fix order
	df.out$levels = as.character(df.out$levels)
	df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])
	
	# Plot
	g1 = ggplot(df.out, aes(x = as.numeric(Coefficient), xmin = as.numeric(L95), xmax  = as.numeric(U95),
													y = fit_id))+
		geom_point(aes(size = Total), shape=22, fill="darkblue")+
		geom_errorbarh(height=0.2) +
		geom_vline(xintercept = 0, linetype = "longdash", colour = "black")+
		scale_x_continuous(breaks= breaks)+
		xlab("Coefficient (95% CI)")+
		theme_classic(14)+
		theme(axis.title.x = element_text(),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.line.y = element_blank(),
					axis.ticks.y = element_blank(),
					legend.position="none")
	
	t1 = ggplot(df.out, aes(x = as.numeric(Coefficient), y = fit_id))+
		annotate("text", x = column_space[1], y = df.out$fit_id, label=df.out[,2], hjust=0, size=table_text_size)+
		annotate("text", x = column_space[2], y = df.out$fit_id, label=df.out[,3], hjust=1, size=table_text_size)+
		annotate("text", x = column_space[3], y = df.out$fit_id, label=df.out[,8], hjust=1, size=table_text_size)+
		theme_classic(14)+
		theme(axis.title.x = element_text(colour = "white"),
					axis.text.x = element_text(colour = "white"),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.ticks.y = element_blank(),
					line = element_blank())
	
	# Add optional arguments
	g1 = g1 + plot_opts
	t1 = t1 + table_opts
	
	# Add dependent name label
	title = 	plot_title(.data, dependent, dependent_label = dependent_label, prefix = prefix, suffix = suffix)
	
	gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
													top=grid::textGrob(title, x=0.02, y=0.2,
																						 gp=grid::gpar(fontsize=title_text_size), just="left"))
}
