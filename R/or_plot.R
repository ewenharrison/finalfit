#' Produce an odds ratio table and plot
#'
#' Produce an odds ratio table and plot from a \code{glm()} or
#' \code{lme4::glmer()} model.
#'
#' @param .data Data frame.
#' @param dependent Character vector of length 1:  name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param random_effect Character vector of length 1, name of random effect variable.
#' @param factorlist Option to provide output directly from
#'   \code{\link{summary_factorlist}()}.
#' @param glmfit Option to provide output directly from \code{\link{glmmulti}()}
#'   and \code{\link{glmmixed}()}.
#' @param confint_type One of \code{c("profile", "default")} for GLM models or
#'   \code{c("default", "Wald", "profile", "boot")} for \code{glmer}
#'   models.
#' @param confint_level The confidence level required.
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
#' @importFrom utils globalVariables
#' @import ggplot2
#' 
#' @examples
#' library(finalfit)
#' library(dplyr)
#' library(ggplot2)
#'
#' # OR plot
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	 or_plot(dependent, explanatory)
#'
#' colon_s %>%
#'   or_plot(dependent, explanatory, table_text_size=4, title_text_size=14,
#'     plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))


or_plot = function(.data, dependent, explanatory, random_effect=NULL, 
									 factorlist=NULL, glmfit=NULL,
									 confint_type = NULL, confint_level = 0.95, remove_ref = FALSE,
									 breaks=NULL, column_space=c(-0.5, 0, 0.5),
									 dependent_label = NULL,
									 prefix = "", suffix = NULL,
									 table_text_size = 4,
									 title_text_size = 13,
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
	
	# Confidence intervals, default to "profile" for glm and "Wald" for glmer
	if(is.null(confint_type) && is.null(random_effect)){
		confint_type = "profile"
	} else if(is.null(confint_type) && (!is.null(random_effect) | inherits(glmfit, "glmerMod"))){
		confint_type = "default"
	}
	
	# Generate or format glm
	if(is.null(glmfit) && is.null(random_effect)){
		glmfit = glmmulti(.data, dependent, explanatory)
		glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
												 confint_type = confint_type, confint_level = confint_level, ...)
	} else if(is.null(glmfit) && !is.null(random_effect)){
		glmfit = glmmixed(.data, dependent, explanatory, random_effect)
		glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
												 confint_type = confint_type, confint_level = confint_level, ...)
	}
	if(!is.null(glmfit) && is.null(random_effect)){
		glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
												 confint_type = confint_type, confint_level = confint_level, 
												 estimate_name = "OR", exp = TRUE, ...)
	} else if(!is.null(glmfit) && !is.null(random_effect)){
		glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
												 confint_type = confint_type, confint_level = confint_level, 
												 estimate_name = "OR", exp = TRUE, ...)
	}
	
	glmfit_df = fit2df(glmfit, condense = FALSE, confint_type = confint_type,  confint_level = confint_level, 
										 estimate_name = "OR", exp = TRUE, ...)
	
	# Merge
	df.out = finalfit_merge(factorlist, glmfit_df_c)
	df.out = finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
	
	# Remove proportions from total column and make continuous explanatory reflect dataset
	df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>% 
		as.numeric()
	df.out$Total[which(df.out$levels %in% c("Mean (SD)", "Median (IQR)", "-"))] = dim(.data)[1]
	
	# For continuous variables, remove level label
	df.out$levels[which(df.out$levels %in% c("Mean (SD)", "Median (IQR)"))] = "-"
	
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
	
	# Extract confidence interval name if not 95%
	confint_name = df.out %>% 
		dplyr::select(dplyr::matches("(U|L)[[:digit:]][[:digit:]]")) %>% 
		names()
	
	p_value_text = ifelse(confint_level != 0.95, ")", ", p-value)")
	if(is.null(suffix)) suffix = paste0(": OR (", confint_level*100, "% CI", p_value_text)
	
	# Plot
	g1 = ggplot(df.out, aes(x = as.numeric(OR), 
													xmin = as.numeric(!! sym(confint_name[1])), xmax  = as.numeric(!! sym(confint_name[2])),
													y = fit_id))+
		geom_errorbarh(height=0.2) +
		geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
		geom_point(aes(size = Total), shape=22, fill="darkblue")+
		scale_x_continuous(trans="log10", breaks= breaks)+
		xlab(paste0("Odds ratio (", confint_level*100, "% CI, log scale)")) +
		theme_classic(11)+
		theme(axis.title.x = element_text(),
					axis.title.y = element_blank(),
					axis.text.y = element_blank(),
					axis.line.y = element_blank(),
					axis.ticks.y = element_blank(),
					legend.position="none")
	
	t1 = ggplot(df.out, aes(x = as.numeric(OR), y = fit_id))+
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
