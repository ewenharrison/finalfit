#' Produce an odds ratio table and plot
#'
#' Produce an odds ratio table and plot from a \code{glm()} or
#' \code{lme4::glmer()} model.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of depdendent variable
#'   (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param factorlist Option to provide output directly from
#'   \code{\link{summary_factorlist}()}.
#' @param glmfit Option to provide output directly from \code{\link{glmmulti}()}
#'   and \code{\link{glmmixed}()}.
#' @param breaks Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space Adjust table column spacing.
#' @param dependent_label Main label for plot.
#' @param prefix Plots are titled by default with the dependent variable. This adds text before that label.
#' @param suffix Plots are titled with the dependent variable. This adds text after that label.
#' @param table_text_size Alter font size of table text.
#' @param title_text_size Alter font size of title text.
#' @param plot_opts A list of arguments to be appended to the ggplot call by "+".
#' @param table_opts A list of arguments to be appended to the ggplot table call by "+".
#' @param ... Other parameters.
#' @return Returns a table and plot produced in \code{ggplot2}.
#'
#' @family finalfit plot functions
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' # OR plot
#' data(colon_s)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#' colon_s %>%
#' 	 or_plot(dependent, explanatory)
#'
#' colon_s %>%
#'   or_plot(dependent, explanatory, table_text_size=4, title_text_size=14,
#'     plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))
#'
#' @import ggplot2

or_plot = function(.data, dependent, explanatory, factorlist=NULL, glmfit=NULL,
                   breaks=NULL, column_space=c(-0.5, 0, 0.5),
                   dependent_label = NULL,
                   prefix = "", suffix = ": (OR, 95% CI, p-value)",
                   table_text_size = 5,
                   title_text_size = 18,
                   plot_opts = NULL, table_opts = NULL, ...){
  requireNamespace("ggplot2", quietly = TRUE)
  # Generate or format factorlist object
  if(is.null(factorlist)){
    factorlist = summary_factorlist(.data, dependent, explanatory, total_col=TRUE, fit_id=TRUE)
  }

  # Generate or format glm
  if(is.null(glmfit)){
    glmfit = glmmulti(.data, dependent, explanatory)
  }
  glmfit_df_c = fit2df(glmfit, condense = TRUE, ...)
  glmfit_df = fit2df(glmfit, condense = FALSE, ...)

  if(is.null(breaks)){
    breaks = scales::pretty_breaks()
  }
  # Merge
  df.out = finalfit_merge(factorlist, glmfit_df_c)
  names(df.out)[which(names(df.out) %in% "OR")] = "OR (multivariate)"
  df.out = finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")

  # Fill in total for continuous variables (NA by default)
  df.out$Total[is.na(df.out$Total)] = dim(.data)[1]

  # Remove unwanted lines, where there are more variables in model than wish to display.
  # Note merge function in summarizer merge is now `all` rather than `all.x` as wish to preserve interactions
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
  g1 = ggplot(df.out, aes(x = as.numeric(OR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
                          y = fit_id))+
    geom_point(aes(size = Total), shape=22, fill="darkblue")+
    geom_errorbarh(height=0.2) +
    geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
    scale_x_continuous(trans="log10", breaks= breaks)+
    xlab("Odds ratio (95% CI, log scale)")+
    theme_classic(14)+
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
