#' Produce a hazard ratio table and plot
#'
#' Produce hazard ratio table and plot from a Cox Proportional Hazards analysis, \code{survival::coxph()}.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of survival object in form \code{Surv(time, status)}.
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param factorlist Option to provide output directly from \code{\link{summary_factorlist}()}.
#' @param coxfit Option to provide output directly from \code{coxphmulti()}.
#' @param remove_ref Logical. Remove reference level for factors.
#' @param breaks Manually specify x-axis breaks in format \code{c(0.1, 1, 10)}.
#' @param column_space Adjust table column spacing.
#' @param dependent_label Main label for plot.
#' @param prefix Plots are titled by default with the dependent variable. This adds text before that label.
#' @param suffix Plots are titled with the dependent variable. This adds text after that label.
#' @param table_text_size Alter font size of table text.
#' @param title_text_size Alter font size of title text.
#' @param plot_opts A list of arguments to be appended to the ggplot call by "+".
#' @param table_opts A list of arguments to be appended to the ggplot table call by "+".
#' @param ... Other parameters passed to \code{fit2df()}.
#' @return Returns a table and plot produced in \code{ggplot2}.
#'
#' @family finalfit plot functions
#' @export
#'
#' @examples
#' # HR plot
#' library(finalfit)
#' library(dplyr)
#' library(ggplot2)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "Surv(time, status)"
#' colon_s %>%
#'   hr_plot(dependent, explanatory, dependent_label = "Survival")
#'
#' colon_s %>%
#'   hr_plot(dependent, explanatory, dependent_label = "Survival",
#'     table_text_size=4, title_text_size=14,
#'     plot_opts=list(xlab("HR, 95% CI"), theme(axis.title = element_text(size=12))))
#'
#' @import ggplot2
#'

hr_plot = function(.data, dependent, explanatory, factorlist=NULL, coxfit=NULL,
                   remove_ref = FALSE,
                   breaks=NULL, column_space=c(-0.5, 0, 0.5),
                   dependent_label = "Survival", 
                   prefix = "", suffix = ": HR (95% CI, p-value)",
                   table_text_size = 4,
                   title_text_size = 13,
                   plot_opts = NULL, table_opts = NULL, ...){

  requireNamespace("ggplot2")

  # Generate or format factorlist object
  if(!is.null(factorlist)){
    if(is.null(factorlist$fit_id)) stop("summary_factorlist function must include fit_id=TRUE")
  }

  if(is.null(factorlist)){
    factorlist = summary_factorlist(.data, dependent, explanatory, fit_id=TRUE)
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

  # Specify breaks if provided
  if(is.null(breaks)){
    breaks = scales::breaks_log()
  }

  # Extract totals (this is CPH specific due to how summary_factorlist works)
  factorlist$Total = as.numeric(stringr::str_extract(as.character(factorlist$all), "^[:digit:]*"))

  # Fill in total for continuous variables
  factorlist$Total[which(factorlist$levels %in% c("Mean (SD)", "Median (IQR)"))] = dim(.data)[1]
  
  # For continuous variables, remove level label
  drop = grepl("Mean \\(SD\\)|Median \\(IQR\\)", factorlist$levels)
  factorlist$levels[drop] = "-"

  factorlist$all = NULL
  
  # Generate or format glm
  if(is.null(coxfit)){
    coxfit = coxphmulti(.data, dependent, explanatory)
  }
  coxfit_df_c = fit2df(coxfit, condense = TRUE, estimate_suffix = " (multivariable)", 
                       estimate_name = "HR", exp = TRUE, ...)
  coxfit_df = fit2df(coxfit, condense = FALSE, 
                     estimate_name = "HR", exp = TRUE, ...)

  # Merge
  df.out = finalfit_merge(factorlist, coxfit_df_c)
  df.out = finalfit_merge(df.out, coxfit_df, ref_symbol = "1.0")
  
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
  g1 = ggplot(df.out, aes(x = as.numeric(HR), xmin = as.numeric(L95), xmax  = as.numeric(U95),
                          y = fit_id))+
    geom_errorbarh(height=0.2) +
    geom_vline(xintercept = 1, linetype = "longdash", colour = "black")+
  	geom_point(aes(size = Total), shape=22, fill="darkblue")+
    scale_x_continuous(trans="log10", breaks= breaks)+
    xlab("Hazard ratio (95% CI, log scale)")+ 
    theme_classic(11)+
    theme(axis.title.x = element_text(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position="none")

  t1 = ggplot(df.out, aes(x = as.numeric(HR), y = fit_id))+
    annotate("text", x = column_space[1], y =  df.out$fit_id, label=df.out[,2], hjust=0, size=table_text_size)+
    annotate("text", x = column_space[2], y =  df.out$fit_id, label=df.out[,3], hjust=1, size=table_text_size)+
    annotate("text", x = column_space[3], y =  df.out$fit_id, label=df.out[,6], hjust=1, size=table_text_size)+
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
  title_text = plot_title(.data, dependent, dependent_label = dependent_label, prefix = prefix, suffix = suffix)
  
  title_grob = cowplot::ggdraw() + 
  	cowplot::draw_label(title_text, x = 0, hjust = 0, size = title_text_size) +
  	theme(plot.margin = margin(0, 0, 0, 7))
  
  cowplot::plot_grid(title_grob,
  									 cowplot::plot_grid(t1, g1, align = "h", rel_widths = c(3,2)),
  									 ncol = 1, rel_heights = c(0.1, 1))
}
