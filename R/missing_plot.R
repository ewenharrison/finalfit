#' Missing values heatmap
#'
#' @param .data Data frame.
#' @param dependent Character vector. Optional name of dependent variable.
#' @param explanatory Character vector. Optional name(s) of explanatory variables.
#' @param title Character vector. Optional title for plot
#' @param plot_opts A list of arguments to be appended to the ggplot call by "+".
#'
#' @return Heat map of missing values in dataset.
#' @export
#'
#' @importFrom forcats fct_rev
#' @importFrom tidyr gather
#'
#' @examples
#'
#' colon_s %>%
#'   missing_plot()
missing_plot <- function(.data, dependent=NULL, explanatory=NULL,
                         title=NULL,
                         plot_opts = NULL){
  requireNamespace("ggplot2")

  if(is.null(dependent) && is.null(explanatory)){
    df.in = .data
  }else{
    keep = names(.data) %in% c(dependent, explanatory)
    df.in = .data[keep]
  }

  # Replace missings with 1s
  .data %>%
    dplyr::mutate_all(.fun = function(x){
      ifelse(is.na(x), 1, 0)
    }) -> .data

  # Take dataframe rownames for x-axis
  .data$.id = rownames(.data) %>% as.numeric()

  # Gather to key and values for plot
  .data %>%
    tidyr::gather("var", "value", -.id, factor_key = TRUE) -> plot_df

  # Plot title
  if(is.null(title)) title = paste0("Missing data heatmap")


  ggplot(plot_df, aes(x = .id, y = forcats::fct_rev(var), fill = value))+
    geom_tile()+
    xlab("Dataframe row")+
    ylab("")+
    theme_minimal()+
    theme(legend.position="none")+
    ggtitle(title)+
    plot_opts


}
