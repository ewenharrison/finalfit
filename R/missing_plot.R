#' Missing values occurence plot
#'
#' Create a plot of missing values by observations on the x-axis and variable on
#' the y-axis. \code{Dependent} and \code{explanatory} are for convenience and are optional.
#'
#' @param .data Data frame.
#' @param dependent Character vector. Optional name of dependent variable.
#' @param explanatory Character vector. Optional name(s) of explanatory
#'   variables.
#' @param use_labels Use variable label names in plot labelling.
#' @param title Character vector. Optional title for plot.
#' @param plot_opts A list of arguments to be appended to the ggplot call by
#'   "+".
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
                         use_labels = TRUE,
                         title=NULL,
                         plot_opts = NULL){
  requireNamespace("ggplot2")

  if (is.null(dependent) && is.null(explanatory)) {
    df.in <<- .data
  }
  else {
    df.in = .data %>%
      dplyr::select(dependent, explanatory)
  }

  # Labels
  if(use_labels){
    vlabels = extract_labels(df.in)$vfill
  }

  # Replace missings with 1s
  df.in %>%
    dplyr::mutate_all(.fun = function(x){
      ifelse(is.na(x), 1, 0)
    }) -> df.in

  # Take dataframe rownames for x-axis
  df.in$.id = rownames(df.in) %>% as.numeric()

  # Gather to key and values for plot
  df.in %>%
    tidyr::gather("var", "value", -.id, factor_key = TRUE) -> plot_df

  # Plot title
  if(is.null(title)) title = paste0("Missing values map")


  ggplot(plot_df, aes(x = .id, y = forcats::fct_rev(var), fill = value))+
    geom_raster()+
    xlab("Observation")+
    scale_y_discrete("", breaks = rev(levels(plot_df$var)), labels=rev(vlabels))+
    theme_minimal()+
    theme(legend.position="none")+
    ggtitle(title)+
    plot_opts
}


#' Missing values data frame
#'
#' Create a data frame of missing vs. observed values for all variables
#' provided. \code{Dependent} and \code{explanatory} are for convenience and are
#' optional.
#'
#' @param .data Data frame.
#' @param dependent Character vector. Optional name of dependent variable.
#' @param explanatory Character vector. Optional name(s) of explanatory
#'   variables.
#'
#' @return Data frame of missing values for all variables.
#' @export
#'
#' @examples
#' colon_s %>%
#'   missing_df()
missing_df = function(.data, dependent=NULL, explanatory=NULL){
  if (is.null(dependent) && is.null(explanatory)) {
    df.in = .data
  }
  else {
    df.in = .data %>%
      dplyr::select(dependent, explanatory)
  }
  df.out = df.in %>%
    is.na() %>%
    data.frame() %>%
    dplyr::mutate_all(factor, levels=c("FALSE", "TRUE"), labels=c("Obs", "Miss"))
  names(df.out) = paste0(names(df.out), "_na")
  return(df.out)
}


#' Missing values pairs plot
#'
#' Compare the occurence of missing values in all variables by each other.
#' Suggest limit the number of variables to a maximum of around six.
#' \code{Dependent} and \code{explanatory} are for convenience of variable
#' selection, are optional, and have no other specific function.
#'
#' @param .data Data frame.
#' @param dependent Character vector. Optional name of dependent variable.
#' @param explanatory Character vector. Optional name(s) of explanatory
#'   variables.
#' @param use_labels Use variable label names in plot labelling.
#' @param title Character vector. Optional title for plot.
#' @param position For discrete variables, choose "stack" or "fill" to show
#'   counts or proportions.
#' @param showXAxisPlotLabels Show x-axis plot labels.
#' @param showYAxisPlotLabels Show y-axis plot labels.
#'
#' @return A plot matrix comparing missing values in all variables against each
#'   other.
#' @export
#' @importFrom purrr pmap
#' @examples
#' \dontrun{
#' explanatory = c("age", "nodes", "age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#' colon_s %>%
#'   missing_pairs(dependent, explanatory)
#' }
missing_pairs = function(.data, dependent = NULL, explanatory = NULL,
                         use_labels = TRUE,
												 title=NULL,
												 position = "stack",
                         showXAxisPlotLabels = TRUE,
                         showYAxisPlotLabels = FALSE){
  if (is.null(dependent) && is.null(explanatory)) {
    df.in = .data
  }
  else {
    df.in = .data %>%
      dplyr::select(dependent, explanatory)
  }
  vars_n = length(df.in)
  df.miss = missing_df(df.in)
  df.plot = data.frame(df.in, df.miss)
  obs_vector = rep(names(df.in), vars_n)
  miss_vector = rep(names(df.miss), each=vars_n)
  if(use_labels){
    labels = extract_labels(df.in)$vfill
  }else{
    labels = extract_labels(df.in)$vname
  }

  # Plot title
  if(is.null(title)) title = paste0("Missing data matrix")

  # Everything below can be made into functions
  obs_discrete = sapply(df.in, function(x){
    any(is.factor(x),
        is.character(x),
        is.logical(x))
  })
  obs_discrete_vector = rep(obs_discrete, vars_n)

  # Make colours permanent
  palColours = c("lightblue", "gray50")
  names(palColours) = c("Obs", "Miss")
  colScale = scale_fill_manual(values=palColours)

  # Make list of plots
  plot_list = purrr::pmap(list(obs_vector, miss_vector, obs_discrete_vector),
                          function(obs, miss, discrete){
                            if(!discrete){
                              ggplot(data = df.plot) +
                                geom_boxplot(aes_string(x=miss, y=obs, fill=miss))+
                                colScale+
                                scale_x_discrete(limits=c("Miss", "Obs"))+
                                coord_flip()
                              #	geom_density(aes_string(x = miss), colour = "darkblue")
                            }else{
                              ggplot(data = df.plot, aes_string(x = obs, fill=miss)) +
                                geom_bar(position=position)+
                                colScale

                            }
                          })

  # Plot matrix
  GGally::ggmatrix(plot_list, nrow=vars_n, ncol=vars_n,
                   xAxisLabels = labels,
                   yAxisLabels = paste(labels, "(miss)"),
                   showXAxisPlotLabels = showXAxisPlotLabels,
                   showYAxisPlotLabels = showYAxisPlotLabels,
                   title = title)+

    theme_classic()
}
