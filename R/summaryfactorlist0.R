#' Summarise a set of factors by a dependent variable
#'
#' Internal function, not called directly.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  name of depdendent variable
#'   (2 to 5 factor levels).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param cont Summary for continuous variables: mean (standard deviation) or
#'   median (interquartile range).
#' @param p Logical: Include statistical test (see
#'   \code{\link[Hmisc]{summary.formula}}).
#' @param na_include Logical: include missing data in summary (\code{NA}).
#' @param column Logical: Compute margins by column rather than row.
#' @param total_col Logical: include a total column summing across factor
#'   levels.
#' @param orderbytotal Logical: order final table by total column high to low.
#' @param fit_id Logical: not used directly, allows merging via
#'   \code{\link{finalfit_merge}}.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when
#'   \code{na_include=TRUE}.
#' @param add_dependent_label Add the name of the dependent label to the top
#'   left of table
#' @return Returns a \code{factorlist} dataframe.
#'
#' @keywords internal

summary_factorlist0 <- function(.data, dependent, explanatory,  cont="mean", p=FALSE, na_include=FALSE,
                                column=FALSE, total_col=FALSE, orderbytotal=FALSE, fit_id=FALSE,
                                na_to_missing = TRUE, add_dependent_label=FALSE,
                                dependent_label_prefix="Dependent: ", dependent_label_suffix=""){

  s = Hmisc:::summary.formula(as.formula(paste(dependent, "~", paste(explanatory, collapse="+"))), data = .data,
                              overall=FALSE, method="response", na.include=na_include, continuous=5,
                              fun=function(x) {
                                mean = mean(x)
                                sd = sd(x)
                                L = quantile(x, probs=c(0.25))[[1]]
                                median = quantile(x, probs=c(0.5))[[1]]
                                U = quantile(x, probs=c(0.75))[[1]]
                                return(data.frame(mean, sd, L, median, U))
                              }
  )

  # Dataframe
  df.out = data.frame(label=attr(s, "vlabel"), levels=attr(s, "dimnames")[[1]])

  # Add in lm level names, this needs hacked in given above methodology
  if (fit_id){
    vname = attr(s, "vname")
    vname_logical = (vname == "")
    for (i in 1:length(vname)){
      if(vname_logical[i]) vname[i] = vname[i-1]
    }
    levels = as.character(df.out$levels)
    df.out$fit_id = paste0(vname, levels)
    df.out$index = 1:dim(df.out)[1]
  }

  if (cont=="mean"){
    mean.out = sprintf("%.1f", matrix(s[,2]))
    sd.out = sprintf("%.1f", matrix(s[,3]))
    result.out = data.frame(paste0(mean.out, " (", sd.out, ")"))
    colnames(result.out) = "Mean (sd)"
  }

  if (cont=="median"){
    median.out = sprintf("%.1f", matrix(s[,5]))
    L_IQR = sprintf("%.1f", matrix(s[,4]))
    U_IQR = sprintf("%.1f", matrix(s[,6]))
    result.out = data.frame(paste0(median.out, " (", L_IQR, " to ", U_IQR, ")"))
    colnames(result.out) = "Median (IQR)"
  }

  df.out = cbind(df.out, result.out)

  # Add dependent name label
  if(add_dependent_label){
    df.out = dependent_label(df.out=df.out, .data=.data, dependent,
                             prefix=dependent_label_prefix, suffix = dependent_label_suffix)
  }


  return(df.out)
}
