#' Plot survival curves with number-at-risk table
#'
#' Produce a survival curve plot and number-at-risk table using \code{survminer::ggsurvplot}
#'   and \code{finalfit} conventions.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  Survival object of the form \code{Surv(time, status)}.
#' @param explanatory Character vector of max length 2: quoted name(s) of explanatory variables.
#' @param ... Arguments passed to \code{\link[survminer]{ggsurvplot}}.
#' @return Returns a table and plot produced in \code{ggplot2}.
#'
#' @family finalfit plot functions
#' @export
#' @import ggplot2
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' # Survival plot
#' data(colon_s)
#' explanatory = c("perfor.factor")
#' dependent = "Surv(time, status)"
#' colon_s %>%
#'   surv_plot(dependent, explanatory, xlab="Time (days)", pval=TRUE, legend="none")
#'
surv_plot = function(.data, dependent, explanatory, ...){
  if(length(explanatory)>2){
    stop("Explanatory must have a maximum of two variables")
  }

  args = list(...)
  .formula = as.formula(paste0(dependent, "~", paste(explanatory, collapse="+")))
  args$fit = substitute(survival::survfit(.formula, data=.data), list(.formula=.formula))
  args$data=.data

  # Defaults which can be modified via ...
  if (is.null(args$xlab)) args$xlab="Time"
  if (is.null(args$ylab)) args$ylab="Probability"
  if (is.null(args$censor)) args$censor=FALSE
  if (is.null(args$conf.int)) args$conf.int=FALSE
  if (is.null(args$risk.table)) args$risk.table=TRUE
  if (is.null(args$linetype)) args$linetype="strata"
  if (is.null(args$palette)) args$palette="lancet"
  if (is.null(args$legend.title)) args$legend.title=""
  if (is.null(args$font.x)) args$font.x=14
  if (is.null(args$font.y)) args$font.y=14
  if (is.null(args$font.tickslab)) args$font.tickslab=12
  if (is.null(args$ggtheme)) args$ggtheme=theme_classic()

  ggsurv = do.call(
    survminer::ggsurvplot, args
  )
  ggsurv$table = ggsurv$table + survminer::theme_cleantable()
  return(ggsurv)
}
