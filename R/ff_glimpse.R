#' Descriptive statistics for dataframe
#'
#' Everyone has a funcion like this, str, glimpse, glance etc. This one is
#' specifically designed for use with \code{finalfit} language. It is different
#' in dividing variables by numeric vs factor, the former using
#' \code{link[psych]{describe}}.
#'
#' @param .data Dataframe.
#' @param dependent Optional character vector: name(s) of depdendent
#'   variable(s).
#' @param explanatory Optional character vector: name(s) of explanatory
#'   variable(s).
#' @param digits Significant digits for continuous variable summaries
#'
#' @return Dataframe on summary data.
#' @export
#'
#' @examples
#' library(finalfit)
#' dependent = 'mort_5yr'
#' explanatory = c("age.factor", "extent.factor", "perfor.factor")
#' colon_s %>%
#'   finalfit_glimpse(dependent, explanatory)

ff_glimpse <- function(.data, dependent=NULL, explanatory=NULL, digits = 1){
  if(is.null(dependent) && is.null(explanatory)){
    df.in = .data
  }else{
    .data %>%
      dplyr::select(dependent, explanatory) -> df.in
  }

  # Continuous
  df.in %>%
    dplyr::select_if(is.numeric) -> df.numeric

  df.numeric %>%
    broom::tidy(na.rm = TRUE, interp=FALSE,skew = FALSE, ranges = TRUE,
                check=TRUE,fast=F, omit=FALSE) %>%
    format(digits = digits, scientific=FALSE) -> df.numeric.out1

  df.numeric %>%
    lapply(function(x){
      label = attr(x, "label")
      list(label=label)
    }) %>%
    do.call(rbind, .) -> df.numeric.out2

  df.numeric.out = cbind(df.numeric.out1, df.numeric.out2)
  df.numeric.out = df.numeric.out[,c(1,9, 2:7)]

  # Factors
  df.in %>%
    dplyr::select_if(Negate(is.numeric)) %>%
    lapply(function(x){
      n = which(!is.na(x)) %>% length()
      label = attr(x, "label")
      levels_n = length(levels(x))
      levels = ifelse(is.factor(x),
                      levels(x) %>%
                        paste(collapse=", "),
                      "-")
      levels_count = ifelse(is.factor(x),
                            summary(x) %>%
                              paste(collapse = ", "),
                            "-")
      levels_percent = ifelse(is.factor(x),
                              summary(x) %>%
                                prop.table() %>%
                                `*`(100) %>%
                                format(digits = 2) %>%
                                paste(collapse = ", "),
                              "-")
      list(label=label, levels=levels, level_n=levels_n, n=n,
           levels_count=levels_count, levels_percent = levels_percent)
    }
    ) %>%
    do.call(rbind, .) %>%
    data.frame() -> df.factors.out

  df.factors.out$column = rownames(df.factors.out)
  rownames(df.factors.out) <- c()
  df.factors.out = df.factors.out[,c(7, 1:6)]

  cat("Numerics\n")
  print(df.numeric.out, row.names=FALSE)
  cat("Factors\n")
  print(df.factors.out, row.names=FALSE)

  return(invisible(
    list(
      numerics = df.numeric.out,
      factors = df.factors.out))
  )
}

#' @rdname ff_glimpse
finalfit_glimpse <- ff_glimpse
