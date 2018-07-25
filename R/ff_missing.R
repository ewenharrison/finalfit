#' Characterise missing data for \code{finalfit} models
#'
#' Using \code{finalfit} conventions, produces a missing data matrix using
#' \code{\link[mice]{md.pattern}}.
#'
#' @param .data Dataframe. Missing values must be coded \code{NA}.
#' @param dependent Character vector usually of length 1, name of depdendent
#'   variable.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#'   to a missing data pattern (1=observed, 0=missing). Rows and columns are
#'   sorted in increasing amounts of missing information. The last column and
#'   row contain row and column counts, respectively.
#'
#' @export
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	finalfit_missing(dependent, explanatory)
#'
ff_missing = function(.data, dependent=NULL, explanatory=NULL){
  if(is.null(dependent) && is.null(explanatory)){
    df.in = .data
  }else{
    keep = names(.data) %in% c(dependent, explanatory)
    df.in = .data[keep]
  }
  mice::md.pattern(df.in)
}

#' @rdname ff_missing
#' @export
finalfit_missing = ff_missing
