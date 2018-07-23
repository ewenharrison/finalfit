#' Generate newdata for simulations
#'
#' Generate newdata while respecting the variable types and factor levels in the
#' primary data frame used to run model.
#'
#' Generate model predictions against a specified set of explanatory levels with
#' bootstrapped confidence intervals. Add a comparison by difference or ratio of
#' the first row of \code{newdata} with all subsequent rows.
#'
#' @param .data Dataframe.
#' @param dependent Optional character vector of length 1:  name of depdendent
#'   variable. Not usually specified in bootstrapping model predictions.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param rowwise Logical. Format \code{newdata} is provided in.
#' @param newdata A list of rows or columns coresponding exactly to the order of
#'   explanatory variables. Useful errors generated if requirements not fulfilled
#' @return A list of multivariable \code{\link[stats]{glm}} fitted model
#'   outputs. Output is of class \code{glmlist}.
#'
#' @seealso \code{\link{boot_predict}} \code{\link{boot_compare}}
#' @export
#'
#' @examples
#' # See boot_predict.
#' library(finalfit)
#' library(dplyr)
#'
#' # Predict probability of death across combinations of factor levels
#' explanatory = c("age.factor", "extent.factor", "perfor.factor")
#' dependent = 'mort_5yr'
#'
#' # Generate combination of explanatory variable levels rowwise
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, newdata = list(
#'     c("<40 years",  "Submucosa", "No"),
#'     c("<40 years", "Submucosa", "Yes"),
#'     c("<40 years", "Adjacent structures", "No"),
#'     c("<40 years", "Adjacent structures", "Yes")
#'    )) -> newdata
#'
#' # Generate combination of explanatory variable levels colwise.
#' explanatory = c("nodes", "extent.factor", "perfor.factor")
#' colon_s %>%
#'   finalfit_newdata(explanatory = explanatory, rowwise = FALSE, newdata = list(
#'   rep(seq(0, 30), 4),
#'   c(rep("Muscle", 62), rep("Adjacent structures", 62)),
#'   c(rep("No", 31), rep("Yes", 31), rep("No", 31), rep("Yes", 31))
#' )) -> newdata
#'

ff_newdata = function(.data, dependent=NULL, explanatory=NULL,  rowwise=TRUE, newdata){
  .data %>%
    dplyr::select(dependent, explanatory) %>%
    dplyr::slice(0) -> df.out

  is_numeric = sapply(df.out, is.numeric)

  if(rowwise){
    df.new = do.call(rbind.data.frame, newdata)
    df.new[ ,is_numeric] = as.numeric(as.character(df.new[ ,is_numeric]))
  }else{
    df.new = do.call(data.frame, newdata)
  }

  df.out[1:dim(df.new)[1],] = df.new
  return(df.out)
}

#' @rdname ff_newdata
finalfit_newdata = ff_newdata


#' @rdname ff_newdata
#' @export
finalfit_newdata = ff_newdata
