#' Extract \code{glm} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.glm} is the model extract method for standard
#' \code{\link[stats]{glm}} models, which have not used \code{finalfit} model
#' wrappers.
#'
#' @rdname fit2df
#' @method fit2df glm
#' @export
#'
fit2df.glm <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                       explanatory_name = "explanatory",
                       estimate_name = "OR",
                       estimate_suffix = "",
                       p_name = "p",
                       digits=c(2,2,3), confint_sep = "-", ...){

  df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
                       estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                       p_name=p_name, digits=digits)

  if (condense==TRUE){
    df.out = condense_fit(df.out, explanatory_name=explanatory_name,
                          estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                          p_name=p_name, digits=digits, confint_sep=confint_sep)
  }

  if (remove_intercept==TRUE){
    df.out = remove_intercept(df.out)
  }

  # Extract model metrics
  if (metrics==TRUE){
    x = .data
    n_data = dim(x$data)[1]
    n_model = dim(x$model)[1]
    aic = round(x$aic, 1)
    auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
    metrics.out = paste0(
      "Number in dataframe = ", n_data,
      ", Number in model = ", n_model,
      ", Missing = ", n_data-n_model,
      ", AIC = ", aic,
      ", C-statistic = ", auc)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
}
