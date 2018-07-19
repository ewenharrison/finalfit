#' Extract \code{glmuni} and \code{glmmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.glmlist} is the model extract method for \code{glmuni} and \code{glmmulti}.
#'
#' @rdname fit2df
#' @method fit2df glmlist
#' @export

fit2df.glmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                           explanatory_name = "explanatory",
                           estimate_name = "OR",
                           estimate_suffix = "",
                           p_name = "p",
                           digits=c(2,2,3), confint_sep = "-", ...){

  if (metrics==TRUE && length(.data)>1){
    stop("Metrics only generated for single models: multiple models supplied to function")
  }

  df.out = plyr::ldply(.data, .id = NULL, extract_fit, explanatory_name=explanatory_name,
                       estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                       p_name=p_name, digits=digits)

  if (condense==TRUE){
    df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
                          estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                          p_name=p_name, digits=digits, confint_sep=confint_sep)
  }

  if (remove_intercept==TRUE){
    df.out = remove_intercept(df.out)
  }

  # Extract model metrics
  if (metrics==TRUE){
    x = .data[[1]]
    n_data = dim(x$data)[1]
    n_model = dim(x$model)[1]
    aic = round(x$aic, 1)
    auc = round(pROC::roc(x$y, x$fitted)$auc[1], 3)
    h_l = metrics_hoslem(x$y, x$fitted)
    metrics.out = paste0(
      "Number in dataframe = ", n_data,
      ", Number in model = ", n_model,
      ", Missing = ", n_data-n_model,
      ", AIC = ", aic,
      ", C-statistic = ", auc,
      ", H&L = ", h_l)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
}
