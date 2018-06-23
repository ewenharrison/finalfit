#' Extract \code{lmuni} and \code{lmmulti} model fit results to dataframe:
#' \code{finalfit} model extracters
#'
#' \code{fit2df.lmlist} is the model extract method for \code{lmuni} and
#' \code{lmmulti}.
#'
#' @rdname fit2df
#' @method fit2df lmlist
#' @export

fit2df.lmlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                          explanatory_name = "explanatory",
                          estimate_name = "Coefficient",
                          estimate_suffix = "",
                          p_name = "p",
                          digits=c(2,2,3), confint_sep = " to ", ...){

  if (metrics==TRUE && length(.data)>1){
    stop("Metrics only generated for single models: multiple models supplied to function")
  }

  df.out = plyr::ldply(.data, .id = NULL, extract_fit, explanatory_name=explanatory_name,
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
    x = .data[[1]]
    n_model = dim(x$model)[1]
    n_missing = length(summary(x)$na.action)
    n_data = n_model+n_missing
    n_model = dim(x$model)[1]
    loglik = round(logLik(x), 2)
    r.squared = signif(summary(x)$r.squared, 2)
    adj.r.squared = signif(summary(x)$adj.r.squared, 2)
    metrics.out = paste0(
      "Number in dataframe = ", n_data,
      ", Number in model = ", n_model,
      ", Missing = ", n_missing,
      ", Log-likelihood = ", loglik,
      ", R-squared = ", r.squared,
      ", Adjusted r-squared = ", adj.r.squared)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
}
