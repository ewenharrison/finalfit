#' Extract \code{lmerMod} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.lmerMod} is the model extract method for standard
#' \code{lme4::\link[lme4]{lmer}} models and for the
#' \code{finalfit::\link{lmmixed}} model wrapper.
#'
#' @rdname fit2df
#' @method fit2df lmerMod
#' @export

fit2df.lmerMod = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
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
    n_model = length(x@resp$mu)
    n_groups = summary(x)$ngrps
    loglik = round(summary(x)$logLik, 2)
    aic = round(summary(x)$AICtab[[1]], 1)
    metrics.out = paste0(
      "Number in model = ", n_model,
      ", Number of groups = ", paste(n_groups, collapse="/"),
      ", Log likelihood = ", loglik,
      ", REML criterion = ", aic)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
}
