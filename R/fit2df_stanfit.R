#' Extract \code{stanfit} model fit results to dataframe: \code{finalfit} model
#' extracters
#'
#' \code{fit2df.stanfit} is the model extract method for our standard Bayesian
#' hierarchical binomial logistic regression models. These models will be fully
#' documented separately. However this should work for a single or multilevel
#' Bayesian logistic regression done in Stan, as long as the fixed effects are
#' specified in the parameters block as a vector named \code{beta}, of length
#' \code{P}, where \code{P} is the number of fixed effect parameters. e.g.
#' parameters{ vector[P] beta; }
#'
#' @rdname fit2df
#' @method fit2df stanfit
#' @export
#'
fit2df.stanfit = function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                          explanatory_name = "explanatory",
                          estimate_name = "OR",
                          estimate_suffix = "",
                          p_name = "p",
                          digits=c(2,2,3), confint_sep = "-", ...){
  args = list(...)

  if(is.null(args$X)) stop("Must include design matrix from Stan procedure, e.g. X=X")

  df.out = extract_fit(.data=.data, explanatory_name=explanatory_name,
                       estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                       p_name=p_name, digits=digits, X=args$X)

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
    # n_data = dim(x$data)[1] # no equivalent here
    n_model = dim(args$X)[1]
    # aic = round(x$aic, 1) # add WAIC later?
    # auc = round(roc(x$y, x$fitted)$auc[1], 3) # Add predicted mu later?
    metrics.out = paste0(
      #	"Number in dataframe = ", n_data,
      ", Number in model = ", n_model)
    #	", Missing = ", n_data-n_model,
    #	", AIC = ", aic,
    #	", C-statistic = ", auc)
  }

  if (metrics==TRUE){
    return(list(df.out, metrics.out))
  } else {
    return(df.out)
  }
  return(df.out)
}
