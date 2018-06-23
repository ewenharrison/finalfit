#' Extract \code{coxphuni} and \code{coxphmulti} model fit results to dataframe: \code{finalfit} model extracters
#'
#' \code{fit2df.coxphlist} is the model extract method for \code{coxphuni} and \code{coxphmulti}.
#'
#' @rdname fit2df
#' @method fit2df coxphlist
#' @export

fit2df.coxphlist <- function(.data, condense=TRUE, metrics=FALSE, remove_intercept=TRUE,
                             explanatory_name = "explanatory",
                             estimate_name = "HR",
                             estimate_suffix = "",
                             p_name = "p",
                             digits=c(2,2,3), confint_sep = "-", ...){
  if(metrics==TRUE) warning("Metrics not currently available for this model")

  df.out = plyr::ldply(.data, .id = NULL, extract_fit, explanatory_name=explanatory_name,
                       estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                       p_name=p_name, digits=digits)

  if (condense==TRUE){
    df.out = condense_fit(.data=df.out, explanatory_name=explanatory_name,
                          estimate_name=estimate_name, estimate_suffix=estimate_suffix,
                          p_name=p_name, digits=digits, confint_sep=confint_sep)
  }
  return(df.out)
}
