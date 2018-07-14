#' Merge a \code{\link{summary_factorlist}()} table with any number of model
#' results tables.
#'
#' A function that takes the output from \code{\link{summary_factorlist}(...,
#' fit_id=TRUE)} and merges with any number of model dataframes, usually
#' produced with a model wrapper followed by the \code{\link{fit2df}()} function
#' (see examples).
#'
#' @param factorlist Output from \code{\link{summary_factorlist}(...,
#'   fit_id=TRUE)}.
#' @param fit2df_df Output from model wrappers followed by
#'   \code{\link{fit2df}()}.
#' @param ref_symbol Reference symbol for model reference levels, typically "-"
#'   or "1.0".
#' @param estimate_name If you have chosen a new `estimate name` (e.g. "Odds
#'   ratio") when running a model wrapper (e.g. `glmuni`), then you need to pass
#'   this new name to `finalfit_merge` to generate correct table.
#'   Defaults to OR/HR/Coefficient
#' @return Returns a dataframe of combined tables.
#'
#' @seealso \code{\link{summary_factorlist}} \code{\link{fit2df}}
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' data(colon_s)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#'
#' # Create separate tables
#' colon_s %>%
#'   summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)") -> example.univariable
#'
#' colon_s %>%
#' 	glmmulti(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' colon_s %>%
#' 	glmmixed(dependent, explanatory, random_effect) %>%
#' 	fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#' 	finalfit_merge(example.univariable) %>%
#' 	finalfit_merge(example.multivariable) %>%
#' 	finalfit_merge(example.multilevel) %>%
#' 	select(-c(fit_id, index)) -> example.final
#' example.final

finalfit_merge = function(factorlist, fit2df_df, ref_symbol = "-", estimate_name=NULL){
  if(is.null(factorlist$fit_id)) stop("Include fit_id=TRUE in summary_factorlist()")
  explanatory_name = names(fit2df_df)[1]
  or_col_id = ifelse(is.null(estimate_name), "Coefficient|OR|HR", paste0(estimate_name, "|Coefficient|OR|HR"))
  or_col = grep(or_col_id, names(fit2df_df), value=TRUE)
  df.out = merge(factorlist, fit2df_df, by.x = "fit_id", by.y = explanatory_name, all = TRUE)
  df.out[,or_col] = as.character(df.out[,or_col])
  df.out[is.na(df.out[,or_col]),or_col] = ref_symbol
  df.out = df.out[order(df.out$index),]
  return(df.out)
}

#' @rdname finalfit_merge
ff_merge = finalfit_merge
