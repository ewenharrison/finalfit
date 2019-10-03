#' Compare bootstrapped distributions
#'
#' Not usually called directly. Included in \code{\link{boot_predict}}. Usually used in combination with A function that takes the output from \code{\link{summary_factorlist}(...,
#' fit_id=TRUE)} and merges with any number of model dataframes, usually
#' produced with a model wrapper followed by the \code{\link{fit2df}()} function
#' (see examples).
#'
#' @param bs.out Output from \code{boot::boot},
#' @param confint_sep String separating lower and upper confidence interval
#' @param condense Logical. FALSE gives numeric values, usually for plotting.
#'   TRUE gives table for final output.
#' @param compare_name Name to be given to comparison metric.
#' @param comparison Either "difference" or "ratio".
#' @param ref_symbol Reference level symbol
#' @param digits Rounding for estimate values and p-values, default c(2,3).
#' @return A dataframe of first differences or ratios for boostrapped distributions of a metric of interest.
#'
#' \code{finalfit} predict functions
#'
#' @seealso \code{\link{boot_predict}} \code{\link{finalfit_newdata}}
#' @export
#'
#' @examples
#' # See boot_predict.

boot_compare = function(bs.out, confint_sep = " to ", comparison = "difference", condense=TRUE,
                        compare_name = NULL, digits = c(2, 3), ref_symbol = 1){

  if(is.null(compare_name)){
    compare_name = paste0(toupper(substring(comparison, 1, 1)), substring(comparison, 2))
  }

  bs_1 = bs.out$t[,1]
  bs_c = bs.out$t[,-1]

  if(comparison == "difference"){
    estimate = bs_c - bs_1
    null_ref = 0
  }else if(comparison == "ratio"){
    estimate = bs_c / bs_1
    null_ref = 1
  }

  if(is.null(dim(estimate))) estimate = matrix(estimate, ncol=1) #allow single vector to pass to apply

  estimate_centre = apply(estimate, 2, median)
  estimate_conf.low = apply(estimate, 2, quantile, probs = c(0.025))
  estimate_conf.high = apply(estimate, 2, quantile, probs = c(0.975))
  estimate_p1 = apply(estimate, 2, function(x) mean(x < null_ref ))
  estimate_p2 = apply(estimate, 2, function(x) mean(x > null_ref ))
  estimate_p3 = apply(estimate, 2, function(x) mean(x == null_ref ))
  estimate_p = apply(rbind(estimate_p1, estimate_p2), 2, min)
  estimate_p = ifelse(estimate_p3==1, 1, estimate_p)
  estimate_p = apply(rbind(estimate_p*2, 1), 2, min)  #two-tailed, max 1

  if(condense==FALSE){
    df.out = data.frame(estimate_centre, estimate_conf.low, estimate_conf.high, estimate_p,
                        stringsAsFactors=FALSE)
    colnames(df.out) = c(comparison, paste0(comparison, "_conf.low"),
                         paste0(comparison, "_conf.high"), paste0(comparison, "_p"))
    df.out = rbind(null_ref, df.out)
  }else if(condense==TRUE){
    estimate_centre = round_tidy(estimate_centre, digits[1])
    estimate_conf.low = round_tidy(estimate_conf.low, digits[1])
    estimate_conf.high = round_tidy(estimate_conf.high, digits[1])
    estimate_p = p_tidy(estimate_p, digits[2])
    df.out = paste0(estimate_centre, " (", estimate_conf.low, confint_sep,
                    estimate_conf.high, ", p", estimate_p, ")")
    df.out = c(ref_symbol, df.out)
    df.out = data.frame(df.out)
    colnames(df.out) = compare_name
  }
  return(df.out)
}
