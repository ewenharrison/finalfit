#' Hosmer-Lemeshow goodness of fit test
#'
#' Internal, not usually called directly
#'
#' @param y Observed y, usually of the form \code{fit$y}.
#' @param yhat Predicted y_hat, usually for the form \code{fit$fitted}
#' @param g Number of bins to calculate quantiles.
#' @param digits Number of decimal places of form \code{c(2,3)}, where \code{digits[1]} is
#'   for chi-sq estimate and \code{digits[2]} is for p-value.
#'
#' @return Character string of chi-sq result, df, and p-value. Significant
#'   p-value suggests poor fit.
#' @export
#'
#' @author Adapted from Peter Solymos.
#' @source https://github.com/psolymos/ResourceSelection/blob/master/R/hoslem.test.R
#'
#' @examples
metrics_hoslem <- function(y, yhat, g=10, digits = c(2,3)) {
  qq <- unique(quantile(yhat, probs=seq(0, 1, 1/g)))
  yhat_cut <- cut(yhat, breaks = qq, include.lowest = TRUE)
  observed <- xtabs(cbind("y0" = 1-y, "y1" = y) ~ yhat_cut)
  expected <- xtabs(cbind("yhat0" = 1-yhat, "yhat1" = yhat) ~ yhat_cut)
  chisq <- sum((observed - expected)^2 / expected)
  p = 1 - pchisq(chisq, g-2)
  par <- g-2
  out = paste0("Chi-sq(", par, ") ", round_tidy(chisq, digits = digits[1]), " (p", p_tidy(p, digits = digits[2]), ")")
  return(out)
}
