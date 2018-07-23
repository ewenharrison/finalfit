# Functions used in fit2df() ----

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Model output.
#' @param explanatory_name Name for this column in output.
#' @param estimate_name Name for this column in output.
#' @param estimate_suffix Appeneded to estimate name.
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param ... Other arguments.
#'
#' @keywords internal
#' @export

extract_fit = function(.data, explanatory_name, estimate_name,
                       estimate_suffix,  p_name, digits, ...){
  UseMethod("extract_fit")
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit glm
#' @export

extract_fit.glm = function(.data, explanatory_name="explanatory", estimate_name="OR",
                           estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(coef(x))
  estimate = exp(coef(x))
  confint = exp(confint(x))
  p = summary(x)$coef[,"Pr(>|z|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit glmerMod
#' @export

extract_fit.glmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
                                estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(lme4::fixef(x))
  estimate = exp(lme4::fixef(x))
  confint = exp(lme4::confint.merMod(x, method='Wald'))
  confint = confint[-grep("sig", rownames(confint)),]
  p = summary(x)$coef[,"Pr(>|z|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)

}


#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit lm
#' @export

extract_fit.lm = function(.data, explanatory_name="explanatory", estimate_name="Coefficient",
                          estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(coef(x))
  estimate = coef(x)
  confint = confint(x)
  p = summary(x)$coef[,"Pr(>|t|)"]

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit lmerMod
#' @export

extract_fit.lmerMod = function(.data, explanatory_name="explanatory", estimate_name="OR",
                               estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  explanatory = names(lme4::fixef(x))
  estimate = exp(lme4::fixef(x))
  confint = exp(lme4::confint.merMod(x, method='Wald'))
  confint = confint[-grep("sig", rownames(confint)),]
  p = 1-pnorm(abs(summary(x)$coefficients[,3]))
  warning("P-value for lmer is estimate assuming t-distribution is normal. Bootstrap for final publication.")

  df.out = data.frame(explanatory, estimate, confint[,1], confint[,2], p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit coxph
#' @export

extract_fit.coxph = function(.data, explanatory_name="explanatory", estimate_name="HR",
                             estimate_suffix = "",  p_name = "p", digits=c(2,2,3), ...){
  x=.data

  results = summary(x)$conf.int
  explanatory = row.names(results)
  estimate = results[,1]
  confint_L = results[,3]
  confint_U = results[,4]
  p = summary(x)$coefficients[explanatory,
                              max(dim(summary(x)$coefficients)[2])] # Hack to get p fe and re
  df.out = data.frame(explanatory, estimate, confint_L, confint_U, p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}


#' Extract model output to dataframe
#'
#' Internal function, not called directly.
#'
#' @param X Design matrix from Stan modelling procedure.
#'
#' @keywords internal
#' @rdname extract_fit
#' @method extract_fit stanfit
#' @export

extract_fit.stanfit = function(.data, explanatory_name="explanatory", estimate_name="OR",
                               estimate_suffix = "",  p_name = "p", digits=c(2,2,3), X, ...){
  stanfit = .data
  pars = "beta"
  quantiles =  c(0.025, 0.50, 0.975)

  explanatory = attr(X, "dimnames")[[2]]
  results = rstan::summary(stanfit,
                           pars = pars,
                           probs = quantiles)$summary
  estimate = exp(results[, 1])
  confint_L = exp(results[, 4])
  confint_U = exp(results[, 6])

  # Determine a p-value based on two-sided examination of chains
  chains = rstan::extract(stanfit, pars=pars, permuted = TRUE, inc_warmup = FALSE,
                          include = TRUE)
  p1.out = apply(chains[[1]], 2, function(x)mean(x<0))
  p2.out = apply(chains[[1]], 2, function(x)mean(x>0))
  p1.out = p1.out*2
  p2.out = p2.out*2
  p.out = ifelse(p1.out < 1, p1.out, p2.out)
  p = round(p.out, 3)

  df.out = data.frame(explanatory, estimate, confint_L, confint_U, p)
  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix), "L95", "U95", p_name)
  return(df.out)
}

#' Extract variable labels from dataframe
#'
#' Internal function, not called directly.
#'
#' @param .data Dataframe containing labelled variables.
#'
#' @keywords internal
#' @export

extract_variable_label = function(.data){
  sapply(colnames(.data), function(x){
    label = attr(.data[,x], "label")
    ifelse(is.null(label), x, label)
  })
}

#' Condense model output dataframe for final tables
#'
#' Internal function, not called directly. Can only be used in conjunction with
#'   extract_fit
#'
#' @param .data Dataframe of five columns, must be this order, (1) explanatory
#'   variable names, (2) estimate, (3) confidence interval lower limit, (4)
#'   confidence interval upper limit, (5) p-value.
#' @param explanatory_name Name for this column in output
#' @param estimate_name Name for this column in output
#' @param estimate_suffix Appeneded to estimate name
#' @param p_name Name given to p-value estimate
#' @param digits Number of digits to round to (1) estimate, (2) confidence
#'   interval limits, (3) p-value.
#' @param confint_sep String to separate confidence intervals, typically "-" or
#'   " to ".
#'
#' @keywords internal
#' @export

condense_fit = function(.data, explanatory_name="explanatory", estimate_name="OR",
                        estimate_suffix = "", p_name = "p",
                        digits=c(2,2,3), confint_sep = "-"){
  x = .data
  d.estimate = digits[1]
  d.confint = digits[2]
  d.p = digits[3]

  explanatory = x[,1]
  estimate = round_tidy(x[,2], d.estimate)
  confint_low = round_tidy(x[,3], d.confint)
  confint_high = round_tidy(x[,4], d.confint)
  p = p_tidy(x[,5], d.p)

  df.out = data.frame(
    explanatory,
    paste0(
      estimate, " (",
      confint_low, confint_sep,
      confint_high, ", ",
      p_name, p, ")"))

  colnames(df.out) = c(explanatory_name, paste0(estimate_name, estimate_suffix)
  )
  return(df.out)
}

#' Round values but keep trailing zeros
#'
#' Internal function, not called directly
#'
#' e.g. for 3 decimal places I want 1.200, not 1.2.
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @return Vector of strings.
#'
#' @keywords internal
#' @export

round_tidy = function(x, digits){
  sprintf.arg = paste0("%.", digits, "f")
  x.out = do.call(sprintf, list(sprintf.arg, x)) # keep trailing zeros
  return(x.out)
}

#' Round p-values but keep trailing zeros
#'
#' Internal function, not called directly
#'
#' e.g. for 3 decimal places I want 0.100, not 0.1. Note this function with
#' convert 0.000 to <0.001. All other values are prefixed with "=" by default
#'
#' @param x Numeric vector of values to round
#' @param digits Integer of length one: value to round to.
#' @param prefix Appended in front of values for use with \code{condense_fit}.
#' @return Vector of strings.
#'
#' @keywords internal
#' @export

p_tidy = function(x, digits, prefix="="){
  x.out = paste0(prefix, round_tidy(x, digits))
  all_zeros = paste0(prefix, round_tidy(0, digits))
  less_than = paste0("<", format(10^-digits, scientific=FALSE))
  x.out[x.out == all_zeros] = less_than
  return(x.out)
}

#' Remove intercept from model output
#'
#' Internal function, not called directly
#'
#' @param .data Numeric vector of values to round
#' @param intercept_name Name given to interept in model. Should never have to
#'   change from default.
#' @return Vector of strings.
#'
#' @keywords internal
#' @export

# Tried to do this with dplyr programming and failed miserably.
# quo() enquo() !! all a bit of a nightmare
# So let's square bracket away!
remove_intercept = function(.data, intercept_name = "(Intercept)"){
  .data = .data[-which(.data[,1] == intercept_name),]
  return(.data)
}

#' Remove duplicate levels within \code{\link{summary_factorlist}}: \code{finalfit} helper function
#'
#' Not called directly.
#'
#' @param factorlist A factorlist intermediary.
#' @param na_to_missing Logical: convert \code{NA} to 'Missing' when \code{na_include=TRUE}.
#' @return Returns a \code{factorlist} dataframe.
#'
#' @keywords internal
#' @export

rm_duplicate_labels = function(factorlist, na_to_missing = TRUE){
  x = factorlist
  duplicate_rows = duplicated(x$label)
  x$label = as.character(x$label)
  x$label[duplicate_rows] = ""
  if (any(names(x) %in% "p")){
    x$p[duplicate_rows] = ""
    x$p[x$p == "0.000"] = "<0.001"
  }
  if (na_to_missing == TRUE){
    x$levels = as.character(x$levels)
    x$levels[which(x$levels == "NA")] = "Missing"
  }
  return(x)
}

#' Make a label for the dependent variable
#'
#' Not usually called directly. Can be used to label final results dataframe.
#'
#' @param df.out Dataframe (results table) to be altered.
#' @param .data Original dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @return Returns the label for the dependent variable, if specified.
#' @export
#' @examples
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' explanatory_multi = c("age.factor", "obstruct.factor")
#' random_effect = "hospital"
#' dependent = 'mort_5yr'
#'
#' # Separate tables
#' colon_s %>%
#' 	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> example.summary
#'
#' colon_s %>%
#' 	glmuni(dependent, explanatory) %>%
#' 	fit2df(estimate_suffix=" (univariable)") -> example.univariable
#'
#' colon_s %>%
#' 	 glmmulti(dependent, explanatory) %>%
#' 	 fit2df(estimate_suffix=" (multivariable)") -> example.multivariable
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel") -> example.multilevel
#'
#' # Pipe together
#' example.summary %>%
#'   finalfit_merge(example.univariable) %>%
#'   finalfit_merge(example.multivariable) %>%
#' 	 finalfit_merge(example.multilevel) %>%
#' 	 select(-c(fit_id, index)) %>%
#' 	 dependent_label(colon_s, dependent) -> example.final
#'   example.final
dependent_label = function(df.out, .data, dependent, prefix = "Dependent: ", suffix=""){
  d_label = attr(.data[,which(names(.data) %in% dependent)], "label")

  if (is.null(d_label)){
    d_label = dependent
  } else {
    d_label = d_label
  }
  names(df.out)[1] = paste0(prefix, d_label, suffix)
  names(df.out)[2] = ""

  return(df.out)
}

#' Label plot title
#'
#' Not called directly.
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1:  quoted name of depdendent
#'   variable. Can be continuous, a binary factor, or a survival object of form
#'   \code{Surv(time, status)}
#' @param prefix Prefix for dependent label
#' @param suffix Suffix for dependent label
#'
#' @keywords internal
#' @export
plot_title = function(.data, dependent, dependent_label, prefix = "", suffix=""){
  if (is.null(dependent_label)){
    d_label = attr(.data[,which(names(.data) %in% dependent)], "label")
    if (is.null(d_label)){
      d_label = dependent
    } else {
      d_label = d_label
    }
  } else {
    d_label = dependent_label
  }
  out = paste0(prefix, d_label, suffix)
  return(out)
}

# Specify global variables
globalVariables(c("L95", "U95", "fit_id", "Total", "OR", "HR", "."))




# Workaround ::: as summary.formula not (yet) exported from Hmisc
`%:::%` = function (pkg, name){
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  get(name, envir = asNamespace(pkg), inherits = FALSE)
}

#' Call to Hmisc:::summary.formula
#'
#' Not called directly.
#'
#' @keywords internal
#' @import Hmisc
summary_formula = 'Hmisc' %:::% 'summary.formula'



#' psych:: describe interval only
#'
#' Internal, not normally called directly, though can be.
#'
#' @param .data Dataframe or matrix.
#' @param na.rm Logical. Remove missing data.
#' @param interp Logical. Interpolate median.
#' @param skew Logical. Include skew and kurtosis.
#' @param ranges Logical. Include range.
#' @param trim Drop the top and bottom trim fraction
#' @param type  Skew or kurtosis
#' @param check Logical. Include non-numeric variables
#' @param fast Not used.
#' @param quant Specify quantiles.
#' @param IQR Logical. Interquartile range.
#' @param omit Logical. Omit non-numerical variables.
#'
#' @return Dataframe.
#' @export
#'
#' @keywords internal
#'
#' @importFrom stats mad na.omit sd
#'
#' @references https://github.com/cran/psych/blob/master/R/describe.R
#' @author Procedures for Psychological, Psychometric, and Personality Research.
#'   Homepage: https://personality-project.org/r/psych
#'   https://personality-project.org/r/psych-manual.pdf
#'
ff_describe <- function (.data, na.rm=TRUE, interp=FALSE, skew=TRUE, ranges=TRUE, trim=.1, type=3,
                           check=TRUE, fast=NULL, quant=NULL, IQR=FALSE, omit=FALSE){
  keep_names <- names(.data)
  cl <- match.call()
  #first, define a local function
  valid <- function(x) {sum(!is.na(x))}
  if(!na.rm) x <- na.omit(x)   #just complete cases

  if(is.null(fast)) {
    if (prod(dim(.data)) > 10^7) {fast <- TRUE } else {fast <- FALSE}}  #the default is to use fast for large data sets
  if(fast) {skew <- FALSE
  }
  numstats <- 10 + length(quant)	+ IQR
  if ( NCOL(.data) < 2)  {if(is.data.frame(.data)) {      #
    if( !is.numeric(.data[,1])) {warning ("You were trying to describe a non-numeric data.frame or vector which describe converted to numeric.")
      .data[,1] <- as.numeric(.data[,])
    }

    .data <- .data[,1] }   #getting around the problem of single column data frames
    #do it for vectors or
    len  <- 1
    nvar <- 1
    stats = matrix(rep(NA,numstats),ncol=numstats)    #create a temporary array
    stats[1, 1] <-  valid(.data )
    stats[1, 2] <-  mean(.data, na.rm=na.rm )
    stats[1, 10] <- sd(.data,na.rm=na.rm)
    if(interp) {stats[1, 3] <- interp.median(.data,na.rm=na.rm  ) }  else {stats[1,3] <- median(.data,na.rm=na.rm) }
    stats[1,9] <- mean(.data,na.rm=na.rm, trim=trim)
    stats[1, 4] <-  min(.data, na.rm=na.rm )
    stats[1, 5] <-  max(.data, na.rm=na.rm )
    stats[1, 6] <-  skew(.data,na.rm=na.rm,type=type  )
    stats[1, 7] <-  mad(.data,na.rm=na.rm)
    stats[1, 8] <-  kurtosi(.data,na.rm=na.rm,type=type)
    vars <- 1
    if(!is.null(quant)) { Qnt <- quantile(.data,prob=quant,na.rm=TRUE)
    stats[1,(IQR+11):numstats] <- t(Qnt)}
    if(IQR) {Quart <- t(quantile(.data,prob=c(.25,.75),na.rm=TRUE))
    Iqr <- Quart[,2] -Quart[,1]
    stats[1,11] <- Iqr
    }
    rownames(stats) <- keep_names[1]
  } else {
    nvar <- ncol(.data)
    stats = matrix(rep(NA,nvar*numstats),ncol=numstats)    #create a temporary array

    if(is.null(colnames(.data))) colnames(.data) <- paste0("X",1:ncol(.data))
    rownames(stats) <- colnames(.data)
    stats[,1] <- apply(.data,2,valid)
    vars <- c(1:nvar)
    ##adapted from the pairs function to convert logical or categorical to numeric
    select <- 1:nvar

    if(!is.matrix(.data) && check) {  #does not work for matrices
      for(i in 1:nvar) {
        if(!is.numeric(.data[[i]] ))  {
          if(fast)  {.data[[i]] <- NA} else {
            if(omit) {select[i] <- NA}
            if(is.factor(unlist(.data[[i]])) | is.character(unlist(.data[[i]]))) {  .data[[i]] <- as.numeric(.data[[i]])
            rownames(stats)[i] <- paste(rownames(stats)[i],"*",sep="")

            } else {.data[[i]] <- NA}

          }
        }
      }
    }


    select <- select[!is.na(select)]

    .data <- as.matrix(.data[,select])
    vars <- vars[select]

    stats <- stats[select,]
    if(!is.numeric(.data)) {message("Converted non-numeric matrix input to numeric.  Are you sure you wanted to do this. Please check your data")
      .data <- matrix(as.numeric(.data),ncol=nvar)
      rownames(stats) <- paste0(rownames(stats),"*")}

    stats[,2] <- apply(.data, 2,mean,na.rm=na.rm )
    stats[,10] <- apply(.data,2,sd,na.rm=na.rm)


    if (skew) {stats[, 6] <-  skew(.data,na.rm=na.rm,type=type  )
    stats[,8] <- kurtosi(.data,na.rm=na.rm,type=type)}


    if(ranges) {
      if(fast) {
        stats[,4] <- apply(.data,2,min,na.rm=na.rm)
        stats[,5] <- apply(.data,2,max,na.rm = na.rm)
      } else {
        stats[, 4] <-  apply(.data,2,min, na.rm=na.rm )
        stats[, 5] <-  apply(.data,2,max, na.rm=na.rm )
        stats[,7] <-   apply(.data,2,mad, na.rm=na.rm)
        stats[,9]  <- apply(.data,2, mean,na.rm=na.rm,trim=trim)
        if(interp) {stats[, 3] <- apply(.data,2,interp.median,na.rm=na.rm  ) }  else {stats[,3] <- apply(.data,2,median,na.rm=na.rm) }
      }}

    if(!is.null(quant)) { Qnt <- apply(.data,2,quantile,prob=quant,na.rm=TRUE)
    stats[,(IQR+11):numstats] <- t(Qnt)}

    if(IQR) {Quart <- t(apply(.data,2,quantile,prob=c(.25,.75),na.rm=TRUE))
    Iqr <- Quart[,2] - Quart[,1]
    stats[,11] <- Iqr
    }
  }  #end of maxtrix input
  #now summarize the results
  if (numstats > (10 + IQR)) {
    colnames(stats)[(11+IQR):numstats] <- paste0("Q",quant[1:length(quant)])}

  #the following output was cleaned up on June 22, 2016 added the quantile information.

  #the various options are ranges, skew, fast, numstats > 10
  if(fast) { answer <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10],se=stats[,10]/sqrt(stats[,1])) }  #minimal case

  #if((!skew) && ranges) {answer <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10],min= stats[,4],max=stats[,5], range=stats[,5]-stats[,4],se=stats[,10]/sqrt(stats[,1])) }
  if(skew) {
    if(ranges) { answer  <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10], median = stats[, 3],trimmed =stats[,9], mad = stats[,7], min= stats[,4],max=stats[,5],
                                        range=stats[,5]-stats[,4],skew = stats[, 6], kurtosis = stats[,8],se=stats[,10]/sqrt(stats[,1])) } else {
                                          answer  <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10],skew = stats[, 6], kurtosis = stats[,8],se=stats[,10]/sqrt(stats[,1])) }
  } else {if(ranges) {answer <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10],min= stats[,4],max=stats[,5], range=stats[,5]-stats[,4],se=stats[,10]/sqrt(stats[,1])) } else {
    answer  <-  data.frame(vars=vars,n = stats[,1],mean=stats[,2], sd = stats[,10],se=stats[,10]/sqrt(stats[,1]))   }
  }
  if(IQR) answer <- data.frame(answer,IQR=stats[,11])

  if (numstats > (10+ IQR)) {if(nvar > 1 ) {answer <- data.frame(answer, stats[,(IQR+11):numstats])   #add the quantile information
  } else {

    answer <- data.frame(answer, t(stats[,(IQR+11):numstats])) }
  }

  class(answer) <- c("psych","describe","data.frame")
  return(answer)
}


#' Skew from psych
#'
#' Internal only
#'
#' @author Procedures for Psychological, Psychometric, and Personality Research.
#'   Homepage: https://personality-project.org/r/psych
#'   https://personality-project.org/r/psych-manual.pdf
#'
#' @keywords internal
skew <-  function (x, na.rm = TRUE,type=3)
  {
    if (length(dim(x)) == 0) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      sdx <- sd(x,na.rm=na.rm)
      mx <- mean(x)
      n <- length(x[!is.na(x)])
      switch(type,
             {skewer <- sqrt(n) *( sum((x - mx)^3,  na.rm = na.rm)/( sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 1
             {skewer <- n *sqrt(n-1) *( sum((x - mx)^3,  na.rm = na.rm)/((n-2) * sum((x - mx)^2,na.rm = na.rm)^(3/2)))}, #case 2
             {skewer <- sum((x - mx)^3)/(n * sd(x)^3) })  #case 3
    } else {

      skewer <- rep(NA,dim(x)[2])
      if (is.matrix(x)) {mx <- colMeans(x,na.rm=na.rm)} else {mx <- apply(x,2,mean,na.rm=na.rm)}
      sdx <- apply(x,2,sd,na.rm=na.rm)
      for (i in 1:dim(x)[2]) {
        n <- length(x[!is.na(x[,i]),i])
        switch(type,
               {skewer[i] <-sqrt(n) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/( sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))}, #type 1
               {skewer[i] <- n *sqrt(n-1) *( sum((x[,i] - mx[i])^3,  na.rm = na.rm)/((n-2) * sum((x[,i] - mx[i])^2,na.rm = na.rm)^(3/2)))},#type 2
               {skewer[i] <- sum((x[,i] - mx[i])^3,  na.rm = na.rm)/(n * sdx[i]^3)} #type 3
        ) #end switch
      } #end loop
    }
    return(skewer)
  }



#' kurtosi from psych
#'
#' @keywords internal
#'
#'
#' @author Procedures for Psychological, Psychometric, and Personality Research.
#'   Homepage: https://personality-project.org/r/psych
#'   https://personality-project.org/r/psych-manual.pdf
kurtosi <-  function (x, na.rm = TRUE,type=3)
  {
    if (length(dim(x)) == 0) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      if (is.matrix(x) ) { mx <- colMeans(x,na.rm=na.rm)} else {mx <- mean(x,na.rm=na.rm)}
      sdx <- sd(x,na.rm=na.rm)
      n <- length(x[!is.na(x)])
      switch(type,
             {kurt <- sum((x - mx)^4,  na.rm = na.rm)*n /(sum((x - mx)^2,na.rm = na.rm)^2)  -3},  #type 1
             {
               kurt <- n*(n + 1)*sum((x - mx)^4,  na.rm = na.rm)/( (n - 1)*(n - 2)*(n - 3)*(sum((x - mx)^2,na.rm = na.rm)/(n - 1))^2)  -3 *(n- 1)^2 /((n - 2)*(n - 3)) }, # type 2
             {kurt <- sum((x - mx)^4)/(n *sdx^4)  -3} )  #	type 3
    } else {

      kurt <- rep(NA,dim(x)[2])
      #  mx <- mean(x,na.rm=na.rm)
      mx <-apply(x,2 ,mean,na.rm=na.rm)
      if(type==3)  sdx <- apply(x,2,sd,na.rm=na.rm)

      for (i in 1:dim(x)[2]) {
        n <- length(x[!is.na(x[,i]),i])
        switch(type,
               { kurt[i] <- sum((x[,i] - mx[i])^4,  na.rm = na.rm)*length(x[,i]) /(sum((x[,i] - mx[i])^2,na.rm = na.rm)^2)  -3},  #type 1
               {
                 xi <- x[,i]-mx[i]
                 kurt[i] <- n*(n + 1)*sum((x[,i] - mx[i])^4,  na.rm = na.rm)/( (n - 1)*(n - 2)*(n - 3)*(sum((x[,i] - mx[i])^2,na.rm = na.rm)/(n - 1))^2)  -3 *(n- 1)^2 /((n - 2)*(n - 3)) }  #type 2
               ,
               {
                 kurt[i] <- sum((x[,i] - mx[i])^4,  na.rm = na.rm)/((length(x[,i]) - sum(is.na(x[,i]))) * sdx[i]^4)  -3},  #type 3
               {NULL})
        names(kurt) <- colnames(x)
      }}
    return(kurt)
  }



#' Interpolate median from psych
#'
#' @keywords internal
#'
#'
#' @author Procedures for Psychological, Psychometric, and Personality Research.
#'   Homepage: https://personality-project.org/r/psych
#'   https://personality-project.org/r/psych-manual.pdf
interp.median  <-  function(x,w=1,na.rm=TRUE) {
    im <- interp.quantiles(x,q=.5,w,na.rm=na.rm)
    return(im)}


#' Interpolate quantiles from psych
#'
#' @keywords internal
#'
#' @author Procedures for Psychological, Psychometric, and Personality Research.
#'   Homepage: https://personality-project.org/r/psych
#'   https://personality-project.org/r/psych-manual.pdf
interp.quantiles  <-   function(x,q=.5,w=1,na.rm=TRUE) {
    if (!(q>0) | !(q<1) ) {stop("quantiles most be greater than 0 and less than 1 q = ",q)}
    if(is.vector(x)) {im <- interp.q(x,q,w,na.rm=na.rm) } else {
      if((is.matrix(x) | is.data.frame(x)) ){
        n <- dim(x)[2]
        im <- matrix(NA,ncol=n)
        for (i in 1:n) {im[i] <- interp.q(x[,i],q,w=w,na.rm=na.rm)}
        colnames(im) <- colnames(x)
      } else {stop('The data must be either a vector, a matrix, or a data.frame')}
      return(im)
    }}
