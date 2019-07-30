#' Characterise missing data for \code{finalfit} models
#'
#' Using \code{finalfit} conventions, produces a missing data matrix using
#' \code{\link[mice]{md.pattern}}.
#'
#' @param .data Data frame. Missing values must be coded \code{NA}.
#' @param dependent Character vector usually of length 1, name of depdendent
#'   variable.
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param rotate.names Logical. Should the orientation of variable names on plot
#'   should be vertical.
#'
#' @return A matrix with \code{ncol(x)+1} columns, in which each row corresponds
#'   to a missing data pattern (1=observed, 0=missing). Rows and columns are
#'   sorted in increasing amounts of missing information. The last column and
#'   row contain row and column counts, respectively.
#'
#' @export
#' @examples
#' library(finalfit)
#' library(dplyr)
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' 	missing_pattern(dependent, explanatory)
#' 
missing_pattern = function(.data, dependent=NULL, explanatory=NULL, 
													 rotate.names = TRUE, ...){
  if(is.null(dependent) && is.null(explanatory)){
    df.in = .data
  }else{
    keep = names(.data) %in% c(dependent, explanatory)
    df.in = .data[keep]
  }
  mice::md.pattern(df.in, rotate.names = rotate.names, ...)
}


#' Create predictorMatrix for use with mice
#'
#' @param .data Data frame.
#' @param drop_from_imputed Quoted names of variables not to impute.
#' @param drop_from_imputer Quoted names of variables not to use in imputation
#'   algorithm.
#'
#' @return Matrix formatted for predictorMatrix argument in mice.
#' @importFrom mice make.predictorMatrix
#' @export
#'
#' @examples
#' library(mice)
#' library(dplyr)
#' library(Hmisc)
#'
#' # Create some extra missing data
#' ## Smoking missing completely at random
#' set.seed(1)
#' colon_s$smoking_mcar =
#'   sample(c("Smoker", "Non-smoker", NA),
#'   dim(colon_s)[1], replace=TRUE,
#'   prob = c(0.2, 0.7, 0.1)) %>%
#'   factor()
#' Hmisc::label(colon_s$smoking_mcar) = "Smoking (MCAR)"
#'
#' ## Make smoking missing conditional on patient sex
#' colon_s$smoking_mar[colon_s$sex.factor == "Female"] =
#'   sample(c("Smoker", "Non-smoker", NA),
#' 	 sum(colon_s$sex.factor == "Female"),
#' 	 replace = TRUE, prob = c(0.1, 0.5, 0.4))
#'
#' colon_s$smoking_mar[colon_s$sex.factor == "Male"] =
#'   sample(c("Smoker", "Non-smoker", NA),
#' 	 sum(colon_s$sex.factor == "Male"),
#' 	 replace=TRUE, prob = c(0.15, 0.75, 0.1))
#' colon_s$smoking_mar = factor(colon_s$smoking_mar)
#' Hmisc::label(colon_s$smoking_mar) = "Smoking (MAR)"
#'
#' explanatory = c("age", "sex.factor",
#'   "nodes", "obstruct.factor", "smoking_mar")
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#' select(dependent, explanatory) %>%
#'   missing_predictorMatrix(drop_from_imputed =
#'     c("obstruct.factor", "mort_5yr")) -> predM
#'
#' colon_s %>%
#' 	select(dependent, explanatory) %>%
#' 	mice(m = 2, predictorMatrix = predM) %>% # e.g. m=10 when for real
#' 	# Run logistic regression on each imputed set
#' 	with(glm(formula(ff_formula(dependent, explanatory)),
#' 					 family="binomial")) %>%
#' 	pool() %>%
#' 	summary(conf.int = TRUE, exponentiate = TRUE) %>%
#' 	# Jiggle into finalfit format
#' 	mutate(explanatory_name = rownames(.)) %>%
#' 	select(explanatory_name, estimate, `2.5 %`, `97.5 %`, p.value) %>%
#' 	condense_fit(estimate_suffix = " (multiple imputation)") %>%
#' 	remove_intercept() -> fit_imputed
#'
missing_predictorMatrix = function(.data,
																	 drop_from_imputed = NULL,
																	 drop_from_imputer = NULL){
	.data %>%
		mice::make.predictorMatrix()  -> df.out

	if (!is.null(drop_from_imputed)){
		df.out[rownames(df.out) %in% drop_from_imputed, ] = 0
	}
	if (!is.null(drop_from_imputer)){
		df.out[ ,colnames(df.out) %in% drop_from_imputer] = 0
	}
	return(df.out)
}

