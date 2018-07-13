#' finalfit: Quickly create elegant final results tables and plots when modelling.
#'
#' @section \code{finalfit} model wrappers:
#'
#' \link{glmuni},
#' \link{glmmulti},
#' \link{glmmulti_boot},
#' \link{glmmixed},
#' \link{lmuni},
#' \link{lmmulti},
#' \link{lmmixed},
#' \link{coxphuni},
#' \link{coxphmulti}.
#'
#' @section \code{finalfit} model extractor:
#'
#' Generic:
#' \link{fit2df}
#'
#' Methods (not called directly):
#' \link{fit2df.glm},
#' \link{fit2df.glmlist},
#' \link{fit2df.glmboot},
#' \link{fit2df.lm},
#' \link{fit2df.lmlist},
#' \link{fit2df.glmerMod},
#' \link{fit2df.lmerMod},
#' \link{fit2df.coxph},
#' \link{fit2df.coxphlist},
#' \link{fit2df.stanfit}.
#'
#' @section \code{finalfit} all-in-one function:
#'
#' Generic:
#' \link{finalfit}.
#'
#' Methods (not called directly):
#' \link{finalfit.glm},
#' \link{finalfit.lm},
#' \link{finalfit.coxph}.
#'
#' @section \code{finalfit} plotting functions:
#'
#' \link{or_plot},
#' \link{hr_plot},
#' \link{surv_plot}.
#'
#' @section \code{finalfit} helper functions:
#'
#' \link{finalfit_merge},
#' \link{finalfit_missing}.
#'
#' @section \code{finalfit} prediction functions:
#'
#' \link{boot_predict},
#' \link{finalfit_newdata}.
#'
#' Methods (not called directly):
#' \link{boot_compare}.
#'
#' @docType package
#' @name finalfit-package
#'
#' @importFrom stats as.formula coef confint glm lm logLik pnorm
#'   quantile setNames
#' @importFrom survival Surv coxph
#' @exportPattern ^[[:alpha:]]+
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' Chemotherapy for Stage B/C colon cancer
#'
#' This is a modified version of \code{survival::\link[survival]{colon}}.These
#' are data from one of the first successful trials of adjuvant chemotherapy for
#' colon cancer. Levamisole is a low-toxicity compound previously used to treat
#' worm infestations in animals; 5-FU is a moderately toxic (as these things go)
#' chemotherapy agent. There are two records per person, one for recurrence and
#' one for death
#'
#' @name colon_s
#'
#' @format A data frame with 929 rows and 33 variables
#' @source \code{\link[survival]{colon}}
#' @docType data
#' @usage data(colon_s)
#' @keywords data
NULL
