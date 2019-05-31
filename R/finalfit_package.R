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
#' \link{coxphmulti},
#' \link{crruni},
#' \link{crrmulti},
#' \link{svyglmuni},
#' \link{svyglmmulti}.
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
#' \link{fit2df.crr},
#' \link{fit2df.crrlist},
#' \link{fit2df.stanfit}.
#'
#' @section \code{finalfit} all-in-one function:
#'
#' Generic:
#' \link{finalfit}.
#' \link{finalfit_permute}.
#'
#' Methods (not called directly):
#' \link{finalfit.glm},
#' \link{finalfit.lm},
#' \link{finalfit.coxph}.
#'
#' @section \code{finalfit} plotting functions:
#'
#' \link{coefficient_plot},
#' \link{or_plot},
#' \link{hr_plot},
#' \link{surv_plot},
#' \link{ff_plot}.
#'
#' @section \code{finalfit} helper functions:
#'
#' \link{ff_glimpse},
#' \link{ff_label},
#' \link{ff_merge},
#' \link{ff_interaction}.
#'
#' @section \code{finalfit} prediction functions:
#'
#' \link{boot_predict},
#' \link{finalfit_newdata}.
#'
#' Methods (not called directly):
#' \link{boot_compare}.
#'
#' @section \code{finalfit} missing data functions:
#'
#' \link{missing_glimpse},
#' \link{missing_pattern},
#' \link{missing_compare},
#' \link{missing_plot},
#' \link{missing_pairs}.
#'
#' @docType package
#' @name finalfit-package
#'
#' @importFrom stats as.formula coef confint confint.default glm lm logLik pnorm
#'   quantile setNames model.matrix
#' @importFrom survival Surv coxph
# @exportPattern ^[[:alpha:]]+
NULL

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @export
NULL

#' magrittr compound assignment pipe-operator
#'
#' @name %<>%
#' @rdname compoundpipe
#' @keywords internal
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
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
