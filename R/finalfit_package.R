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
#' @importFrom stats as.formula formula coef confint confint.default glm lm logLik pnorm
#'   quantile setNames model.matrix terms
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

#' magrittr exposition pipe-operator
#'
#' @name %$%
#' @rdname expositionpipe
#' @keywords internal
#' @importFrom magrittr %$%
#' @usage lhs \%$\% rhs
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

#' Western Collaborative Group Study
#'
#' 3154 healthy young men aged 39-59 from the San Francisco area were assessed
#' for their personality type. All were free from coronary heart disease at the
#' start of the research. Eight and a half years later change in this situation
#' was recorded.
#'
#' The WCGS began in 1960 with 3,524 male volunteers who were employed by 11
#' California companies. Subjects were 39 to 59 years old and free of heart
#' disease as determined by electrocardiogram. After the initial screening, the
#' study population dropped to 3,154 and the number of companies to 10 because
#' of various exclusions. The cohort comprised both blue- and white-collar
#' employees. At baseline the following information was collected:
#' socio-demographic including age, education, marital status, income,
#' occupation; physical and physiological including height, weight, blood
#' pressure, electrocardiogram, and corneal arcus; biochemical including
#' cholesterol and lipoprotein fractions; medical and family history and use of
#' medications; behavioral data including Type A interview, smoking, exercise,
#' and alcohol use. Later surveys added data on anthropometry, triglycerides,
#' Jenkins Activity Survey, and caffeine use. Average follow-up continued for
#' 8.5 years with repeat examinations
#'
#' @name wcgs
#'
#' @format A data frame with 3154 observations on the following 13 variables.
#'   \itemize{ \item{\code{id}}{ Subject ID } \item{\code{age}}{ Age: age in
#'   years } \item{\code{height}}{ Height: height in inches }
#'   \item{\code{weight}}{ Weight: weight in pounds } \item{\code{sbp}}{
#'   Systolic blood pressure: mmHg } \item{\code{dbp}}{ Diastolic blood
#'   pressure: mmHg } \item{\code{chol}}{ Cholesterol: mg/100 ml }
#'   \item{\code{personality}}{Personality type/Behavior pattern: a factor with
#'   levels \code{A1}, \code{A2}, \code{B3}, \code{B4} }
#'   \item{\code{personality_2L}}{ Dichotomous personality type / behavior
#'   pattern: \code{A} = aggressive; \code{B} = passive }  \item{\code{ncigs0}}{
#'   Smoking: Cigarettes/day } \item{\code{smoking}}{ Smoking: \code{No},
#'   \code{Yes} } \item{\code{arcus0}}{ Corneal arcus: \code{No}, \code{Yes} }
#'   \item{\code{chd}}{ Coronary heart disease event: \code{No} \code{Yes}}
#'   \item{\code{typechd}}{ coronary heart disease is a factor with levels
#'   \code{No}, \code{MI_SD} (MI or sudden death), \code{Silent_MI},
#'   \code{Angina} } \item{\code{timechd}}{ Observation (follow up) time: Days }
#'   }
#' @source Statistics for Epidemiology by N. Jewell (2004)
#' @references Coronary Heart Disease in the Western Collaborative Group Study
#'   Final Follow-up Experience of 8 1/2 Years Ray H. Rosenman, MD; Richard J.
#'   Brand, PhD; C. David Jenkins, PhD; Meyer Friedman, MD; Reuben Straus, MD;
#'   Moses Wurm, MD JAMA. 1975;233(8):872-877.
#'   doi:10.1001/jama.1975.03260080034016.
#' @docType data
#' @usage data(wcgs)
#' @keywords data
NULL
