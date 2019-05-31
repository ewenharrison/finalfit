#' Competing risks univariable regression: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces univariable Competing Risks
#' Regression models for a set of explanatory variables.
#'
#' Uses \code{\link[cmprsk]{crr}} with \code{finalfit} modelling conventions.
#' Output can be passed to \code{\link{fit2df}}.
#'
#' @param .data Data frame or tibble.
#' @param dependent Character vector of length 1: name of survival object in
#'   form \code{Surv(time, status)}. \code{Status} default values should be 0
#'   censored (e.g. alive), 1 event of interest (e.g. died of disease of
#'   interest), 2 competing event (e.g. died of other cause).
#' @param explanatory Character vector of any length: name(s) of explanatory
#'   variables.
#' @param ... Other arguments to \code{\link[cmprsk]{crr}}
#' @return A list of univariable \code{\link[cmprsk]{crr}} fitted models class
#'   \code{crrlist}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(dplyr)
#' melanoma = boot::melanoma
#' melanoma = melanoma %>%
#'   mutate(
#'     # Cox PH to determine cause-specific hazards
#'     status_coxph = ifelse(status == 2, 0, # "still alive"
#'       ifelse(status == 1, 1, # "died of melanoma"
#'         0)), # "died of other causes is censored"
#'         
#'     # Fine and Gray to determine subdistribution hazards
#'     status_crr = ifelse(status == 2, 0, # "still alive"
#'       ifelse(status == 1, 1, # "died of melanoma"
#'         2)), # "died of other causes"
#'     sex = factor(sex),
#'     ulcer = factor(ulcer)
#'   )
#'
#' dependent_coxph = c("Surv(time, status_coxph)")
#' dependent_crr = c("Surv(time, status_crr)")
#' explanatory = c("sex", "age", "ulcer")
#' 
#' # Create single well-formatted table
#' melanoma %>%
#'   summary_factorlist(dependent_crr, explanatory, column = TRUE, fit_id = TRUE) %>%
#'   ff_merge(
#'     melanoma %>%
#'       coxphmulti(dependent_coxph, explanatory) %>%
#'       fit2df(estimate_suffix = " (Cox PH multivariable)")
#'     ) %>%
#'   ff_merge(
#'     melanoma %>%
#'       crrmulti(dependent_crr, explanatory) %>%
#'       fit2df(estimate_suffix = " (competing risks multivariable)")
#'     ) %>%
#'   select(-fit_id, -index) %>%
#'   dependent_label(melanoma, dependent_crr)

crruni <- function(.data, dependent, explanatory, ...){
  result = list()
  
  # Keep survival object grammar, split into terms
  dependent = dependent %>% 
    gsub("Surv\\(", "", .) %>% 
    gsub("\\)", "", .) %>% 
    strsplit(",") %>% 
    unlist() %>% 
    trimws()
  
  ftime = .data %>% 
    dplyr::pull(dependent[1])
  fstatus = .data %>% 
    dplyr::pull(dependent[2])

  for (i in 1:length(explanatory)){
    cov1 = model.matrix(as.formula(paste0("~", explanatory[i])), .data)[,-1, drop = FALSE] 
    result[[i]] = cmprsk::crr(ftime, fstatus, cov1, ...)
  }
  
  class(result) = "crrlist"
  return(result)
}
