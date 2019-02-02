#' Make an interaction variable and add to dataframe
#'
#' Combine two factor variables to make an interaction variable. Factor level
#' order is determined by the order in the variables themselves. Note, names of
#' the factor variables should not be quoted. The name of the variable is
#' created from the names of the two factors. The variable is also
#' labelled with a name derived from any pre-existing labels.
#'
#' @param .data Data frame.
#' @param ... The unquoted names of two factors.
#' @param levels_sep Quoted character: how levels are separated in new variable.
#' @param var_sep Quoted character: how variable name is separated.
#' @param label_sep Quoted character: how variable label is spearated
#'
#' @return Original data frame with new variable added via `dplyr::mutate`.
#' @export
#'
#' @examples
#'
#' colon_s %>%
#'   ff_interaction(sex.factor, perfor.factor) %>%
#'     summary_factorlist("mort_5yr", "sex.factor__perfor.factor")

ff_interaction = function(.data, ..., levels_sep = "|", var_sep = "__", label_sep=":"){
	.f <- rlang::quos(...)
	.f_len <- length(.f)
	if(.f_len>2) stop("Currently only supports two factors.")
	.n <- purrr::map(.f, rlang::quo_name) %>%
		paste(collapse = var_sep)
	.l <- .data %>% dplyr::select(!!! .f) %>%
		purrr::map(levels)
	.l_length <- purrr::map(.l, length)
	.l <-	paste0(rep(.l[[1]], each=.l_length[[2]]), levels_sep, rep(.l[[2]], .l_length[[1]]))
	.label <- .data %>% dplyr::select(!!! .f) %>%
		extract_variable_label() %>% paste(collapse=label_sep)
	df.out <- dplyr::mutate(.data,
													!! .n := paste(!!! .f, sep = levels_sep) %>% factor(levels=.l) %>%
														ff_label(variable_label=.label)
	)
	return(df.out)
}

#' @rdname ff_interaction
#' @export
finalfit_interaction <- ff_interaction