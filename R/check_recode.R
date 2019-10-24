#' Check accurate recoding of variables
#'
#' This was written a few days after the retraction of a paper in JAMA due to an
#' error in recoding the treatment variable
#' (\url{https://jamanetwork.com/journals/jama/fullarticle/2752474}). This
#' takes a data frame or tibble, fuzzy matches variable names, and produces
#' crosstables of all matched variables. A visual inspection should reveal any
#' miscoding.
#'
#' @param .data Data frame or tibble.
#' @param dependent Optional character vector: name(s) of depdendent
#'   variable(s).
#' @param explanatory Optional character vector: name(s) of explanatory
#'   variable(s). 
#' @param include_numerics Logical. Include numeric variables in function. 
#'
#' @return List of length two. The first is an index of variable combiations.
#'   The second is a nested list of crosstables as tibbles.
#' @export
#'
#' @examples
#' library(dplyr)
#' data(colon_s)
#' colon_s_small = colon_s %>%
#'   select(-id, -rx, -rx.factor) %>%
#'   mutate(
#'     age.factor2 = forcats::fct_collapse(age.factor,
#'       "<60 years" = c("<40 years", "40-59 years")),
#'     sex.factor2 = forcats::fct_recode(sex.factor,
#'     # Intentional miscode
#'       "F" = "Male",
#'       "M" = "Female")
#'   )
#'
#' # Check
#' colon_s_small %>%
#'   check_recode(include_numerics = FALSE)
#'
#' out = colon_s_small %>%
#'   select(-extent, -extent.factor,-time, -time.years) %>%
#'   check_recode()
#' out
#'
#' # Select a tibble and expand
#' out$counts[[9]] %>%
#'   print(n = Inf)
#' # Note this variable (node4) appears miscoded in original dataset survival::colon.
#' 
#' # Choose to only include variables that you actually use. 
#' # This uses standard Finalfit grammar. 
#' dependent = "mort_5yr"
#' explanatory = c("age.factor2", "sex.factor2")
#' colon_s_small %>% 
#'   check_recode(dependent, explanatory)
check_recode <- function(.data, dependent = NULL, explanatory = NULL, include_numerics = TRUE){
	if(!is.data.frame(.data)) stop(".data is not dataframe")
	
	if(include_numerics){
		.data = .data
	} else {
		.data = .data  %>% 
			dplyr::select_if(purrr::negate(is.numeric))
	}
	
	if(is.null(dependent) && is.null(explanatory)){
		.varnames = .data %>% names()
	} else {
		.varnames = .data %>% 
			dplyr::select(dependent, explanatory) %>% 
			names()
	}
	
	.varnames_combinations = .varnames %>%  
		purrr::map(., agrep, names(.data), value = TRUE) %>% 
		dplyr::tibble(var1 = .varnames, 
									var2 = .) %>% 
		tidyr::unnest(cols = c(var2)) %>% 
		dplyr::filter(var1 != var2) %>% 
		dplyr::mutate(
			keep = purrr::map2_chr(var1, var2, ~toString(sort(c(.x, .y))))
		) %>% 
		dplyr::distinct(keep, .keep_all = TRUE) %>%
		dplyr::select(-keep)
	
	count_stuff = function(.data, var1, var2){
		.data %>% 
			dplyr::count(!! sym(var1), !! sym(var2))
	}
	
	list.out = .varnames_combinations %>% 
		purrr::pmap(count_stuff, .data = .data)
	list.out = list(index = .varnames_combinations, counts = list.out)
	return(list.out)
}
