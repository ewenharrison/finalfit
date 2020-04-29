#' Label a variable
#'
#' @param .var Quoted variable name
#' @param variable_label Quoted variable label
#'
#' @return Labelled variable
#' @seealso \code{\link{extract_variable_label}} \code{\link{ff_relabel}}
#' @export
#' @examples
#' colon_s$sex.factor %>%
#'   ff_label("Sex") %>%
#'   str()
ff_label <- function(.var, variable_label){
	attr(.var, "label") = variable_label
	return(.var)
}

#' @rdname ff_label
#' @export
finalfit_label <- ff_label

#' Extract variable labels from dataframe
#'
#' Variable labels can be created using \code{\link{ff_label}}. Some functions
#' strip variable labels (variable attributes), e.g. \code{forcats::fct_recode}.
#' Use this function to create a vector of variable labels from a data frame.
#' Then use \code{\link{ff_relabel}} to relabel variables in data frame.
#'
#' @param .data Dataframe containing labelled variables.
#'
#' @export
#' @examples
#' colon_s %>%
#'   extract_variable_label
extract_variable_label = function(.data){
	if(any(class(.data) %in% c("tbl_df", "tbl"))) .data = data.frame(.data)
	sapply(colnames(.data), function(x){
		label = attr(.data[,x], "label")
		ifelse(is.null(label), x, label)
	})
}

#' Relabel variables in a data frame
#'
#' Variable labels can be created using \code{\link{ff_label}}. Some functions
#' strip variable labels (variable attributes), e.g. \code{forcats::fct_recode}.
#' Use this function to create a vector of variable labels from a data frame.
#' Then use \code{\link{ff_relabel}} to relabel variables in data frame.
#'
#' @param .data Data frame to be relabelled
#' @param .labels Vector of variable labels (usually created using
#'   \code{\link{extract_variable_label}}) of same length as \code{.data}.
#'
#' @export
#'
#' @examples
#' # Label variable
#' colon_s$sex.factor %>%
#'   ff_label("Sex") %>%
#'   str()
#'
#' # Make factor level "Unknown" NA
#' colon_s %>%
#'   dplyr::mutate_if(is.factor, forcats::fct_recode, 
#'   NULL = "Unknown") %>% 
#'   str()
#' 
#' # Reset data
#' data(colon_s)
#' 
#' # Extract variable labels
#' vlabels = colon_s %>% extract_variable_label()
#'
#' # Run function where labels are lost
#' colon_s %>%
#'   dplyr::mutate_if(is.factor, forcats::fct_recode, 
#'   NULL = "Unknown") %>% 
#'   str()
#' 
#' # Relabel
#' colon_s %<>% ff_relabel(vlabels)
#' colon_s %>% str()
#'   
ff_relabel <- function(.data, .labels){
	# Keep only labels for variables in data
	.labels = .labels[names(.labels) %in% names(.data)]
	relabel_one <- function(.){
	  var <- as.character(match.call()[[2L]])
	  label = .labels[[var]]
	  ff_label(., label)
	}
	.data %>% 
		dplyr::mutate_at(names(.labels), relabel_one) # Apply only to variables for which labels
}

#' @rdname ff_relabel
#' @export
#' 
finalfit_relabel <- ff_relabel




#' Relabel variables from data frame after tidyverse functions
#'
#' @param .data Data frame or tibble after applicaton of label stripping functions. 
#' @param .df Original data frame which contains labels. 
#'
#' @return Data frame or tibble
#' @export
#'
ff_relabel_df <- function(.data, .df){
	.labels = extract_variable_label(.df)
	.labels = .labels[names(.labels) %in% names(.data)]
	relabel_one <- function(.) {
	  var <- as.character(match.call()[[2L]])
	  label = .labels[[var]]
	  ff_label(., label)
	}
	.data %>% 
		dplyr::mutate_at(names(.labels), relabel_one) # Apply only to variables for which labels
}
#' @rdname ff_relabel_df
#' @export
#' 
finalfit_relabel_df <- ff_relabel_df



#' Remove variable labels.
#'
#' @param .data Data frame
#'
#' @return The original data frame with variable label attributes removed.
#' @export
#' @keywords internal
#'
#' @examples
#' colon_s %>%
#'   remove_labels()
remove_labels = function(.data){
	attr_label_null <- function(x){
		attr(x, "label") <- NULL
		return(x)
	}
	
	suppressWarnings( # All these irritiating bind_row warnings
	.data %>% 
		purrr::map_df(attr_label_null)
	)
}
