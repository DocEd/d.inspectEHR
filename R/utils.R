#' Match and replace Athena codes wtih proper names
#'
#' @param column an integer vector of Athena codes
#' @param lookup an extract of the \code{concept} table with 2 columns:
#'   1. concept_name (containing the full name replacement)
#'   2. concept_id (containing the Athena code)
#'
#' @return
#' @export
match_concepts <- function(column, lookup) {

  if (all(is.na(column))) {
    return(column)
  } else {
    lookup$concept_name[match(column,  lookup$concept_id)]
  }

}


#' Tally OMOP Concept ID Column
#'
#' @param ctn a database connection object
#' @param schema the database schema
#' @param table the OMOP target table
#' @param concept_col the concept_id target column (but be \code{..._concept_id})
#'
#' @importFrom dplyr tbl group_by filter select tally collect mutate across
#' @importFrom dbplyr in_schema
#' @importFrom tidyselect everything
#' @importFrom rlang .data !!
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
tally_concept <- function(ctn, schema, table, concept_col) {
  x <- tbl(src = ctn, in_schema(schema = schema, table = table)) %>%
    group_by(.data[[concept_col]]) %>%
    tally() %>%
    collect() %>%
    mutate(across(everything(), as.integer))

  dict <- mini_dict(ctn, schema, x[[concept_col]])

  x[[concept_col]] <- match_concepts(x[[concept_col]], dict)

  return(x)
}

#' Create a mini concept dictionary
#'
#' It is convenient to work with a small dictionary of concepts. This function
#' pulls them out and creates a standardised dictionary of concept ids and names
#' for use in other functions.
#'
#' @param ctn a database connection object
#' @param schema the database schema
#' @param concept_ids an integer vector of athena concept ids
#'
#' @importFrom dplyr tbl filter select collect mutate
#' @importFrom dbplyr in_schema
#' @importFrom rlang !! .data
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
mini_dict <- function(ctn, schema, concept_ids) {

  tbl(src = ctn, in_schema(schema = schema, table = "concept")) %>%
    filter(concept_id %in% !! concept_ids) %>%
    select(.data$concept_id, .data$concept_name) %>%
    collect() %>%
    mutate(concept_id = as.integer(.data$concept_id))

  }


#' Summarise Missing Values within a Dataframe
#'
#' @param x a dataframe
#' @param tbl_name the OMOP table name of \code{x}
#'
#' @importFrom dplyr summarise across mutate
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom tibble add_column
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
summarise_missing <- function(x, tbl_name) {
  x %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(),
                 names_to = "column",
                 values_to = "n rows missing") %>%
    add_column(table = tbl_name, .before = TRUE) %>%
    mutate(`% rows missing` = round((`n rows missing`/nrow(x))*100, 0))
}


#' Check which rows contain zero
#'
#' Appends a dataframe with a new column to identify rows that contain zeros.
#' This is then represented as a factor with the levels: "no" and "yes" so that
#' both levels are observed for plotting purposes
#'
#' @param x a dataframe
#' @param column the column name with tally counts (tidyeval)
#'
#' @importFrom rlang .data
#' @importFrom dplyr if_else
#'
#' @return
#' @export
check_zero_tally <- function(x, column) {
  x %>%
    mutate(.is_zero = if_else({{ column }} == 0, "yes", "no")) %>%
    mutate(.is_zero = factor(
      x = .data$.is_zero,
      levels = c("yes", "no"),
      labels = c("yes", "no"))
    )
}


quick_lead <- function(x) {
  if (length(x) == 1) {
    return(NA)
  } else {
    c(x[2:length(x)], NA)
  }
}

outliers <- function(x, probs = 0.99) {
  boundary <- as.numeric(quantile(x, probs = probs))
  if_else(x >= boundary, "outlier", "main")
}

#' Remove elements from ggplot at Glob level
#'
#' @param x the ggplot after being parsed as a gtable
#' @param name the glob element name
#' @param trim logical flag
#'
#' @return
#' @export
#'
#' @importFrom gtable gtable_trim
gtable_filter_remove <- function (x, name, trim = TRUE){
  matches <- !(x$layout$name %in% name)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  if (trim)
    x <- gtable_trim(x)
  x
}

is.integer64 <- function(x) {
  check_class <- class(x)
  any(class(x) == "integer64")
}
