#' Prepare Small Database Tables
#'
#' Collects and processes some of the smaller database tables:
#'
#'   - person
#'   - visit_occurrence
#'   - visit_detail
#'   - death
#'
#' As these tables grow, it might be prudent to do this preparation in the DB.
#'
#' @param connection a connection object created with \code{\link{DBI::dbConnect}}
#' @param schema_name a character vector for the target schema within the DB
#'
#' @importFrom dplyr collect tbl select across bind_rows distinct filter mutate
#' @importFrom dbplyr in_schema
#' @importFrom purrr map
#' @importFrom rlang !! .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains everything
#'
#' @return
#' @export
prepare_tables <- function(connection, schema_name) {

  st <- vector(mode = "list", length = 4)
  names(st) <- c("person", "visit_occurrence", "visit_detail", "death")

  st[["person"]] <- collect(tbl(connection, in_schema(schema_name, "person")))

  st[["visit_occurrence"]] <- collect(
    tbl(connection, in_schema(schema_name, "visit_occurrence")))


  st[["visit_detail"]] <- collect(
    tbl(connection, in_schema(schema_name, "visit_detail")))

  st[["death"]] <- collect(tbl(connection, in_schema(schema_name, "death")))

  all_concepts <- st %>%
    map(~ select(., contains("concept_id")) %>%
          pivot_longer(everything(),
                       names_to = "column_name",
                       values_to = "concept_id")) %>%
    bind_rows() %>%
    distinct(.data$concept_id, .keep_all = TRUE)

  replace_names <- mini_dict(connection, schema_name, all_concepts$concept_id)

  st %>%
    map(~
          mutate(
            .,
            across(where(is.integer64), as.integer))) %>%
    map(~
          mutate(
            .,
            across(
              c(contains("concept_id"),
                -contains("source"),
                contains("admitting_source_concept_id")),
              match_concepts, lookup = replace_names))
    )
}


#' Prepare Overview Table
#'
#' @param x the output from \code{\link{prepare_tables}}
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select left_join
#' @importFrom rlang .data
#'
#' @return
#' @export
prepare_overview <- function(x) {

  x[["person"]] %>%
    select(.data$person_id,
           .data$gender_concept_id,
           .data$year_of_birth) %>%
    left_join(x[["death"]] %>%
                select(.data$person_id,
                       .data$death_date,
                       .data$death_datetime),
              by = "person_id") %>%
    left_join(x[["visit_occurrence"]] %>%
                select(.data$person_id,
                       .data$visit_occurrence_id,
                       .data$visit_start_datetime,
                       .data$visit_end_datetime,
                       .data$visit_concept_id,
                       .data$admitting_source_concept_id,
                       .data$discharge_to_concept_id),
              by = "person_id")

}


