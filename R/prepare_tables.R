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
    ) %>%
    map(~
          mutate(
            .,
            across(
              c(contains("date"), -contains("datetime")),
              as.Date
            )
          ))
}

#' Prepare Overview Table
#'
#' @param x the output from \code{\link{prepare_tables}}
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select left_join
#' @importFrom rlang .data
#' @importFrom lubridate interval years
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
              by = "person_id") %>%
    mutate(
      age = interval(start = as.Date(paste0(.data$year_of_birth, "-12-31")),
                     end = .data$visit_start_datetime)/years(1))
}


#' Prepare Tally of Clinical Tables
#'
#' @param ctn database connection
#' @param schema character vector of target schema
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_int
#' @importFrom dplyr tbl tally collect pull mutate
#' @importFrom dbplyr in_schema
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
#' @examples
prepare_tally <- function(ctn, schema,
                          tbl_names = c(
                                        "person",
                                        "specimen",
                                        "death",
                                        "visit_occurrence",
                                        "visit_detail",
                                        "procedure_occurrence",
                                        "drug_exposure",
                                        "device_exposure",
                                        "condition_occurrence",
                                        "measurement",
                                        "observation")) {

  clinical_tbls <- tibble(table = tbl_names)

  ttally <- clinical_tbls$table %>%
    map_int(~ tbl(ctn, in_schema(schema, .)) %>%
              tally() %>%
              collect() %>%
              pull() %>%
              as.integer())

  clinical_tbls %>%
    mutate(n = ttally)
}


#' Prepare NULL count information
#'
#' @param st a prepped table list from \code{\link{prepare_tables}}
#'
#' @return
#' @export
#'
#' @importFrom purrr imap
#' @importFrom dplyr bind_rows filter left_join
#'
#' @examples
prepare_null_counts <- function(st) {

  null_counts <- st %>%
    imap(~ summarise_missing(.x, .y)) %>%
    bind_rows() %>%
    filter(!grepl(x = column, pattern = "source"))

  left_join(null_counts, core_null_tolerance,
            by = c("table", "column"))

}


#' Prepare Cohort Boundaries
#'
#' @param x
#'
#' @importFrom dplyr summarise mutate if_else case_when
#' @importFrom tidyr pivot_longer everything
#'
#' @return
#' @export
#'
#' @examples
prepare_cohort_boundaries <- function(x) {
  x %>%
    summarise(
      first_visit_date = min(visit_start_date, na.rm = TRUE),
      first_visit_dttm = min(visit_start_datetime, na.rm = TRUE),
      last_visit_date = max(visit_start_date, na.rm = TRUE),
      last_visit_dttm = max(visit_start_date, na.rm = TRUE),
      first_dc_date = min(visit_end_date, na.rm = TRUE),
      first_dc_dttm = min(visit_end_datetime, na.rm = TRUE),
      last_dc_date = max(visit_end_date, na.rm = TRUE),
      last_dc_dttm = max(visit_end_datetime, na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(everything(), names_to = "observation", values_to = "time") %>%
    mutate(
      comparison = if_else(
        grepl(x = observation, pattern = "first"),
        as.POSIXct("2020-01-01 00:00:00"),
        Sys.time()
      )
    ) %>%
    mutate(
      tolerance = case_when(
        grepl(x = observation, pattern = "first") & time < comparison ~ "fail",
        grepl(x = observation, pattern = "first") & time >= comparison ~ "pass",
        grepl(x = observation, pattern = "last") & time > comparison ~ "fail",
        grepl(x = observation, pattern = "last") & time <= comparison ~ "pass",
        TRUE ~ "fail"
      )
    )
}
