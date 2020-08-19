#' Create a transition matrix from visit details
#'
#' @param overview overview table from \code{\link{prepare_overview}}
#' @param st short_table list from \code{\link{prepare_tables}}
#' @param possible_states a character vector of the possible states to be used
#'   in the markov chain fitting process
#'
#' @return
#' @export
#'
#' @importFrom markovchain markovchainFit
#' @importFrom dplyr filter left_join select group_by summarise mutate if_else
#'   case_when n bind_rows arrange
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_pad
#'
#' @examples
transition_matrix <- function(overview, st, possible_states) {

  vd <- overview %>%
    filter(.data$visit_concept_id == "Emergency Room and Inpatient Visit") %>%
    left_join(st[["visit_detail"]] %>%
              select(.data$visit_detail_id,
                     .data$visit_occurrence_id,
                     .data$visit_detail_start_datetime,
                     .data$visit_detail_end_datetime,
                     .data$care_site_id),
              by = "visit_occurrence_id")

  caps <- vd %>%
    group_by(.data$visit_occurrence_id) %>%
    summarise(
      admit = unique(.data$admitting_source_concept_id),
      discharge = unique(.data$discharge_to_concept_id)
    ) %>%
    mutate(
      admit = if_else(
        admit == "Home",
        "External (Pt Home)",
        "External (Pt Other)"),
      discharge = case_when(
        discharge == "Patient died" ~ "Died",
        discharge == "Home" ~ "External (Pt Home)",
        !is.na(discharge) ~ "External (Pt Other)",
        TRUE ~ NA_character_)
    ) %>%
    pivot_longer(cols = admit:discharge,
                 names_to = "ord",
                 values_to = "care_site_id")

  care_site_t <- vd %>%
    arrange(.data$visit_detail_start_datetime) %>%
    select(.data$visit_occurrence_id, .data$care_site_id) %>%
    group_by(.data$visit_occurrence_id) %>%
    mutate(ord = paste0("b", str_pad(1:n(), width = 3, pad = "0")))

  tm <- bind_rows(caps, care_site_t) %>%
    arrange(visit_occurrence_id, ord) %>%
    split(.$visit_occurrence_id) %>%
    map(function(x) x$care_site_id)

  tm <- markovchainFit(tm,
                       confint = FALSE,
                       possibleStates = possible_states)
}
