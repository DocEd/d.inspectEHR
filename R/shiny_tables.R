#' @importFrom tibble tibble
#' @import dplyr 
#' @importFrom dbplyr in_schema
#' @importFrom purrr map
#' @importFrom rlang !! .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains everything
#' @importFrom forcats fct_infreq
#' 
NULL

row_counts <- renderTable({
  tibble(
    table = c("person", "death", "visit_occurrence", "visit_detail"),
    row_count = c(nrow(p), nrow(d), nrow(vo), nrow(vd))
  ) 
}, striped = TRUE)

null_counts <- renderTable({
  bind_rows(
    p %>%
      summarise_all(~ sum(is.na(.) | . == -1)) %>%
      pivot_longer(everything(), names_to = "column", values_to = "n missing") %>%
      tibble::add_column(table = "person", .before = TRUE) %>%
      mutate(percentage = round((`n missing`/nrow(p)*100), 0)),
    d %>%
      summarise_all(~ sum(is.na(.) | . == -1)) %>%
      pivot_longer(everything(), names_to = "column", values_to = "n missing") %>%
      tibble::add_column(table = "death", .before = TRUE) %>%
      mutate(percentage = round((`n missing`/nrow(d)*100), 0)),
    vo %>%
      summarise_all(~ sum(is.na(.) | . == -1)) %>%
      pivot_longer(everything(), names_to = "column", values_to = "n missing") %>%
      tibble::add_column(table = "visit_occurrence", .before = TRUE) %>%
      mutate(percentage = round((`n missing`/nrow(vo)*100), 0)),
    vd %>%
      summarise_all(~ sum(is.na(.) | . == -1)) %>%
      pivot_longer(everything(), names_to = "column", values_to = "n missing") %>%
      tibble::add_column(table = "visit_detail", .before = TRUE) %>%
      mutate(percentage = round((`n missing`/nrow(vd)*100), 0))
  )
}, striped = TRUE)

ref_integrity <- renderTable({
  tibble::tribble(
    ~ test, ~ count, ~ percentage,
      "persons not in visit_occurrence",
        nrow(anti_join(p, vo, by = "person_id")),
        round((nrow(anti_join(p, vo, by = "person_id"))/nrow(p))*100, 0),
      "visit_occurrence without people",
        nrow(anti_join(vo, p, by = "person_id")),
        round((nrow(anti_join(vo, p, by = "person_id"))/nrow(vo))*100, 0),
      "death without people",
        nrow(anti_join(d, p, by = "person_id")),
        round((nrow(anti_join(d, p, by = "person_id"))/nrow(d))*100, 0),
      "visit_occurrence without visit_detail",
        nrow(anti_join(vo, vd, by = "visit_occurrence_id")),
        round((nrow(anti_join(vo, vd, by = "visit_occurrence_id"))/nrow(vo))*100, 0),
      "visit_detail without visit_occurrence",
        nrow(anti_join(vd, vo, by = "visit_occurrence_id")),
        round((nrow(anti_join(vd, vo, by = "visit_occurrence_id"))/nrow(vo))*100, 0)
  )
}, striped = TRUE)

vo_table <- renderTable({
  res <- vo %>%
    summarise(
      first_date = min(visit_start_datetime, na.rm = T),
      last_date = max(visit_start_datetime, na.rm = T)
    )

  return(res)
}, striped = TRUE)

vo_by_visit_type <- renderTable({
  vo %>%
    group_by(visit_concept_id) %>%
    summarise(
      count = n(),
      percentage = round((n()/nrow(vo))*100, 0)
    )
}, striped = TRUE)

death_discrep_table <- renderTable({
  res <- overview %>%
    filter(
      !is.na(visit_concept_id),
      !is.na(death_datetime),
      discharge_to_concept_id == "Patient died") %>%
    group_by(person_id) %>%
    filter(max(visit_start_datetime) == visit_start_datetime) %>%
    ungroup() %>%
    select(person_id, death_datetime, visit_end_datetime) %>%
    mutate(diff = as.integer(difftime(visit_end_datetime, death_datetime, units = "hours"))) %>%
    ungroup() %>%
    summarise(
      `death > 24 hours before visit` = sum(diff > 24, na.rm = TRUE),
      `death within acceptable limits` = sum(diff <= 24 & diff >= -24, na.rm = TRUE),
      `death > 24 hours after visit` = sum(diff < -24, na.rm = TRUE),
    )

  return(res)
}, striped = TRUE)

overlap_table <- renderTable({
  overview %>%
    filter(!is.na(visit_occurrence_id)) %>%
    group_by(person_id) %>%
    arrange(person_id, visit_start_datetime) %>%
    select(person_id, visit_start_datetime, visit_end_datetime) %>%
    mutate(
      occurrences_ordered = lead(visit_start_datetime) > visit_start_datetime,
      occurrences_nonoverlap = lead(visit_start_datetime) > visit_end_datetime
    ) %>%
    group_by(occurrences_ordered) %>%
    tally() 
}, striped = TRUE)

visit_detail_zero_los <- renderTable({
  vd <- vd %>%
    mutate(
      los = as.numeric(difftime(
        visit_detail_end_datetime,
        visit_detail_start_datetime,
        units = "hours"
       ))
    )

  vd %>%
    group_by(care_site_id, zero_los = los == 0) %>%
    summarise(
      count = n()
    ) 
}, striped = TRUE)

outcome_table_with_visit <- renderTable({
  overview %>%
    distinct(person_id, .keep_all = TRUE) %>%
    filter(!is.na(visit_occurrence_id)) %>%
    group_by(visit_concept_id, death = !is.na(death_datetime)) %>%
    tally() 
}, striped = TRUE)

outcome_table_without_visit <- renderTable({
  overview %>%
    distinct(person_id, .keep_all = TRUE) %>%
    filter(is.na(visit_occurrence_id)) %>%
    group_by(death = !is.na(death_datetime)) %>%
    tally() 
}, striped = TRUE)

# measurements_phase_one <- renderTable({
#   phase1 %>%
#     filter(!(concept_id %in% labs)) %>%
#     select(concept_id, concept_name)
# }, striped = TRUE)
