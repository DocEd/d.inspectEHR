#' @import ggplot2
#' @importFrom forcats fct_rev fct_infreq
#' @importFrom markovchain markovchainFit
#' @importFrom hms as_hms
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains everything
#' @importFrom tibble add_column
shiny_sex <- renderPlot({
  plot_sex(p)
  # p %>%
  #   group_by(gender_concept_id) %>%
  #   tally() %>%
  #   ggplot(aes(x = gender_concept_id)) +
  #   geom_point(aes(y = n)) +
  #       geom_segment(aes(
  #         y = 0,
  #         yend = n,
  #         xend = gender_concept_id)) +
  #   theme_classic() +
  #   labs(y = "count", x = "categories") +
  #   theme(
  #     plot.title.position = "plot",
  #     axis.title.y = element_blank()) +
  #   coord_flip() +
  #   ggtitle("Sex distribution")
})

person_plot_two <- renderPlot({
  p %>%
    mutate(race_concept_id = gsub("England and Wales ethnic category 2011 census", "", race_concept_id)) %>%
    mutate(race_concept_id = fct_rev(fct_infreq(as.factor(race_concept_id)))) %>%
    group_by(race_concept_id) %>%
    tally() %>%
    ggplot(aes(x = race_concept_id)) +
    geom_point(aes(y = n)) +
        geom_segment(aes(
          y = 0,
          yend = n,
          xend = race_concept_id)) +
    theme_classic() +
    labs(y = "count", x = "categories") +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_blank()) +
    coord_flip() +
    ggtitle("Ethnicity Distribution")
})

person_plot_three <- renderPlot({
  p %>%
    mutate(age = as.integer((2020 - year_of_birth)/10)*10) %>%
    group_by(age) %>%
    tally() %>%
    ggplot(aes(x = factor(age))) +
    geom_point(aes(y = n)) +
        geom_segment(aes(
          y = 0,
          yend = n,
          xend = factor(age))) +
    theme_classic() +
    labs(y = "count") +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_blank()) +
      coord_flip() +
    ggtitle("Age Distribution")
})

admission_by_admission_type <- renderPlot({
  vo %>%
    mutate_at(vars(visit_start_datetime), ~ as.Date(.)) %>%
    group_by(visit_start_datetime, visit_concept_id) %>%
    tally() %>%
    ggplot(aes(x = visit_start_datetime, y = n,
               group = visit_concept_id, colour = visit_concept_id)) +
    geom_path() +
    theme_classic() +
    labs(y = "patient attendances", x = "arrival date", colour = "visit type") +
    theme(legend.position = "bottom") +
    ggtitle("Admission profile by type of admission")
})

admission_source <- renderPlot({
  vo %>%
    mutate(admitting_source_concept_id = fct_infreq(as.factor(admitting_source_concept_id))) %>%
    group_by(admitting_source_concept_id) %>%
    tally() %>%
    ggplot(aes(x = admitting_source_concept_id)) +
    geom_point(aes(y = n)) +
    geom_segment(aes(
      y = 0,
      yend = n,
      xend = admitting_source_concept_id)) +
    theme_classic() +
    labs(y = "count") +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_blank()) +
      coord_flip() +
    ggtitle("Admission Source")
})

discharge_to <- renderPlot({
  vos <- vo %>%
    mutate(discharge_to_concept_id = fct_infreq(as.factor(discharge_to_concept_id))) %>%
    group_by(discharge_to_concept_id) %>%
    tally()

  vos %>%
    ggplot(aes(x = discharge_to_concept_id)) +
    geom_point(aes(y = n)) +
    geom_segment(aes(
      y = 0,
      yend = n,
      xend = discharge_to_concept_id)) +
    #geom_text(aes(label = n, y = -10, x = discharge_to_concept_id)) +
    theme_classic() +
    labs(y = "count") +
    theme(
      plot.title.position = "plot",
      axis.title.y = element_blank()) +
    coord_flip() +
    ggtitle("Discharge To")
})

death_time_discrep_plot <- renderPlot({
  overview %>%
    filter(
      !is.na(visit_concept_id),
      !is.na(death_datetime),
      discharge_to_concept_id == "Patient died") %>%
    group_by(person_id) %>%
    filter(max(visit_start_datetime) == visit_start_datetime) %>%
    ungroup() %>%
    select(person_id, death_datetime, visit_end_datetime) %>%
    mutate(diff = as.integer(difftime(visit_end_datetime, death_datetime, units = "hours"))) %>%
    ggplot(aes(x = diff)) +
    geom_density() +
    theme_classic() +
    labs(x = "time difference (hours) between visit end and death") +
    ggtitle("Difference between visit end and death time for in patient deaths")
})

res_adm_plot <- renderPlot({
  out_df <- overview %>%
    filter(!is.na(visit_occurrence_id)) %>%
    mutate(
      outcome = if_else(!is.na(death_datetime), 1L, 0L, missing = as.integer(NA)),
      los = case_when(
        outcome == 1L ~ as.numeric(difftime(death_datetime, visit_start_datetime, units = "days")),
        outcome == 0L ~ as.numeric(difftime(visit_end_datetime, visit_start_datetime, units = "days")),
        TRUE ~ as.numeric(NA)
        )) %>%
    select(person_id, outcome, los) %>%
    filter(!is.na(los))

  ggplot(data = out_df, aes(x = los)) +
    geom_density() +
    theme_classic() +
    labs(x = "length of stay (days)")
})

# A version of this ^ plot with the negative lengths-of-stays removed
# should be added once I've figured out how to do reactivity.

attendances_per_patient_plot <- renderPlot({
  visit_n <- overview %>%
    filter(!is.na(visit_occurrence_id)) %>%
    group_by(person_id) %>%
    tally(name = "attendance_number") %>%
    group_by(attendance_number) %>%
    tally()

  visit_n %>%
    ggplot() +
    geom_linerange(
      aes(xmin = 0, xmax = n, y = attendance_number, group = attendance_number)) +
    geom_point(aes(x = n, y = attendance_number)) +
    scale_y_reverse(limits = c(max(visit_n$attendance_number), 1)) +
    theme_classic() +
    labs(y = "attendances per patient", x = "count")
})

visit_detail_transition_plot <- renderPlot({
  care_site_t <- vd %>%
    filter(!is.na(visit_occurrence_id)) %>%
    arrange(visit_detail_start_datetime) %>%
    split(.$visit_occurrence_id) %>%
    map(function(x) x$care_site_id)

  x <- markovchainFit(care_site_t)

  as_tibble(x$estimate@transitionMatrix) %>%
    add_column(source = rownames(x$estimate@transitionMatrix), .before = TRUE) %>%
    pivot_longer(-source) %>%
    rename(destination = name) %>%
    ggplot(aes(y = source, x = destination)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 2)), colour = "white") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
})

visit_detail_los_by_care_site <- renderPlot({
  vd %>%
    mutate(
      care_site_id = fct_infreq(as.factor(care_site_id)),
      los = as.numeric(
        difftime(visit_detail_end_datetime, visit_detail_start_datetime, units = "days")
      )
    ) %>%
    ggplot(aes(y = los, x = care_site_id)) +
    geom_boxplot() +
    coord_flip() +
    theme_classic() +
    labs(y = "hours in area", x = "care site")
})

distribution_of_times_plot <- renderPlot({
  overview %>%
    select(visit_start_datetime, visit_end_datetime, death_datetime) %>%
    mutate_all(~ as.POSIXct(., tz = "UTC")) %>%
    filter_all(any_vars(!is.na(.))) %>%
    mutate_all(~ as_hms(.)) %>%
    pivot_longer(everything(), names_to = "time_point", values_to = "time") %>%
    ggplot(aes(x = time, fill = time_point)) +
    geom_density(alpha = 0.5) +
    theme_classic() +
    labs(x = "event time distribution", fill = "event") +
    theme(legend.position = "bottom")
})
