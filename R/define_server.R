#' @importFrom shiny renderPlot renderTable
server <- function(input, output, session) {
  # these tables come from setup-tables.R
  output$row_counts <- row_counts
  output$null_counts <- null_counts
  output$ref_integrity <- ref_integrity

  # person table plots
  output$person_plot_one <- person_plot_one
  output$person_plot_two <- person_plot_two
  output$person_plot_three <- person_plot_three

  # visit characteristics
  output$vo_table <- vo_table
  output$vo_by_visit_type <- vo_by_visit_type
  output$admission_by_admission_type <- admission_by_admission_type
  output$admission_source <- admission_source
  output$discharge_to <- discharge_to
  output$death_discrep_table <- death_discrep_table
  output$death_time_discrep_plot <- death_time_discrep_plot

  # visit sequencing
  output$overlap_table <- overlap_table

  ## There's a scope + reactivity issue here that I can't quite
  ## figure out (which is why it's not in a separate file)
  ## Also, raises the question of dynamically adjusting the UI
  ## otherwise we need to implement all the filters for everything
  ## in the same tabSetPanel
  output$res_adm_plot <- res_adm_plot

  # readmissions
  output$attendances_per_patient_plot <- attendances_per_patient_plot 

  # visit details
  output$visit_detail_transition_plot <- visit_detail_transition_plot
  output$visit_detail_zero_los <- visit_detail_zero_los
  output$visit_detail_los_by_care_site <- visit_detail_los_by_care_site

  # outcomes
  output$outcome_table_with_visit <- outcome_table_with_visit
  output$outcome_table_without_visit <- outcome_table_without_visit

  # distribution of times
  output$distribution_of_times_plot <- distribution_of_times_plot

  # # measurements
  # output$measurements_phase_one <- measurements_phase_one
}
