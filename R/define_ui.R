#' @importFrom shiny navbarPage tabPanel sidebarLayout sidebarPanel 
#' checkboxInput mainPanel tabsetPanel tableOutput plotOutput
ui <- navbarPage("QC report", id = "nav",
  tabPanel("Structural checks",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("binary_variable_one", "Tick Box?", value = TRUE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Row counts", tableOutput("row_counts")),
          tabPanel("Null counts", tableOutput("null_counts"))
        )
      )
    )
  ),
  tabPanel("Integrity checks",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("binary_variable_one", "Tick Box?", value = TRUE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Referential integrity", tableOutput("ref_integrity"))
        )
      )
    )
  ),
  tabPanel("Person table",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("binary_variable_person", "By groups?", value = TRUE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Sex distribution", plotOutput("person_plot_one")),
          tabPanel("Ethnicity distribution", plotOutput("person_plot_two")),
          tabPanel("Age distribution", plotOutput("person_plot_three"))
        )
      )
    )
  ),
  tabPanel("Visit Characteristics",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("binary_variable_one", "Tick Box?", value = TRUE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Date boundaries", tableOutput("vo_table")),
          tabPanel("Visits by visit type", tableOutput("vo_by_visit_type")),
          tabPanel("Admission profile by type of admission", plotOutput("admission_by_admission_type")), # empty on dummy data
          tabPanel("Admission source", plotOutput("admission_source")),
          tabPanel("Discharge To", plotOutput("discharge_to")),
          tabPanel("Death discrepancy table", tableOutput("death_discrep_table")),
          tabPanel("Death time discrepancy plot", plotOutput("death_time_discrep_plot"))
        )
      )
    )
  ),
  tabPanel("Visit Sequencing",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("remove_negative", "Remove negative values", value = FALSE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Overlap table", tableOutput("overlap_table")),
          tabPanel("Length of stay", plotOutput("res_adm_plot"))
        )
      )
    )
  ),
  tabPanel("Readmissions",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("remove_negative", "Remove negative values", value = FALSE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Attendances per patient", plotOutput("attendances_per_patient_plot"))
        )
      )
    )
  ),
  tabPanel("Visit Details",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("remove_negative", "Remove negative values", value = FALSE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Patient movement", plotOutput("visit_detail_transition_plot")),
          tabPanel("Visit detail with zero LOS", tableOutput("visit_detail_zero_los")),
          tabPanel("LOS by location", plotOutput("visit_detail_los_by_care_site"))
        )
      )
    )
  ),
  tabPanel("Outcomes",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("remove_negative", "Remove negative values", value = FALSE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Outcome (with visit)", tableOutput("outcome_table_with_visit")),
          tabPanel("Outcome (without visit)", tableOutput("outcome_table_without_visit"))
        )
      )
    )
  ),
  tabPanel("Times",
    sidebarLayout(
      sidebarPanel(width = 2,
        checkboxInput("remove_negative", "Remove negative values", value = FALSE)
      ),
      mainPanel(width = 10,
        tabsetPanel(
          tabPanel("Distribution of times", plotOutput("distribution_of_times_plot"))
        )
      )
    )
  )
  # ,
  # tabPanel("Measurements",
  #   sidebarLayout(
  #     sidebarPanel(width = 2,
  #       checkboxInput("remove_negative", "Remove negative values", value = FALSE)
  #     ),
  #     mainPanel(width = 10,
  #       tabsetPanel(
  #         tabPanel("Distribution of times", tableOutput("measurements_phase_one"))
  #       )
  #     )
  #   )
  # )
)
