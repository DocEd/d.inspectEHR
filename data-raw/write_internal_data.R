## code to prepare internal measurement table checklist
dq_ref <- readr::read_csv("./data/measurements.csv", col_types = "iccccicccciccc")
dq_ans <- readr::read_csv("./data/measurement_answers.csv", col_types = "icicccccc")
core_null_tolerance <- readr::read_csv("./data/core_null_tolerance.csv", col_types = "ccn")
vo_ans <- readr::read_csv("./data/visit_occurrence_answers.csv", col_types = "ciciccc")

usethis::use_data(dq_ref, dq_ans, core_null_tolerance, vo_ans, internal = TRUE, overwrite = TRUE)
