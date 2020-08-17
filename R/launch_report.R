# This is a gross hack to get the tables into the package namespace
# and avoid the `cannot change value of locked binding for 'p'` error
# Should really think about functionalising these? but I don't want to
# recreate them a bunch of times for the same plots
# think about more
p <- NULL
vo <- NULL
vd <- NULL
d <- NULL

#' Launch the shiny version of the report
#'
#' @param parms_list list version of \code{parms} from
#' \code{\link{render_report}}.
#'
#' @importFrom shiny runApp
#'
#' @return
#' @export
launch_shiny_report <- function(connection = NULL, schema = "decovid_omop") {

  ctn <- connection

  st <- prepare_tables(ctn, schema)
  overview <- prepare_overview(st)

  assignInMyNamespace('p', st[["person"]] %>% collect())
  assignInMyNamespace('vo', st[["visit_occurrence"]] %>% collect())
  assignInMyNamespace('vd', st[["visit_detail"]] %>% collect())
  assignInMyNamespace('d', st[["death"]] %>% collect())

  runApp(
    list(ui = ui, server = server),
    port = 7884,
  )
}
