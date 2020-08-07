#' Render Report
#'
#' @param output an output file path
#' @param prams a string "ask", which will launch an interactive window to enter
#'   database connection details, or a list that contains these details:
#'
#'   - driver: string, can be any of: "PostgreSQL", "ODBC", or "SQLite"
#'   - host: string, host name
#'   - port: integer, port number
#'   - dbname: string, database name
#'   - schema: string, database target schema
#'   - user: string, database username
#'   - password: string, database user password
#'   - local_hospital: string, local hospital
#'
#' @return
#' @export
render_report <- function(output = NULL, prams = "ask") {

  if (is.null(output)) {
    rlang::abort("please provide an output file path")
  }

  report_pth <- system.file("analysis_report.Rmd", package = "d.inspectEHR")
  rmarkdown::render(report_pth, params = prams,
                    output_file = output)

}
