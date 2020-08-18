#' Plot Age
#'
#' @param x
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by tally
#' @importFrom ggplot2 ggplot aes geom_point geom_segment labs theme ggtitle
#'   element_blank
#'
#' @return
#' @export
#'
#' @examples
plot_age <- function(x) {
  x %>%
    mutate(age = cut(age, c(18, seq(20, 140, by = 5)))) %>%
    group_by(age) %>%
    tally() %>%
    ggplot(aes(y = age)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = age)) +
    theme_d() +
    labs(y = "count") +
    theme(axis.title.y = element_blank()) +
    ggtitle("Age Distribution")
}

#' Plot Sex
#'
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
plot_sex <- function(x) {
  x %>%
    group_by(gender_concept_id) %>%
    tally() %>%
    ggplot(aes(y = gender_concept_id)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = gender_concept_id)) +
    labs(y = "count", x = "categories") +
    theme_d()+
    theme(axis.title.y = element_blank()) +
    ggtitle("Sex distribution")
}


#' Plot Ethnicity
#'
#'
#' @param x
#'
#' @importFrom dplyr mutate if_else group_by tally
#' @importFrom forcats fct_infreq fct_rev
#' @importFrom ggplot2 ggplot aes geom_point geom_segment labs theme ggtitle
#'
#' @return
#' @export
#'
#' @examples
plot_ethnicity <- function(x) {
  x %>%
    mutate(
      race_concept_id = gsub(
        " - England and Wales ethnic category 2011 census",
        "",
        race_concept_id)) %>%
    mutate(
      race_concept_id = if_else(
        nchar(race_concept_id) >= 30,
        paste0(stringr::str_sub(race_concept_id, 1, 29), "..."),
        race_concept_id)) %>%
    mutate(
      race_concept_id = forcats::fct_rev(
        forcats::fct_infreq(race_concept_id))) %>%
    group_by(race_concept_id) %>%
    tally() %>%
    ggplot(aes(y = race_concept_id)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = race_concept_id)) +
    labs(y = "count", x = "categories") +
    theme_d() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Ethnicity Distribution")

}

#' Plot Visit Profile
#'
#' @param x
#'
#' @importFrom dplyr mutate_at vars group_by tally
#' @importFrom ggplot2 ggplot aes geom_path theme labs
#'
#' @return
#' @export
#'
#' @examples
plot_visit_profile <- function(x) {
  x %>%
    mutate_at(vars(visit_start_datetime), ~ as.Date(.)) %>%
    group_by(visit_start_datetime, visit_concept_id) %>%
    tally() %>%
    ggplot(aes(x = visit_start_datetime, y = n,
               group = visit_concept_id, colour = visit_concept_id)) +
    geom_path() +
    theme_d() +
    labs(y = "patient attendances", x = "arrival date", colour = "visit type") +
    theme(legend.position = "bottom") +
    ggtitle("Admission profile by type of admission")
}

#' DECOVID Plot Theme
#'
#' @param ... arguments to pass to \code{theme}
#'
#' @importFrom ggplot2 %+replace% theme theme_bw element_blank
#'
#' @return
#' @export
#'
#' @examples
theme_d <- function(...) {
  pct <- theme_bw(base_family = "sans", base_size = 11) %+replace%
    theme(
      plot.title.position = "plot",
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank())
}
