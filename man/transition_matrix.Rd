% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transition_matrix.R
\name{transition_matrix}
\alias{transition_matrix}
\title{Create a transition matrix from visit details}
\usage{
transition_matrix(overview, st, possible_states)
}
\arguments{
\item{overview}{overview table from \code{\link{prepare_overview}}}

\item{st}{short_table list from \code{\link{prepare_tables}}}

\item{possible_states}{a character vector of the possible states to be used
in the markov chain fitting process}
}
\value{

}
\description{
Create a transition matrix from visit details
}
