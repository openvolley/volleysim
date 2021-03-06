% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rates.R
\name{vs_estimate_rates}
\alias{vs_estimate_rates}
\title{Estimate parameters required for simulation}
\usage{
vs_estimate_rates(
  x,
  target_team,
  by = "none",
  moderate = TRUE,
  process_model = "all"
)
}
\arguments{
\item{x}{datavolleyplays: the plays component of a datavolley object as returned by \code{\link[datavolley:read_dv]{datavolley::dv_read()}}}

\item{target_team}{string: the team name to calculate rates for. If missing or NULL, rates will be calculated across the entire data.frame \code{x}. If \code{target_team} is "each", rates will be calculated for each team separately}

\item{by}{string: grouping to calculate rates by. Either "none" (calculate whole-data set rates), "match" (by match), or "set" (by match and set)}

\item{moderate}{logical: if \code{TRUE}, apply some checks to attempt to ensure that the estimated rates are reasonable. Currently these checks include:
\itemize{
\item setting error rates are limited to a maximum of 5\%. Some scouts do not include setting actions, except where they are errors or otherwise exceptional, which can lead to unrealistic estimates of setting error rates
}}

\item{process_model}{string: estimate the rates required for which process model? Either "sideout", "phase", or "all"}
}
\value{
A tibble, with columns match_id (if \code{by} is "match"), set_number (if \code{by} is "set"), and (if \code{target_team} is "each") "team". The remaining columns depend on the \code{process_model}: for "sideout", column sideout. If \code{process_model} is "phase" then additionally the columns serve_ace, serve_error, rec_loss_other, rec_att_error, rec_att_kill, rec_att_replayed, rec_no_att, rec_block, trans_loss_other, trans_att_error, trans_att_kill, trans_att_replayed, trans_no_att, and trans_block
}
\description{
Estimate parameters required for simulation
}
\examples{
\dontrun{
  library(datavolley)
  x <- dv_read(dv_example_file())
  rates <- vs_estimate_rates(x, target_team = "each")

  vs_simulate_set(rates) ## simulate a single set
  vs_simulate_match(rates) ## simulate a match
  ## so given the performances of the two teams during that match, we expect
  ##  that the home team should have won, with 3-0 being the most likely scoreline

  ## compare to the actual match result
  summary(x)
}

}
\seealso{
\code{\link[=vs_simulate_set]{vs_simulate_set()}}
}
