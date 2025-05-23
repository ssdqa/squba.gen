% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anomaly_detection.R
\name{detect_outliers}
\alias{detect_outliers}
\title{*_ms_anom_cs Anomaly Detection}
\usage{
detect_outliers(
  df_tbl,
  tail_input = "both",
  p_input = 0.9,
  column_analysis = "prop_concept",
  column_eligible = "analysis_eligible",
  column_variable = "concept_id"
)
}
\arguments{
\item{df_tbl}{tbl for analysis; usually output from \code{\link[=compute_dist_anomalies]{compute_dist_anomalies()}}}

\item{tail_input}{whether to detect anomaly on right, left, or both sides; defaults to \code{both}}

\item{p_input}{the threshold for anomaly; defaults to 0.9}

\item{column_analysis}{a string, which the name of the column for which to compute anomaly detection;}

\item{column_eligible}{a string, which is the name of the column that indicates eligibility for analysis}

\item{column_variable}{a string, which is the name of the variable to compute summary statistics for;}
}
\description{
This function will identify anomalies in a dataframe using the
\code{\link[hotspots:hotspots]{hotspots::outliers()}} function. It assumes:
(1) No time component;
(2) Table has a column indicating whether a particular group or row is eligible for analysis;
(3) column variable exists for which to compute the anomaly
These conditions are met by the output of \code{\link[=compute_dist_anomalies]{compute_dist_anomalies()}}, which is typically
the input for this function
}
\examples{
# create sample input (modeled after EVP)
sample_ms_input <- dplyr::tibble('site' = c('Site A', 'Site A', 'Site A',
                                            'Site A', 'Site B', 'Site B',
                                            'Site B', 'Site B'),
                                 'variable' = c('dx', 'dx', 'drug', 'drug',
                                                'dx', 'dx', 'drug', 'drug'),
                                 'count' = c(100, 140, 39, 42, 137, 111,
                                             12, 15),
                                 'total_var' = c(1000, 1000, 200, 200, 1500,
                                                 1500, 100, 100))
# execute the full analysis, including compute_dist_anomalies and
# detect_outliers
anomaly_output1 <- compute_dist_anomalies(df_tbl = sample_ms_input,
                                          grp_vars = 'variable',
                                          var_col = 'count',
                                          denom_cols = 'total_var')

anomaly_output2 <- detect_outliers(df_tbl = anomaly_output1,
                                   column_analysis = 'count',
                                   column_variable = 'variable')

}
