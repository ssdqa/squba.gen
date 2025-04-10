% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fact_loops.R
\name{compute_fot}
\alias{compute_fot}
\title{Compute Facts Over Time}
\usage{
compute_fot(
  cohort,
  check_func,
  site_col,
  reduce_id = "visit_type",
  time_period = "year",
  time_span = c("2012-01-01", "2022-12-31"),
  site_list
)
}
\arguments{
\item{cohort}{a cohort tbl with a column for \code{site}, \code{person_id}, \code{start_date} and \code{end_date}}

\item{check_func}{the base function for the check that needs to be executed across time; this argument
should be structured as the following, where dat is the input data for the function:

\if{html}{\out{<div class="sourceCode">}}\preformatted{              function(dat)\{check_function(param1 = dat, param2 = param2_input, ...,
              paramX = paramX_input)\}

              all parameters for the base check function should be included if any defaults are not being
              used
}\if{html}{\out{</div>}}}

\item{site_col}{the name of the column in the cohort that contains the site names}

\item{reduce_id}{if the check provided in @check_func returns a list of tables, this is
the ID that should be used to reduce the tables into one dataframe}

\item{time_period}{when time = TRUE, this argument defines the distance between dates within the specified time period. defaults
to \code{year}, but other time periods such as \code{month} or \code{week} are also acceptable}

\item{time_span}{when time = TRUE, this argument defines the start and end dates for the time period of interest. should be
formatted as c(start date, end date) in yyyy-mm-dd date format}

\item{site_list}{A list of sites for which you would like to examine clinical facts. Can be one site
(single-site) or multiple (multi-site)}
}
\value{
one dataframe where the output of @check_func has been executed for each @time_period in
the provided @time_span for each of the sites included in @site_list
}
\description{
Compute Facts Over Time
}
\examples{
\dontrun{
# for a function like...
 mock_function <- function(cohort,
                           input_tbl){

     test <- input_tbl \%>\%
        inner_join(cohort) \%>\%
        group_by(site) \%>\%
        summarise(n_row = n())

     return(test)
 }

# allow this function to be executed for each time period in a series
cohort <- dplyr::tibble('person_id' = c(1,2,3,4),
                        'site' = c('Site A', 'Site A', 'Site B', 'Site B'),
                        'start_date' = c('2012-01-01', '2014-01-10',
                                         '2015-03-05, '2020-01-07'),
                        'end_date' = c('2015-01-01', '2019-01-10',
                                       '2010-03-05, '2024-01-07'))

compute_fot(cohort = cohort,
            check_func = function(dat){
              mock_function(cohort = dat,
                            input_tbl = cdm_tbl('condition_occurrence'))
            },
            site_col = 'site',
            reduce_id = NULL,
            time_period = 'year',
            time_span = c('2012-01-01','2022-12-31'),
            site_list = c('Site A', 'Site B'))


}

}
