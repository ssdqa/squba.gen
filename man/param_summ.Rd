% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_summary.R
\name{param_summ}
\alias{param_summ}
\title{Generate Parameter Summary}
\usage{
param_summ(check_string, ...)
}
\arguments{
\item{check_string}{abbreviation to represent check type, should be the same as what
is prefixed to the names of the output functions}

\item{...}{all of the parameters input into the core function. any argument that is not
able to be vectorized (i.e. a CDM tbl, codeset, etc) will not appear in the final
summary}
}
\value{
a vector with information about which parameters will be required to use
the *_output function for each module; intended to be fed into a cli message
command to generate a console message
}
\description{
This function will summarize the input parameters for the *_process functions, which is
then used to output a string to the console that indicates the appropriate
\code{output_function} to use in the *_output step
}
\examples{
# intended for use inside the *_process functions

param_summ(check_string = 'evp',
           multi_or_single_site = 'single',
           anomaly_or_exploratory = 'exploratory',
           time = FALSE)

}
