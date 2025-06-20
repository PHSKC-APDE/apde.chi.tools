% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chi_count_by_age.R
\name{chi_count_by_age}
\alias{chi_count_by_age}
\title{Generate Age-Specific Counts for Community Health Indicators}
\usage{
chi_count_by_age(ph.data = NULL, ph.instructions = NULL, source_date = NULL)
}
\arguments{
\item{ph.data}{A data table containing line level data to be analyzed}

\item{ph.instructions}{A data table of calculation instructions created by
\code{\link{chi_generate_tro_shell}}}

\item{source_date}{Date of data source, added to output metadata}
}
\value{
Returns a data.table containing age-specific counts with standard CHI groupings:
 \itemize{
   \item \code{indicator_key}: Health metric identifier
   \item \code{year}: Year range of data (e.g., "2019-2021", "2023", etc.)
   \item \code{tab}: Visualization tab type (e.g., "_kingcounty", "demgroups",
   etc.)
   \item \code{cat1}, \code{cat1_varname}, \code{cat1_group}: Primary
   stratification variable details
   \item \code{cat2}, \code{cat2_varname}, \code{cat2_group}: Secondary
   stratification variable details (if applicable)
   \item \code{chi_age}: Age value (0-100)
   \item \code{count}: Number of cases or events in that demographic-age group
   \item \code{source_date}: Date of data source (if provided)
 }
}
\description{
Creates a detailed breakdown of counts by age for CHI data analysis using
instructions created by \code{\link{chi_generate_tro_shell}}. Primarily
used for age standardization and rate calculations when combined with
population estimates. Processes data according to provided instructions and
handles demographic groupings with special treatment for race and ethnicity
variables.
}
\seealso{
\code{\link{chi_generate_tro_shell}} which creates ph.instructions

\code{\link{chi_generate_instructions_pop}} which uses the output of the output
of \code{chi_count_by_age}
}
