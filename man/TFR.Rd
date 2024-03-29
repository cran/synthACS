% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a_data-documentation.R
\docType{data}
\name{TFR}
\alias{TFR}
\title{Total Fertility Rate by race of mother}
\format{
A \code{data.frame} with 175 observations and 3 variables.
\describe{
 \item{year}{The year for which data was was recorded.}
 \item{race}{The racial group of the mothers. One of \code{all} all races; \code{white} 
 non-hispanic whites; \code{black_aa} black / African-American; \code{nat_amer} American Indian
 or Native Alaskan; \code{asian_isl} Asian or Pacific Islander; \code{hisp_lat} Hispanic or Latin 
 American.}
 \item{tfr}{The Total Fertility Rate. See Details} 
}
}
\source{
\url{https://www.cdc.gov/nchs/nvss/births.htm}
}
\usage{
TFR
}
\description{
A dataset containing total fertility rate data by race of the mother. Data for all 
races is provided for 1970-2014 and for individual races from 1989-2014.
}
\section{Details}{

The Total Fertility Rate is defined as the sums of the birth rates for the 5-year age groups
found in \code{\link{BR2014}} multiplied by 5.
}

\references{
Hamilton, Brady E., et al. "Births: final data for 2014." National Vital Statistics 
Reports 64.12 (2015): 1-64.
}
\keyword{datasets}
