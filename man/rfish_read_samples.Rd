% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rna-fish.R
\name{rfish_read_samples}
\alias{rfish_read_samples}
\title{Read samples table from snakemake pipeline output}
\usage{
rfish_read_samples(path, ...)
}
\arguments{
\item{path}{Path to the samples excel.}

\item{...}{Forwarded to \code{readxl::read_excel}}
}
\value{
tibble of samples table
}
\description{
Read samples table from snakemake pipeline output
}
\examples{
#TODO
}
