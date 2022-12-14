% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_stats.R
\name{annex_template}
\alias{annex_template}
\title{Create Copy of annex Output Template}
\usage{
annex_template(file, overwrite = FALSE)
}
\arguments{
\item{file}{name of the file to be written, must end on
xlsx (not case sensitive).}

\item{overwrite}{logical, default is \code{FALSE}.
Can be set to \code{TRUE} to overwrite an existing
file (be aware of loss of data).}
}
\value{
No return.
}
\description{
The main aim of the \code{annex} package is to standardize data sets for the IEA
EBC Annex86 project.  To create the output file,
\code{\link[=annex_write_stats]{annex_write_stats()}} uses a template XLSX file (shipped with the
package).  This function allows to make a local copy to check the format of
the template if needed.
}
\author{
Reto Stauffer
}
