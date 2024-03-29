% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{annex_variable_definition}
\alias{annex_variable_definition}
\title{Variable definition information}
\usage{
annex_variable_definition(as_list = FALSE)
}
\arguments{
\item{as_list}{logical. If \code{FALSE} (default) a \code{data.frame}
will be returned, if \code{TRUE} a list (see Details).}
}
\value{
Returns either a \code{data.frame} or \code{list} of \code{lists}
which contains the allowed (defined) variables.
}
\description{
The template not only contains the definition of the allowed variables,
it also states whether or not additional information is required (or
optional), an upper and lower bound to be considered 'valid' plus
(is specified) a series of allowed units.
Used to prepare the data and convert to annex standard units, quality checks,
as well as validation.
}
\details{
If \code{as_list = TRUE} a list of lists is returned, else
a \code{data.frame}.

\strong{List:} The name of the list corresponds to the name of the variable,
whereas each entry contains a list with a logical flag if additional
information in the META sheet is \code{required} as well as a numeric
\code{lower} and \code{upper} bound which defines in which range
an observation is considered to be valid. Can be \code{NA} if not
specified (both or one of them). \code{allowed_units} contains \code{NA}
(unspecified) or a character wich one or multiple comma separated units
specifications.

If \code{as_list = FALSE} (default) the same information is returned
as a \code{data.frame} containing the same information.
}
\seealso{
annex_variable_definition, annex_room_definition, annex_country_definition
}
\author{
Reto Stauffer
}
