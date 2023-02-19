
#' Country codes (ISO 3166) alpha-2 and alpha-3
#'
#' List of countries with ISO-2 and ISO-3 abbrevations.
#' Used to validate the final XLSX file.
#'
#' @return Returns a \code{data.frame} containing
#' the \code{country} name alongside \code{ISO2}
#' and \code{ISO3} short name (ISO 3116 alpha-2 and alpha-3
#' standard).
#'
#' @references IBAN (2023). COUNTRY CODES ALPHA-2 & ALPHA-3,
#' \url{https://www.iban.com/country-codes}, accessed 2023-02-11.
#'
#' @name ISO3166
#' @docType data
#' @author Reto Stauffer
#' @keywords data countries
NULL


#' Country codes (ISO 3166) alpha-2 and alpha-3
#'
#' @return Returns the data set \code{ISO3166} shipped
#' with the package (see \code{?ISO3166} for details).
#'
#' @export
#' @author Reto Stauffer
annex_countries <- function() {
    file <- system.file("data/ISO3166.rda", package = "annex", mustWork = TRUE)
    load(file)
    return(ISO3166)
}
