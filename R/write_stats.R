
#' Create Copy of annex Output Template
#'
#' The main aim of the `annex` package is to standardize data sets for the IEA
#' EBC Annex86 project.  To create the output file,
#' [annex::annex_write_stats()] uses a template XLSX file (shipped with the
#' package).  This function allows to make a local copy to check the format of
#' the template if needed.
#'
#' @param file name of the file to be written, must end on
#'        xlsx (not case sensitive).
#' @param overwrite logical, default is \code{FALSE}.
#'        Can be set to \code{TRUE} to overwrite an existing
#'        file (be aware of loss of data).
#'
#' @return No return.
#'
#' @author Reto Stauffer
#' @export
annex_template <- function(file, overwrite = FALSE) {
    stopifnot(is.character(file), length(file) == 1)
    stopifnot(isTRUE(overwrite) || isFALSE(overwrite))

    # Checking file extension; must be xlsx (not case sensitive)
    stopifnot(grepl("\\.xlsx$", file, ignore.case = TRUE))

    # File exists and overwrite = FALSE?
    if (file.exists(file) && !overwrite)
        stop("file \"", file, "\" already exists (`overwrite = FALSE`)")
    # Else trying to make a copy of the template
    template <- system.file("template/template.xlsx", package = "annex")
    # Catch return unused for now
    res      <- tryCatch(file.copy(template, file),
                         error = function(x) stop("cannot create \"", file, "\""))
    invisible(NULL)
}

#' Writing Annex Stats to Disc
#'
#' TODO(R)
#'
#' @param x object of class \code{annex_stat} as returned by
#'        [annex::annex_stats()] (wide or long format).
#' @param file name (or path) to the XLSX file where to store
#'        the data. Must end with `xlsx` (not case sensitive).
#'        See 'Details'.
#' @param user positive integer, the user identifier given
#'        by the project team. Will be appended to the data set.
#' @param overwrite logical, defaults to \code{FALSE}.
#'        See 'Details'.
#' @param \dots not yet used.
#' @param quiet logical. If set \code{TRUE} messages will be printed.
#'
#' @details
#' This function is used to write the annex statistics - the final
#' output - into an XLSX file. The output is based on a template
#' file shipped with the package with a predefined format.
#'
#' If the output `file` does not exist, the template will be copied
#' and modified by (i) saving the data into the "STAT" sheet as well
#' as pre-filling some additional meta sheets which have to be
#' manually edited/entered by the user.
#'
#' By default, `overwrite = FALSE`. If the output `file` already
#' exists, the function will be terminated. However, it can be set
#' to `TRUE` to allow [annex::annex_write_stats()] to manipulate/overwrite
#' the current data in that XLSX file. It tries to preserve all
#' custom data (TODO(R): not yet implemented).
#'
#' @return No return, creates a new XLSX file (see argument `file`)
#' and stores the data, or updates an existing XLSX file (see
#' argument `overwrite`).
#'
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom openxlsx getSheetNames
#' @author Reto Stauffer
#' @export
annex_write_stats <- function(x, file, user, overwrite = FALSE, ..., quiet = FALSE) {
    # Stay sane!
    stopifnot(is.character(file), length(file) == 1)
    stopifnot(is.numeric(user), length(user) == 1, user > 0)
    stopifnot(isTRUE(overwrite) || isFALSE(overwrite))
    if (!grepl("\\.xlsx$", file, ignore.case = TRUE))
        stop("the `file` must end on `.xlsx` (not case sensitive); ",
             file, " is invalid")
    if (!dir.exists(dirname(file)))
        stop("directory '", dirname(file), "' does not exist; can't create output file")

    # Check if the input object is what we expect;
    # Also used as sanity check for the annex_write_stats function.
    x <- annex_stats_reshape(x, format = "wide")
    annex_check_stats_object(x)

    # Create or overwrite file
    if (!file.exists(file) || (file.exists(file) && overwrite)) {
        annex_template(file, overwrite = overwrite)
    } else {
        stop("TODO(R): Mode to overwrite existing data not yet implemented")
    }

    # Ensure all sheets are in the XLSX file (as we don't create them, we manipulate them)
    file_sheets <- getSheetNames(file)
    required_sheets <- c("STAT", "META-Study", "META-Home", "META-Room",
                         "META-Pollutant", "META-Period")
    if (!all(required_sheets %in% file_sheets))
        stop("not all required sheets exist in the XLSX file '", file, "'. Missing: ",
             paste(sprintf("'%s'", required_sheets[!required_sheets %in% file_sheets]), collapse = ", "))

    # Manipulating the data set
    x <- cbind(data.frame(user = sprintf("%04d", user)), x)

    # Open connection; write data
    if (!quiet) {
        message("Starting to write ", appendLF = FALSE) # Hot fix TODO(R)
        message(file)
    }
    workbook <- loadWorkbook(file)

    write_annex_metaStudy(workbook,     x, quiet)
    write_annex_metaHome(workbook,      x, quiet)
    write_annex_metaRoom(workbook,      x, quiet)
    write_annex_metaPollutant(workbook, x, quiet)
    write_annex_metaPeriod(workbook,    x, quiet)
    write_annex_STAT(workbook,          x, quiet)

    # Saving final file
    if (!quiet) message(" - Saving file")
    saveWorkbook(workbook, file, overwrite = TRUE)

}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_STAT <- function(wb, x, quiet, sheet = "STAT") {
    if (!quiet) message(" - Writing ", sheet)
    writeData(wb, sheet = sheet, x = x, colNames = TRUE)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaStudy <- function(wb, x, quiet, sheet = "META-Study") {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study")))
    tmp <- with(tmp, interaction(user, study, sep = "-", drop = TRUE))
    x   <- data.frame(ID           = tmp,
                      Contact      = "<Contact Name>",
                      ORCID        = "<ORCID>",
                      Years        = "<4 Digit Year>",
                      Publications = "<doi:....>",
                      Links        = "<https://...>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaHome <- function(wb, x, quiet, sheet = "META-Home") {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home")))
    tmp <- with(tmp, interaction(user, study, home, sep = "-", drop = TRUE))

    x   <- data.frame(ID                      = tmp,
                      LocationCountry         = "<ISO3>",
                      LocationCity            = "<City Name>",
                      VentilationType         = "<Ventilation Type>",
                      VentilationTypeComment  = "<Comment Vent Type>",
                      VentilationRate         = "<Rate in [l/s]>",
                      VentilationRateMethod   = "<Rate Method>",
                      VentilationRateComment  = "<Rate Comment>",
                      Airtightness            = "<Airtightn>",
                      AirtightnessRefPressure = "<Pressure in [hPa]>",
                      AirtightnessNorm        = "<Normalization Value>",
                      TypeOfBuilding          = "<Type of Building>",
                      SizeOfHome              = "<Size in [m^2]>",
                      TypeOccupants           = "<Occupants Type>",
                      ConstructionType        = "<Construction Type/Materials>",
                      EnergyStandard          = "<Energy Standard>",
                      YearOfConstruction      = "<4 Digit Year>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaRoom <- function(wb, x, quiet, sheet = "META-Room") {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home", "room")))
    tmp <- with(tmp, interaction(user, study, home, room, sep = "-", drop = TRUE))
    x <- data.frame(ID                     = tmp,
                    MeasurementLocation    = "<Measurement location>",
                    FreshAirSupply         = "<Air Supply Description>",
                    VentilationRate        = "<Rate Room in [l/s]>",
                    VentilationRateMethod  = "<Rate Method>",
                    Comments               = "<Comments>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaPollutant <- function(wb, x, quiet, sheet = "META-Pollutant") {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home", "room", "variable")))
    tmp <- with(tmp, interaction(user, study, home, room, variable, sep = "-", drop = TRUE))
    x <- data.frame(ID                     = tmp,
                    PollutantName          = "<Pollutant Name with Details>",
                    PollutantUnit          = "<Measurement Unit>",
                    PollutantInfo          = "<Additional Information>",
                    MeasurementDevice      = "<Measurement Device Info>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaPeriod <- function(wb, x, quiet, sheet = "META-Period") {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home", "room", "variable")))
    tmp <- with(tmp, interaction(user, study, home, room, variable, sep = "-", drop = TRUE))
    x <- data.frame(ID                     = tmp,
                    Definition             = "<Definition>",
                    Comments               = "<Comments>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' Checking Annex Stats to XML
#'
#' Used for checking/validating \code{annex_stat} objects;
#' used internally by [annex::annex_write_stats()] to ensure that
#' what the users (try to) store to the final XLSX files
#' is what is expected for the final output file.
#'
#' @param x object of class \code{annex_stat} as returned by
#'        [annex::annex_stats()].
#'
#' @details The following will be checked:
#'
#' \itemize{
#'   \item Input is of correct type and has at least one observation.
#'   \item
#'   \item
#'   \item
#'   \item
#' }
#'
#' @return No return, will throw an error if something does
#' not match the expected file format.
#'
#' @author Reto Stauffer
#'
annex_check_stats_object <- function(x) {
    stopifnot(inherits(x, "annex_stats"), is.data.frame(x), NROW(x) > 0)

    # The following variables must exist and be either
    # character or factor to count as valid
    must_exist <- c("study", "home", "room", "season", "tod", "variable")
    for (n in must_exist) {
        if (!n %in% names(x)) stop("missing `", n, "'` in the annex stats object")
        if (!is.character(x[[n]]) && !is.factor(x[[n]]))
            stop("`", n, "` must be either character or factor, not ",
                 paste(class(x[[n]]), collapse = ", "))
    }
    # Well, all other variables must be numeric then
    for (n in names(x)[!names(x) %in% must_exist]) {
        if (!is.numeric(x[[n]]))
            stop("`", n, "` must be numeric not ",
                 paste(class(x[[n]]), collapse = ", "))
    }
}



















