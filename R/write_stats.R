
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
#' @param mode, character, writing mode. Can be one of \code{"write"} (default),
#'        \code{"append"} (add new data) or \code{"update"} (update existing data).
#'        See 'Mode' for more information.
#' @param \dots not yet used.
#' @param ask logical. If \code{TRUE} the user will be asked to confirm that
#'        the function will modify the existing \code{file} (used when
#'        \code{mode == "append"} or \code{mode == "update"}). See
#'        Section 'Writing mode' for more information.
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
#' @section Writing mode: There are three writing modes. Warning: Depending on
#' the mode used, existing data can get lost (i.e., removed). The following
#' modes are available:
#'
#' **write**: Default mode, write data into a fresh XLSX file. It is assumed
#' that the output \code{file} does not yet exist. If it exists an error will
#' be thrown as it is unkown if the user would like to append new data to 
#' an existing file or update (overwrite) data in an existing file.
#'
#' **append**: Append data to an existing XLSX file. This mode expects that
#' the \code{file} does already exist and is in the correct format (will check
#' sheets and columns against the template). If \code{file} does not exist or
#' the content of \code{file} does not follow the format of the template, an
#' error will be thrown.
#'
#' \code{mode = "append"} falls back to \code{mode = "write"} if the output \code{file}
#' does not yet exist.
#'
#' Else the data of \code{x} will be appended to the sheet 'STAT' and additional
#' entries in the 'META*' sheets will be created if needed. In case the new object \code{x}
#' contains data which are already in \code{STAT} an error will be thrown
#' (so the new data to be appended must be unique).
#'
#' **update**: Update the data of an existing \code{file}. **Warning:** this
#' will delete (drop) existing data in the sheet 'STAT' and append new entries
#' in the 'META*' sheets (won't delete existing entries). The latter could cause
#' additional warnings when validating the file if there are 'META*' entries
#' which are no longer needed (as the data have been deleted).
#'
#' @return No return, creates a new XLSX file (see argument `file`)
#' and stores the data, or updates an existing XLSX file (see
#' argument section 'Writing mode').
#'
#' @importFrom openxlsx loadWorkbook writeData saveWorkbook
#' @importFrom openxlsx getSheetNames
#' @author Reto Stauffer
#' @export
annex_write_stats <- function(x, file, user, mode = "write", ..., ask = TRUE, quiet = FALSE) {
    # Stay sane!
    stopifnot(is.character(file), length(file) == 1)
    stopifnot(is.numeric(user), length(user) == 1, user > 0)
    if (!grepl("\\.xlsx$", file, ignore.case = TRUE))
        stop("the `file` must end on `.xlsx` (not case sensitive); ",
             file, " is invalid")
    if (!dir.exists(dirname(file)))
        stop("directory '", dirname(file), "' does not exist; can't create output file")

    # Writing mode and additional options
    mode <- match.arg(mode, c("write", "append", "update"))
    if (mode == "append" && !file.exists(file)) mode <- "write" # fallback
    ask   <- as.logical(ask)[1L];     stopifnot(isTRUE(ask) | isFALSE(ask))
    quiet <- as.logical(quiet)[1L];   stopifnot(isTRUE(quiet) | isFALSE(quiet))

    # Check if the input object is what we expect;
    # Also used as sanity check for the annex_write_stats function.
    x <- annex_stats_reshape(x, format = "wide")
    annex_check_stats_object(x)

    # Replacing NaN (not a number) with a simple NA to avoid having
    # cells with #NUMBER! or #ZAHL! or whatever in the spreadsheet later on.
    x[is.na(x)] <- NA

    # Error: mode == "write" while the file exists 
    if (mode == "write" && file.exists(file)) {
        stop(red $ bold("ERROR: 'mode = \"write\"' has been chosen but the output file ",
                        "\"", file, "\" does already exist. Check argument 'mode' ",
                        "in `?annex_write_stats` for more information.", sep = ""))
    #  Else (mode == "write") create a copy of the template
    } else if (mode == "write") {
        if (!quiet) message("Get a copy of the template")
        annex_template(file, overwrite = FALSE)
    } else {
        # If the output file does not exist: WARN
        if (!file.exists(file)) {
            stop(red $ bold("ERROR: 'mode = \"", mode, "\" has been selected which expects that ",
                            "the file \"", file, "\" already exists ",
                            "but the file can not be found.", sep = ""))
        }
    }

    # Ensure all sheets are in the XLSX file (as we don't create them, we manipulate them)
    file_sheets <- getSheetNames(file)
    required_sheets <- c("STAT", "META-Study", "META-Home", "META-Room", "META-Variable")
    if (!all(required_sheets %in% file_sheets))
        stop("not all required sheets exist in the XLSX file '", file, "'. Missing: ",
             paste(sprintf("'%s'", required_sheets[!required_sheets %in% file_sheets]), collapse = ", "))

    # If not mode == "write" we check that the columns in the different
    # sheets match the ones from the template.
    if (!mode == "write") {
        template <- system.file("template/template.xlsx", package = "annex")
        stopifnot("problems to find the template (internal package problem)" = file.exists(template))
        for (sheet in required_sheets) {
            if (sheet %in% "STAT") next
            # compare ...
            cols_template <- names(read_xlsx(template, sheet = sheet, n_max = 1))
            cols_file     <- names(read_xlsx(file,     sheet = sheet, n_max = 1))
            if (!identical(cols_template, cols_file))
                stop(red $ bold("ERROR: Columns in sheet '", sheet, "' in file ",
                                "\"", file, "\" do not match the columns in the template.\n",
                                "Cannot update/append data.\n",
                                "Got:       ", paste(cols_file, collapse = ", "), "\n",
                                "Expected:  ", paste(cols_file, collapse = ", "),
                                sep = ""))
        }
    }

    # Manipulating the data set
    x <- cbind(data.frame(user = sprintf("%04d", user)), x)

    # Open connection; write data
    if (!quiet) {
        message("Starting to write ", appendLF = FALSE) # Hot fix TODO(R)
        message(file)
    }
    workbook <- loadWorkbook(file)

    write_annex_metaStudy(workbook,     x, quiet, update = !mode == "write")
    write_annex_metaHome(workbook,      x, quiet, update = !mode == "write")
    write_annex_metaRoom(workbook,      x, quiet, update = !mode == "write")
    write_annex_metaVariable(workbook,  x, quiet, update = !mode == "write")
    write_annex_STAT(workbook,          x, quiet, update = !mode == "write")

    # Saving final file
    if (!quiet) message(" - Saving file")
    saveWorkbook(workbook, file, overwrite = TRUE)
}

#' @importFrom openxlsx writeData readWorkbook
#' @author Reto Stauffer
write_annex_STAT <- function(wb, x, quiet, sheet = "STAT", update) {
    if (!quiet) message(" - Writing ", sheet)
    writeData(wb, sheet = sheet, x = x, colNames = TRUE)
}

upsert_annex_sheet <- function(wb, x, quiet, sheet, update) {
    # In case update = TRUE (append/update mode) we need to import the existing
    # existing sheet and append the new information in \code{x} if needed.
    if (update) {
        oldx <- readWorkbook(wb, sheet)
        stopifnot(nrow(x) == nrow(oldx))
        # Drop rows in x if the ID already exists in oldx
        x  <- subset(x, !ID %in% oldx$ID)
        nx <- NROW(x) # Keep rows added for message below
        x  <- rbind(oldx, x)
        if (!quiet && nx) message(" - Adding ", nx, " rows into '", sheet, "'", sep = "")
    } else {
        if (!quiet) message(" - Write ", NROW(x), " rows into '", sheet, "'", sep = "")
    }
    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaStudy <- function(wb, x, quiet, sheet = "META-Study", update) {
    if (!quiet) message(" - Writing ", sheet)

    tmp <- unique(subset(x, select = c("user", "study")))
    tmp <- with(tmp, interaction(user, study, sep = "-", drop = TRUE))
    x   <- data.frame(ID           = tmp,
                      Contact      = "<Contact Name>",
                      ORCID        = "<ORCID>",
                      Years        = "<4 Digit Year>",
                      Publications = "<doi:....>",
                      Links        = "<https://...>")
    upsert_annex_sheet(wb, x, quiet, sheet, update)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaHome <- function(wb, x, quiet, sheet = "META-Home", update) {
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

    upsert_annex_sheet(wb, x, quiet, sheet, update)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaRoom <- function(wb, x, quiet, sheet = "META-Room", update) {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home", "room")))
    ID  <- with(tmp, interaction(user, study, home, room, sep = "-", drop = TRUE))
    x <- data.frame(ID                     = ID,
                    MeasurementLocation    = "<Additional room information>",
                    FreshAirSupply         = "<Air Supply Description>",
                    VentilationRate        = "<Rate Room in [l/s]>",
                    VentilationRateMethod  = "<Rate Method>",
                    Comments               = "<Comments>")

    writeData(wb, sheet = sheet, x = x, colNames = FALSE, startRow = 2, startCol = 1)
}

#' @importFrom openxlsx writeData
#' @author Reto Stauffer
write_annex_metaVariable <- function(wb, x, quiet, sheet = "META-Variable", update) {
    if (!quiet) message(" - Writing ", sheet)
    tmp <- unique(subset(x, select = c("user", "study", "home", "room", "variable")))
    ID  <- with(tmp, interaction(user, study, home, room, variable, sep = "-", drop = TRUE))
    x <- data.frame(ID                     = ID,
                    VariableName           = "<Additional variable information>",
                    VariableUnit           = "<Measurement Unit>",
                    VariableInfo           = "<Additional Information>",
                    MeasurementDevice      = "<Measurement Device Info>")

    upsert_annex_sheet(wb, x, quiet, sheet, update)
}

###' @importFrom openxlsx writeData
###' @author Reto Stauffer
##write_annex_metaSeason <- function(wb, x, quiet, sheet = "META-Season") {
##    if (!quiet) message(" - Writing ", sheet)
##    tmp <- unique(subset(x, select = c("user", "study", "home", "room", "variable")))
##    tmp <- with(tmp, interaction(user, study, home, room, variable, sep = "-", drop = TRUE))
##    x <- data.frame(ID                     = tmp,
##                    Definition             = "<Definition>",
##                    Comments               = "<Comments>")
##
##    upsert_annex_sheet(wb, x, quiet, sheet, update)
##}

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
    must_exist <- c("study", "home", "room", "month", "tod", "variable")
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



















