


#' Validate annex Output File
#'
#' Validate XLSX file created by [annex::annex_write_stats()].
#' Checks if all required sheets/columns are available and that
#' all user-modified META information has been entered correctly.
#'
#' @param file name of the file to be validated (XLSX file).
#' @param user positive integer, the user identifier given
#'        by the project team.
#' @param quiet logical, defaults to \code{FALSE}. If \code{TRUE},
#'        the output will be limited.
#' @param \dots currently unused.
#'
#' @return Some checks will cause an error and stop execution.
#' Others will cause a message with some information on what
#' has to be fixed to make the document valid. If the function
#' does not stop due to an error it will return \code{TRUE}
#' if the file has been validated as proper, or \code{FALSE}
#' if issues have been found.
#'
#' @importFrom openxlsx read.xlsx
#' @author Reto Stauffer
#' @export
annex_validate <- function(file, user, quiet = FALSE, ...) {
    stopifnot(is.character(file), length(file) == 1)
    stopifnot(file.exists(file))
    stopifnot(is.numeric(user), length(user) == 1, user > 0)

    # By default setting checkflag to TRUE; will be set to FALSE
    # whenever we encounter a problem.
    checkflag <- TRUE

    # --------------------------------------------
    # Major problems: will stop execution
    # --------------------------------------------

    # Checking for required data sheets
    required_sheets <- c("STAT", "META-Study", "META-Home", "META-Room", "META-Variable", "Definitions")
    file_sheets     <- getSheetNames(file)
    missing_sheets  <- required_sheets[!required_sheets %in% file_sheets]
    if (length(missing_sheets) > 0)
        message("ERROR: missing required sheet(s) ",
                paste(sprintf("'%s'", missing_sheets), collapse = ", "))

    # Checking existence, order, and name of the columns in all sheets
    # Will throw an error if something is fishy/missing.
    tmp <- annex_validate_sheet_columns(file, required_sheets[!required_sheets == "STAT"], quiet)
    checkflag <- checkflag * tmp; rm(tmp)

    # Some of the functions below will check if the META-sheets do contain
    # all user/study/home/room definitions required, and only those. Therefore
    # we need all unique definitions from the STAT table.
    stat_meta <- read.xlsx(file, sheet = "STAT")[-2, c("user", "study", "home", "room", "variable")]

    # Check if the Definitions file is as it should be; Error if not.
    annex_validate_sheet_Definitions(file)

    # Checking content of the different sheets using dedicated functions
    for (sheet in required_sheets) {
        if (sheet == "Definitions") next
        FUN  <- get(sprintf("annex_validate_sheet_%s", sub("^META-", "meta", sheet)))
        args <- list(file = file, user = user, stat_meta = stat_meta, quiet = quiet)
        checkflag <- checkflag * do.call(FUN, args)
    }

    return(as.logical(checkflag))
}


# -------------------------------------------------------------------
# Checking existence, order, and name of the columns of a series of sheets

#' @importFrom openxlsx read.xlsx
annex_validate_sheet_columns <- function(file, sheets, quiet = FALSE) {

    # Template XLSX to compare the user file against
    template_xlsx <- system.file("template/template.xlsx", package = "annex")
    # Default return; will be changed if needed
    checkflag <- TRUE

    # Helper function to read the first row of the XLSX file (header)
    get_line1 <- function(f, sheet) {
        tryCatch(as.character(read.xlsx(f, sheet = sheet, sep.names = " ",
                                   colNames = FALSE, startRow = 1, rows = 1)),
                 warning = function(w) structure(gsub("\n", "", w), class = "warning"))
    }

    # Looping over all sheets
    for (sheet in sheets) {
        if (!quiet) message("- Checking columns in sheet '", sheet, "' ...", appendLF = FALSE)
        templ <- get_line1(template_xlsx, sheet)
        usr   <- get_line1(file, sheet)
        # Got a warning when reading the XLSX sheet
        if (inherits(usr, "warning")) {
            if (!quiet) message(" PROBLEM DETECTED")
            message("- WARNING: sheet '", sheet, "', ", usr)
            checkflag <- checkflag * FALSE
        } else {
            idx   <- usr[seq_along(templ)] == templ
            if (!all(idx)) {
                message(" PROBLEM DETECTED")
                exp <- paste(sprintf("'%s'", usr[seq_along(templ)]), collapse = ", ")
                got <- paste(sprintf("'%s'", templ), collapse = ", ")
                stop("problem in sheet '", sheet, "', expected columns:\n",
                     "expected (", exp, ")\nbut got  (", got, ")")
            }
            if (!quiet) message(" OK")
        }
    }
    return(as.logical(checkflag))
}


# -------------------------------------------------------------------
# Checking the META-Study sheet.
# Checking the main STAT sheet
# This function will always throw an error if there is anything fishy.
annex_validate_sheet_STAT <- function(file, user, quiet, ...) {

    # Reading file
    data <- read.xlsx(file, sheet = "STAT")

    # checking required columns (correct order; columns A:...)
    required_cols <- c("user", "study", "home", "room", "month", "tod", "variable")
    tmp <- names(data)[seq_along(required_cols)] == required_cols
    tmp <- sapply(tmp, function(x) isTRUE(x))
    if (!all(tmp)) {
        stop("VALIDATION: Missing columns ",
                paste(sprintf("'%s'", required_cols[!tmp == TRUE]), collapse = ", "),
                " in sheet `STAT` (or in wrong order)")
    }

    # Checking user
    if (nrow(data) == 0) stop("VALIDATION: No data in sheet 'STAT'")

    # Missing values
    for (col in required_cols) {
        if (any(is.na(data[[col]])))
            stop("missing values (empty cells) in column '", col, "' in sheet 'STAT'")
    }

    # 'tod' must be 'all' or 'XX-XX'
    tmp   <- unique(data$tod)
    check <- grepl("^(all|[0-9]{2}-[0-9]{2})$", tmp)
    if (!all(check))
        stop("column 'tod' in sheet 'STAT' contains illegal values: ",
             paste(sprintf("'%s'", tmp[!check]), collapse = ", "))

    # 'month' must be 'all' or '1' to '12'
    tmp   <- unique(data$month)
    check <- grepl("^(all|[1-9][0-9]?)$", tmp)
    if (!all(check))
        stop("column 'month' in sheet 'STAT' contains illegal values: ",
             paste(sprintf("'%s'", tmp[!check]), collapse = ", "))

    # Checking content
    tmp <- as.integer(unique(data$user))
    if (!all(tmp == user))
        stop("VALIDATION: Unexpected user(s) in sheet 'STAT'. Expected ",
             user, ", sheet contains ", paste(tmp[!tmp == user], collapse = ", "))

    # all other columns must be numeric
    for (col in names(data)[!names(data) %in% required_cols]) {
        if (!is.numeric(data[[col]]))
            stop("column '", col, "' in sheet 'STAT' seems to contain non-numeric values")
    }

    # Check if 'room' is valid
    check_for_allowed_rooms(data$room)
    check_for_allowed_variables(data$variable)

    # If we end up here everything is fine; continue
    return(TRUE)
}

# -------------------------------------------------------------------
# Checking existence, order, and name of the columns of a series of sheets

# Helper function to check META sheets. Contains a series of 'general'
# checks which are identical for all META-* sheets.

#' @importFrom stats na.omit
annex_validate_sheet_ID_check <- function(data, sheet, stat_meta, ID) {

    # Reading meta sheet
    stopifnot(is.character(ID), length(ID) > 0, all(ID %in% names(stat_meta)))

    checkflag <- TRUE # Initialization

    # Getting levels in 'STAT' sheet (ID_stats; based on meta_stat)
    # as well as those in the meta file (ID_meta; based on data$ID)
    ID_stats <- levels(interaction(stat_meta[ID], drop = TRUE, sep = "-"))
    ID_meta  <- na.omit(unique(data$ID))

    if (any(is.na(data$ID))) {
        message("- WARNING: column 'ID' in sheet '", sheet, "' contains empty/missing values")
        checkflag <- checkflag * FALSE
    }
    # Removing ID = NA for further checks (already warned above)
    data <- subset(data, !is.na(ID))

    # Missing definition?
    tmp <- setdiff(ID_stats, ID_meta)
    if (length(tmp) > 0) {
        message("- WARNING: sheet '", sheet, "' missing definition for ID(s): ",
                paste(sprintf("'%s'", tmp), collapse = ", "))
        checkflag <- checkflag * FALSE
    }

    # Additional unrequired definition in meta?
    tmp <- setdiff(ID_meta, ID_stats)
    if (length(tmp) > 0) {
        message("- WARNING: sheet '", sheet, "' contains additional ID(s): ",
                paste(sprintf("'%s'", tmp), collapse = ", "))
        checkflag <- checkflag * FALSE
    }

    # Checking for missing META information
    for (col in names(data)[!names(data) == "ID"]) {
        idx <- which(is.na(data[[col]]))
        if (length(idx) > 0) {
            message("- WARNING: '", sheet, "' column '", col, "' missing info for IDs: ",
                    paste(sprintf("'%s'", data$ID[idx]), collapse = ", "))
            checkflag <- checkflag * FALSE
        }
    }

    return(as.logical(checkflag))
}


#' @importFrom openxlsx read.xlsx
annex_validate_sheet_metaStudy <- function(file, quiet, stat_meta, ..., sheet = "META-Study") {

    # Default return; will be changed if needed
    checkflag <- TRUE

    # Reading the data
    data     <- read.xlsx(file, sheet, sep.names = " ")
    data[grepl("^<.*>$", data)] <- NA # Replacing placeholders with NA

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta, c("user", "study"))
    checkflag <- checkflag * tmp; rm(tmp)

    return(as.logical(checkflag))
}

#' @importFrom openxlsx read.xlsx
#' @importFrom stats na.omit
annex_validate_sheet_metaHome <- function(file, quiet, stat_meta, ..., sheet = "META-Home") {

    # Default return; will be changed if needed
    checkflag <- TRUE

    # Reading the data
    data     <- read.xlsx(file, sheet, sep.names = " ")
    data[grepl("^<.*>$", data)] <- NA # Replacing placeholders with NA

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta,
                                         ID = c("user", "study", "home"))
    checkflag <- checkflag * tmp; rm(tmp)

    # Are Loctions given in ISO3?
    iso3 <- na.omit(unique(data[, "Location: Country"]))
    idx  <- which(!grepl("^[a-zA-Z0-9]]{3}$", iso3))
    if (length(idx) > 0) {
        message("- WARNING: 'Location: Country' in '", sheet, "' not ISO3 standard, found: ",
                paste(sprintf("'%s'", iso3[idx]), collapse = ", "))
        checkflag <- checkflag * FALSE
    }

    return(as.logical(checkflag))
}

#' @importFrom openxlsx read.xlsx
annex_validate_sheet_metaRoom <- function(file, quiet, stat_meta, ..., sheet = "META-Room") {

    # Default return; will be changed if needed
    checkflag <- TRUE

    # Reading the data
    data     <- read.xlsx(file, sheet, sep.names = " ")
    data[grepl("^<.*>$", data)] <- NA # Replacing placeholders with NA

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta,
                                         ID = c("user", "study", "home", "room"))
    checkflag <- checkflag * tmp; rm(tmp)

    return(as.logical(checkflag))
}

#' @importFrom openxlsx read.xlsx
annex_validate_sheet_metaVariable <- function(file, quiet, stat_meta, ..., sheet = "META-Variable") {

    # Default return; will be changed if needed
    checkflag <- TRUE

    # Reading the data
    data     <- read.xlsx(file, sheet, sep.names = " ")
    data[grepl("^<.*>$", data)] <- NA # Replacing placeholders with NA

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta,
                                         ID = c("user", "study", "home", "room", "variable"))
    checkflag <- checkflag * tmp; rm(tmp)

    return(as.logical(checkflag))
}


#' @importFrom openxlsx read.xlsx
annex_validate_sheet_Definitions <- function(file, sheet = "Definitions") {
    template_xlsx <- system.file("template/template.xlsx", package = "annex")
    templ <- read.xlsx(template_xlsx, sheet = sheet)
    user  <- read.xlsx(file, sheet = sheet)
    idx   <- templ == user
    if (!all(is.na(idx) | idx))
        stop("the '", sheet, "' table does not match the template (has been manipulated)")
    return(NULL)
}
