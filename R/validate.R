


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
#' @import crayon
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
        stop(red $ bold("ERROR: missing required sheet(s) ",
             paste(sprintf("'%s'", missing_sheets), collapse = ", ")))

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
        cat(bold("Validating XLSX sheet", sheet, "\n"))
        FUN  <- get(sprintf("annex_validate_sheet_%s", sub("^META-", "meta", sheet)))
        args <- list(file = file, user = user, stat_meta = stat_meta, quiet = quiet)
        tmp  <- do.call(FUN, args)   # Calling function
        if (tmp) cat(green("  Everything OK (no warnings)\n"))
        checkflag <- checkflag * tmp # Store flag
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
    cat(bold("Checking columns in all sheets\n"))
    for (sheet in sheets) {
        dots <- paste(rep(".", 20 - 2 - nchar(sheet)), collapse = "")
        if (!quiet) message(sprintf("  %-20s", sprintf("'%s' %s", sheet, dots)), appendLF = FALSE)
        templ <- get_line1(template_xlsx, sheet)
        usr   <- get_line1(file, sheet)
        # Got a warning when reading the XLSX sheet
        if (inherits(usr, "warning")) {
            if (!quiet) message(" PROBLEM DETECTED")
            message(yellow("- WARNING: sheet '", sheet, "', ", usr))
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
            if (!quiet) message(green(" OK"))
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
    required_cols <- c("user", "study", "home", "room", "month", "tod", "variable",
                       "quality", "interval_Min", "interval_Q1", "interval_Median",
                       "interval_Mean", "interval_Q3", "interval_Max")
    tmp <- names(data)[seq_along(required_cols)] == required_cols
    tmp <- sapply(tmp, function(x) isTRUE(x))
    if (!all(tmp)) {
        missing <- required_cols[!required_cols %in% names(data)[seq_along(required_cols)]]
        if (length(missing > 0)) {
            stop(red $ bold("Missing columns in sheet 'STAT'.\n",
                    "  Missing: ", paste(sprintf("'%s'", missing), collapse = ", "), "\n",
                    "  First ", length(required_cols), " columns expected to be:\n  ",
                    paste(sprintf("'%s'", required_cols), collapse = ", "), sep = ""))
         } else {
            stop(red $ bold("Wrong order of columns in sheete 'STAT'.\n",
                    "  First ", length(required_cols), " columns expected to be:\n  ",
                    paste(sprintf("'%s'", required_cols), collapse = ", "), sep = ""))
         }
    }

    # Checking user
    if (nrow(data) == 0) stop(red $ bold("No data in sheet 'STAT'"))

    # Missing values
    for (col in required_cols) {
        idx <- is.na(data[[col]])
        if (any(idx))
            stop(red $ bold("Found ", sum(idx), " missing values (empty cells) in column '", col,
                            "' in sheet 'STAT',\n", get_row_info(idx), sep = ""))
    }

    # 'tod' must be 'all' or 'XX-XX'
    check <- !grepl("^(all|[0-9]{2}-[0-9]{2})$", data$tod)
    if (any(check))
        stop(red $ bold("column 'tod' in sheet 'STAT' contains illegal values: ",
             paste(sprintf("'%s'", unique(data$tod[check])), collapse = ", "), "\n",
             get_row_info(check), sep = ""))

    # 'month' must be 'all' or '1' to '12'
    check <- !grepl("^(all|[1-9][0-9]?)$", data$month)
    if (any(check))
        stop(red $ bold("column 'month' in sheet 'STAT' contains illegal values: ",
             paste(sprintf("'%s'", tmp[!check]), collapse = ", "), "\n",
             get_row_info(check), sep = ""))

    # Checking content
    tmp <- as.integer(unique(data$user))
    if (!all(tmp == user))
        stop(red $ bold("unexpected user(s) in sheet 'STAT'. Expected ",
             user, ", sheet contains ", paste(tmp[!tmp == user], collapse = ", "), "\n",
             get_row_info(check), sep = ""))

    # Check that quality and interval_ columns are numeric as well 
    # as all other non-required columns.
    tmp <- required_cols[grepl("^(quality|interval_.*)$", required_cols)]
    for (col in c(tmp, names(data)[!names(data) %in% required_cols])) {
        if (!is.numeric(data[[col]]))
            stop(red $ bold("column '", col, "' in sheet 'STAT' seems to contain non-numeric values", sep = ""))
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
        message(yellow("  WARNING: column 'ID' in sheet '", sheet, "' contains empty/missing values"))
        checkflag <- checkflag * FALSE
    }
    # Removing ID = NA for further checks (already warned above)
    data <- subset(data, !is.na(ID))

    # Missing definition?
    tmp <- setdiff(ID_stats, ID_meta)
    if (length(tmp) > 0) {
        message(yellow("  WARNING: sheet '", sheet, "' ",
                sprintf("missing definition for ID%s: ", ifelse(length(tmp) == 1, "", "s")),
                paste(sprintf("'%s'", tmp), collapse = ", "), sep = ""))
        checkflag <- checkflag * FALSE
    }

    # Additional unrequired definition in meta?
    tmp <- setdiff(ID_meta, ID_stats)
    if (length(tmp) > 0) {
        message(yellow("  WARNING: sheet '", sheet, "' ",
                sprintf("contains additional ID%s: ", ifelse(length(tmp) == 1, "", "s")),
                paste(sprintf("'%s'", tmp), collapse = ", "), sep = ""))
        checkflag <- checkflag * FALSE
    }


    # Checking for missing META information
    required_cols <- get_required_columns(sheet) # Required columns
    for (col in names(data)[!names(data) == "ID"]) {
        is_required <- col %in% required_cols
        idx <- if (is_required) {
            which(is.na(data[[col]]) | grepl("^<.*>$", data[[col]]))
        } else {
            grep("^<.*>$", data[[col]])
        }
        if (length(idx) > 0) {
            if (is_required) {
                # This is considered as a problem
                message(yellow("  WARNING: '", sheet, "' column '", col, "' missing required info for",
                        get_ID_info(data$ID[idx]), " (", get_row_info(idx, n = 4, prefix = "row"), ")", sep = ""))
                checkflag <- checkflag * FALSE
            } else {
                # This is just a NOTE not an problem per se
                message("  NOTE: '", sheet, "' column '", col, "' provides no info for",
                        get_ID_info(data$ID[idx]), " (", get_row_info(idx, n = 4, prefix = "row"), ")", sep = "")
            }
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

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta,
                                         ID = c("user", "study", "home"))
    checkflag <- checkflag * tmp; rm(tmp)

    # Are Loctions (if given) in ISO3 standard?
    colname <- "Location: Country"
    idx     <- which(!is.na(data[[colname]]) & !data[[colname]] %in% ISO3166$ISO3)
    if (length(idx) > 0) {
        message(yellow("  WARNING: '", colname, "' in '", sheet, "' ",
                "not in ISO3 standard, found: ",
                paste(data[[colname]][idx], collapse = ", "),
                " (", get_row_info(idx, prefix = "row"), ").\n",
                "  See `?annex_countries` to get all allowed country abbrevations (ISO3166 alpha-3; ISO3).",
                sep = ""))
        checkflag <- checkflag * FALSE
    }

    # Is the year of construction (if given) a four digit year?
    colname <- "Year of contruction / major renovation (four digit year)"
    idx     <- which(!is.na(data[[colname]]) &
                     !grepl("^<.*>$", data[[colname]]) &
                     !grepl("^[0-9]{4}$", data[[colname]]))
    if (length(idx) > 0) {
        message(yellow("  WARNING: '", colname, "' in '", sheet, "' ",
                "given in wrong format. Found ",
                paste(data[[colname]][idx], collapse = ", "),
                " (", get_row_info(idx, prefix = "row"), ").\n", sep = ""))
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

    # Checking ID column and missing/empty fields
    tmp <- annex_validate_sheet_ID_check(data, sheet, stat_meta,
                                         ID = c("user", "study", "home", "room", "variable"))
    checkflag <- checkflag * tmp; rm(tmp)

    # Depending on the variable "Variable: additional Info" must be provided.
    vars_req <- annex_variable_definition()
    vars_req <- subset(vars_req, required)$name
    colname  <- "Variable additional information"
    idx <- which((is.na(data[[colname]]) | grepl("^<.*>$", data[[colname]])) &
                 grepl(sprintf("-(%s)$", paste(vars_req, collapse = "|")), data$ID))
    if (length(idx) > 0) {
        message(yellow("  WARNING: '", colname, "' in '", sheet, "' ",
                "missing (required for this variable). ",
                get_row_info(idx, prefix = "Row"), sep = ""))
        checkflag <- checkflag * FALSE
    }

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



#' Get row information
#'
#' Used for warning and error messages. Returns a printable
#' string with the information where to find these missing, invalid, ... values
#' in the XLSX file.
#'
#' @param x integer or logical, see 'Details'.
#' @param n integer, defaults to \code{5}. Number of rows to be explicitly listed (if any).
#' @param offset integer, number of additional header lines in the XLSX sheet to correct
#'        the row indicator (defaults to \code{1}).
#' @param prefix character or \code{NULL}, default \code{"  Row"}.
#'
#' @details
#' If the input is of class integer it is assumed that the values correspond to the
#' observations in a \code{data.frame}. I.e, \code{1} is the first observation in the
#' \code{data.frame}.
#'
#' If the input is a logical vector it is assumed that all entries being \code{TRUE}
#' are suspicious, and that the order corresponds to the observations in a \code{data.frame}.
#'
#' The return value will point to these suspicous rows but in the context of an XLSX file
#' which typically has one additional header line (thus the default \code{offset = 1L}).
#'
#' @return Character string with the information where suspicious values have been
#' found in the XLSX file. If there are no suspicious rows, \code{FALSE} is returned.
#'
#' @author Reto Stauffer
get_row_info <- function(x, n = 5L, offset = 1L, prefix = "  Row") {
    stopifnot(is.logical(x) || is.integer(x))
    if (is.logical(x)) x <- which(x)

    n      <- as.integer(n)[1L];      stopifnot(n >= 1)
    offset <- as.integer(offset)[1L]; stopifnot(offset >= 0)
    stopifnot(is.null(prefix) || (is.character(prefix) && length(prefix) == 1L))

    if (length(x) == 0) {
        res <- FALSE
    } else {
        res <- paste(if (is.null(prefix)) "" else sprintf("%s%s: ", prefix, ifelse(length(x) == 1, "", "s")),
                     paste(head(x, n = n) + offset, collapse = ", "),
                     ifelse(length(x) > n, sprintf(" and %d more", length(x) - n), ""),
                     sep = "")
    }
    return(res)
}

#' Get ID information
#'
#' Used for warning and error messages. Returns a printable
#' string with IDs which have been identified.
#'
#' @param x character vector.
#' @param n integer, defaults to \code{5}. Number of rows to be explicitly listed (if any).
#' @param prefix character or \code{NULL}, default \code{"ID"}.
#'
#' @return Character string with the information where suspicious values have been
#' found in the XLSX file. If there are no suspicious rows, \code{FALSE} is returned.
#'
#' @author Reto Stauffer
get_ID_info <- function(x, n = 5L, prefix = " ID") {
    stopifnot(is.character(x))
    n      <- as.integer(n)[1L];      stopifnot(n >= 1)
    stopifnot(is.null(prefix) || (is.character(prefix) && length(prefix) == 1L))

    if (length(x) == 0) {
        res <- FALSE
    } else {
        res <- paste(if (is.null(prefix)) "" else sprintf("%s%s: ", prefix, ifelse(length(x) == 1, "", "s")),
                     paste(head(x, n = n), collapse = ", "),
                     ifelse(length(x) > n, sprintf(" and %d more", length(x) - n), ""),
                     sep = "")
    }
    return(res)
}


#' Get required columns for warnings and infos
#'
#' Different sheets contain different columns which require having the user
#' to specify meta information or other details (no empty cells allowed).
#' This function returns the names of these columns (as in the XLSX file)
#' used for validation. If no definition is available, \code{NULL} is returned.
#'
#' @param sheet character, name of the XLSX sheet.
#'
#' @return Returns \code{NULL} if there is no definition/no required columns,
#' or a character vector with the exact column name as used in XLSX.
#'
#' @author Reto Stauffer
get_required_columns <- function(sheet) {
    stopifnot(is.character(sheet), length(sheet) == 1L)

    # Definition
    def <- list("META-Study" = c("Contact", "Institution"),
                "META-Home"  = c("Location: Country", "Ventilation type"),
                "META-Variable" = "Variable unit")

    return(if (sheet %in% names(def)) def[[sheet]] else NULL)
}




