


#' Validate annex Output File
#'
#' Validate XLSX file created by [annex::annex_write_stats()].
#' Checks if all required sheets/columns are available and that
#' all user-modified META information has been entered correctly.
#'
#' @param file name of the file to be validated (XLSX file).
#' @param user positive integer, the user identifier given
#'        by the project team.
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
annex_validate <- function(file, user, ...) {
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
    required_sheets <- c("STAT", "META-Study", "META-Home", "META-Room",
                         "META-Pollutant", "META-Period", "META-Time", "Definitions")
    file_sheets     <- getSheetNames(file)
    missing_sheets  <- required_sheets[!required_sheets %in% file_sheets]
    if (length(missing_sheets) > 0)
        message("ERROR: missing required sheet(s) ",
                paste(sprintf("'%s'", missing_sheets), collapse = ", "))

    required_sheets <- "STAT"
    for (sheet in required_sheets) {
        FUN <- get(sprintf("annex_validate_sheet_%s", sheet))
        checkflag <- min(checkflag, FUN(file, options = list(user = user)))
    }

    return(as.logical(checkflag))
}

# Checking the main STAT sheet
# This function will always throw an error if there is anything fishy.
annex_validate_sheet_STAT <- function(file, options) {

    # Reading file
    data <- read.xlsx(file, sheet = "STAT")

    # checking required columns (correct order; columns A:...)
    required_cols <- c("user", "study", "home", "room", "season", "tod", "variable")
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

    # Season and tod (for now) must be 'all' or 'XX-XX'
    for (col in c("season", "tod")) {
        tmp   <- unique(data[[col]])
        check <- grepl("^(all|[0-9]{2}-[0-9]{2})$", tmp)
        if (!all(check))
            stop("column '", col, "' in sheet 'STAT' contains illegal values: ",
                 paste(sprintf("'%s'", tmp[!check]), collapse = ", "))
    }

    # Checking content
    tmp <- as.integer(unique(data$user))
    if (!all(tmp == options$user))
        stop("VALIDATION: Unexpected user(s) in sheet 'STAT'. Expected ",
             options$user, ", got ", paste(tmp[!tmp == options$user], collapse = ", "))

    # all other columns must be numeric
    for (col in names(data)[!names(data) %in% required_cols]) {
        if (!is.numeric(data[[col]]))
            stop("column '", col, "' in sheet 'STAT' seems to contain non-numeric values")
    }

    # Check if 'room' is valid
    check_for_allowed_rooms(data$room)
    check_for_allowed_pollutants(data$variable)

    # If we end up here everything is fine; continue
    return(TRUE)
}

