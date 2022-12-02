

#' Checking Annex Config
#'
#' An 'annex config object' is a simple data.frame which
#' can be created using Rs standard features. The function
#' is checking if the content of the object matches the
#' requirements of being an annex config object.
#' This check will be performed in \code{link{annex()}} automatically,
#' but can also be done by the user manually.
#'
#' @param x object of class \code{data.frame} to be checked.
#'
#' @return Invisibly returns a (possibly modified) version of \code{x}
#' containing only the required columns in a specific order.
#'
#' @details The function checks if the config object is set up properly
#' and contains the required information for preparing the annex data.
#'
#' Fails if:
#' \itemize{
#'      \item TODO(R): checks changed
#'      \item the input is not a \code{data.frame}
#'      \item variables are missing ('column', 'variable', 'study', 'home', 'room')
#'      \item configuration for \code{variable = "datetime"} is missing
#'      \item there is no definition for variables (datetime only)
#'      \item the config contains missing values in the required variables
#'      \item column name for \code{variable = "datetime"} is missing
#'      \item the variable 'column' is not unique
#'      \item there are duplicated entries for the combination of
#'            variable/study/home/room (must be unique)
#'      \item variables study, home or room contain contain non-allowed characters.
#'            Must only contain letters (lowercase or uppercase), numbers,
#'            and underscores. Must start with a letter.
#'}
#'
#' @author Reto Stauffer
#' @export
annex_check_config <- function(x) {
    stopifnot(is.data.frame(x))
    stopifnot(NROW(x) > 0)

    # Required columns
    required_cols <- c("column", "variable", "study", "home", "room")
    if (!all(required_cols %in% names(x)))
        stop("not all required columns (", paste(required_cols, collapse = ", "), ") ",
             "exist in the 'config object'. Missing: ",
             paste(required_cols[!required_cols %in% names(x)], collapse = ", "))

    # Force study, home, and room to NA for variable datetime
    x[which(x$variable == "datetime"), c("study", "home", "room")] <- NA

    # Missing datetime
    if (!any(x$variable == "datetime"))
        stop("missing entry with variable = 'datetime' in config")
    if (nrow(x) == 1 && x$variable == "datetime")
        stop("only datetime column specified, no variables present (makes no sense)")

    # Missing values
    check_na <- sapply(subset(x[, required_cols], variable != "datetime"), function(x) sum(is.na(x)))
    if (any(check_na > 0))
        stop("missing values in ", paste(names(check_na)[check_na > 0], collapse = ", ")," not allowed")
    if (is.na(subset(config, variable == "datetime", select = c(column), drop = TRUE)))
        stop("missing entry for 'column' where variable = 'datetime'")

    # Allowed variable names, study, home, and room
    x$variable <- check_for_allowed_pollutants(x$variable)
    x$room     <- check_for_allowed_rooms(x$room)

    # Duplicated columns?
    idx <- which(duplicated(x$column))
    if (length(idx) > 0)
        stop("entries in column 'column' must be unique; duplicates found for ",
             paste(x$column[idx], collapse = ", "))

    # Ensure that there are no cuplicated variable|study|home|room entries
    idx <- which(duplicated(tmp <- subset(x, select = c(variable, study, home, room))))
    if (length(idx) > 0) {
        cat("Duplicated entries in config for:\n")
        print(tmp[idx, ])
        stop("combination (variable, study, home, room) must be unique")
    }

    # Colons (:) forbidden in study, home, or room as this would break
    # our unique interaction later on.
    for (v in c("variable", "study", "home", "room")) {
        tmp <- subset(x, variable != "datetime", select = v, drop = TRUE)
        idx <- which(!grepl("^[a-z][a-z0-9_]{0,}$", tmp, ignore.case = TRUE))
        if (length(idx) > 0)
            stop("values in variable ", v, " must only contain letters (lower or upper case), ",
                 "numbers, and underscores. Must start with a letter. The following are not allowed: ",
                 paste(tmp[idx], collapse = ", "))

    }

    # Invisible return reduced to the required columns
    invisible(x[, required_cols])
}



#' Checking for Allowed Pollutants
#'
#' The XLSX file template.xlsx contains a series of
#' pre-defined names for the pollutants (sheet 'Definitions').
#' This function checks if all user defined variable names
#' are valid. Not case sensitive; will be adjusted if needed.
#'
#' @param x character vector with variable names.
#'
#' @return Character vector (with possibly adjusted) variable
#' names, or fails.
#'
#' @importFrom openxlsx read.xlsx
#' @author Reto Stauffer
check_for_allowed_pollutants <- function(x) {
    stopifnot(is.character(x), length(x) > 0, !any(is.na(x)))

    # Path to XLSX file to be read
    xlsx <- system.file("template/template.xlsx", package = "annex")

    library("readxl")
    tmp <- suppressMessages(read_excel(xlsx, sheet = "Definitions"))
    stopifnot("Pollutants" %in% names(tmp))
    allowed_pollutants <- na.omit(c("datetime", tmp$Pollutants))

    # Fix casing
    idx_case <- match(tolower(x), tolower(allowed_pollutants))
    if (!all(is.na(idx_case))) x[which(!is.na(idx_case))] <- allowed_pollutants[na.omit(idx_case)]

    # Show error message of not-allowed variable names
    idx <- which(!x %in% allowed_pollutants)
    if (length(idx) > 0)
        stop("variable names ", paste(sprintf("'%s'", x[idx]), collapse = ", "),
             " not allowed. Allowed are: ",
             paste(sprintf("'%s'", allowed_pollutants), collapse = ", "), ".")

    # Return
    return(x)

}


#' Checking for Allowed Rooms
#'
#' The XLSX file template.xlsx contains a series of
#' pre-defined names for the rooms (sheet 'Definitions').
#' This function checks if all user defined room names
#' are valid. Not case sensitive; will be adjusted if needed.
#'
#' @param x character vector with room names.
#'
#' @return Character vector (with possibly adjusted) room
#' names, or fails.
#'
#' @importFrom readxl read_excel
#' @author Reto Stauffer
check_for_allowed_rooms <- function(x) {
    stopifnot(is.character(x), length(x) > 0)

    # Path to XLSX file to be read
    xlsx <- system.file("template/template.xlsx", package = "annex")

    library("readxl")
    tmp <- suppressMessages(read_excel(xlsx, sheet = "Definitions"))
    stopifnot("Measurement location" %in% names(tmp))
    allowed_rooms <- na.omit(tmp[["Measurement location"]])

    # Fix casing
    idx_case <- match(tolower(x), tolower(allowed_rooms))
    if (!all(is.na(idx_case))) x[which(!is.na(idx_case))] <- allowed_rooms[na.omit(idx_case)]

    # Show error message of not-allowed variable names
    idx <- which(!is.na(x) & !x %in% allowed_rooms)
    if (length(idx) > 0)
        stop("room names ", paste(sprintf("'%s'", x[idx]), collapse = ", "),
             " not allowed. Allowed are: ",
             paste(sprintf("'%s'", allowed_rooms), collapse = ", "), ".")

    # Return
    return(x)

}
