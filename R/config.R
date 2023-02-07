

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
    x$variable <- check_for_allowed_variables(x$variable)
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



#' Checking for Allowed Variables
#'
#' The XLSX file template.xlsx contains a series of
#' pre-defined names for the variables (sheet 'Definitions').
#' This function checks if all user defined variable names
#' are valid. Not case sensitive; will be adjusted if needed.
#'
#' @param x character vector with variable names.
#'
#' @return Character vector (with possibly adjusted) variable
#' names, or fails.
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom stats na.omit
#' @author Reto Stauffer
check_for_allowed_variables <- function(x) {
    stopifnot(is.character(x), length(x) > 0, !any(is.na(x)))

    # Path to XLSX file to be read
    allowed_variables <- annex_variable_definition()$name

    # Fix casing
    idx_case <- match(tolower(x), tolower(allowed_variables))
    if (!all(is.na(idx_case))) x[which(!is.na(idx_case))] <- allowed_variables[na.omit(idx_case)]

    # Show error message of not-allowed variable names
    idx <- which(!x == "datetime" & !x %in% allowed_variables)
    if (length(idx) > 0)
        stop("variable names ", paste(sprintf("'%s'", x[idx]), collapse = ", "),
             " not allowed. Allowed are: ",
             paste(sprintf("'%s'", allowed_variables), collapse = ", "), ".")

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
#' @author Reto Stauffer
check_for_allowed_rooms <- function(x) {
    stopifnot(is.character(x), length(x) > 0)
    allowed_rooms <- annex_room_definition()$name

    # Fix casing
    idx_case <- match(tolower(x), tolower(allowed_rooms))
    if (!all(is.na(idx_case))) x[which(!is.na(idx_case))] <- allowed_rooms[na.omit(idx_case)]

    # Show error message of not-allowed variable names
    pattern <- sprintf("^(%s)[0-9]{0,2}$", paste(allowed_rooms, collapse = "|"))
    idx <- which(!is.na(x) & !grepl(pattern, x))
    if (length(idx) > 0)
        stop("room names ", paste(sprintf("'%s'", x[idx]), collapse = ", "),
             " not allowed. Allowed are: ",
             paste(sprintf("'%s'", allowed_rooms), collapse = ", "),
             " followed by up to two digits",
             sprintf("(e.g., '%1$s', '%1$s1', '%1$s2' or '%1$s35')", allowed_rooms[[1]]))


    # Return
    return(x)

}



#' Variable information
#'
#' The template not only contains the definition of the allowed variables,
#' it also states whether or not additional information is required (or
#' optional) and an upper and lower bound to be considered 'valid'.
#' Used for quality flags.
#'
#' @details If \code{as_list = TRUE} a list of lists is returned, else
#' a \code{data.frame}.
#'
#' **List:** The name of the list corresponds to the name of the variable,
#' whereas each entry contains a list with a logical flag if additional
#' information in the META sheet is \code{required} as well as a numeric
#' \code{lower} and \code{upper} bound which defines in which range
#' an observation is considered to be valid. Can be \code{NA} if not
#' specified (both or one of them).
#'
#' If \code{as_list = FALSE} (default) the same information is returned
#' as a \code{data.frame} containing the same information.
#'
#' @return Returns either a \code{data.frame} or \code{list} of \code{lists}
#' which contains the allowed (defined) variables.
#'
#' @seealso check_for_allowed_variables
#'
#' @author Reto Stauffer
#' @export
annex_variable_definition <- function(as_list = FALSE) {
    # Path to XLSX file to be read
    template_xlsx <- system.file("template/template.xlsx", package = "annex")
    tmp <- suppressMessages(read.xlsx(template_xlsx, sheet = "Definitions", sep.names = " "))
    required <- c("Variable" = "name",
                  "Pollution: Additional information required" = "required",
                  "Lower bound" = "lower",
                  "Upper bound" = "upper")
    stopifnot(names(required) %in% names(tmp))

    # Extracting information needed
    res <- subset(tmp, select = names(required))
    names(res) <- unname(required)
    res <- transform(subset(res, !is.na(name)),
                     required = as.logical(required),
                     lower = as.numeric(lower),
                     upper = as.numeric(upper))
    if (as_list) {
        res <- setNames(lapply(seq_len(NROW(res)),
                               function(i) as.list(subset(res[i, ], select = -name))),
                        res$name)
    }
    return(res)
}


#' Room information
#'
#' The template contains a series of base abbrevations allowed to define
#' a room alongside the 'long name'. This function returns the definition
#' as a \code{data.frame}.
#'
#' @return \code{data.frame} with base room abbrevation plus long name.
#'
#' @seealso check_for_allowed_rooms
#'
#' @author Reto Stauffer
#' @export
annex_room_definition <- function() {
    # Path to XLSX file to be read
    template_xlsx <- system.file("template/template.xlsx", package = "annex")
    tmp <- suppressMessages(read.xlsx(template_xlsx, sheet = "Definitions", sep.names = " "))
    required <- c("Measurement location"             = "name",
                  "Measurement location description" = "long_name")
    stopifnot(names(required) %in% names(tmp))

    # Extracting information needed
    res <- subset(tmp, select = names(required))
    names(res) <- unname(required)
    res <- subset(res, !is.na(name))
    return(res)
}

