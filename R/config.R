

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
#' Throws errors if:
#' \itemize{
#'      \item the input is not a \code{data.frame}
#'      \item variables are missing ('column', 'variable', 'unit', 'study', 'home', 'room')
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
    required_cols <- c("column", "variable", "unit", "study", "home", "room")
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
    tmp <- subset(x[, required_cols[!grepl("^unit$", required_cols)]], variable != "datetime")
    check_na <- sapply(tmp, function(x) sum(is.na(x)))
    if (any(check_na > 0))
        stop("missing values in ", paste(names(check_na)[check_na > 0], collapse = ", ")," not allowed")
    if (is.na(subset(x, variable == "datetime", select = c(column), drop = TRUE)))
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

    # Checking units provided by the user; if the variable definition contains
    # allowed_units the user _must_ specify a valid unit, else the user is not
    # allowed to specify anything (must be empty).
    split_allowed <- function(x) {
        if (is.na(x)) {
            res <- NA
        } else {
            res <- unname(sapply(strsplit(x, ",")[[1]], trimws))
            res <- if (sum(nchar(x) > 0) == 0) NA else res[nchar(res) > 0]
        }
        return(res)
    }

    vars <- annex_variable_definition()
    for (i in seq_len(NROW(x))) {
        if (x$variable[i] == "datetime") next
        allowed <- split_allowed(subset(vars, name == x$variable[i], select = allowed_units, drop = TRUE))
        # Unit given but there are no allowed units
        if (all(is.na(allowed)) && !is.na(x$unit[i])) {
            stop("column \"", x$column[i], "\" variable \"", x$variable[i], "\" ",
                 "has a unit defined (\"", x$unit[i], "\"; obs. ", i,
                 ") but no units are allowed for \"",
                 x$variable[i], "\" (see `annex_variable_definition()`).", sep = "")
        } else if (any(!is.na(allowed)) && is.na(x$unit[i])) {
            stop("column \"", x$column[i], "\" variable \"", x$variable[i], "\" ",
                 "has no unit defined (obs. ", i, ").\n",
                 "  Must be one of: ",
                 paste(allowed, collapse = ", "),
                 " (see `annex_variable_definition()`).", sep = "")
        } else if (any(!is.na(allowed)) && !is.na(x$unit[i])) {
            check   <- grepl(sprintf("^%s$", x$unit[i]), allowed, ignore.case = TRUE)
            if (!any(check))
                stop("column \"", x$column[i], "\" variable \"", x$variable[i], "\" ",
                     "contains invalid unit (", x$unit[i], "). Must be one of: ",
                     paste(allowed, collapse = ", "),
                     " (see `annex_variable_definition()`).", sep = "")
            x$unit[i] <- allowed[check]
        }
    }


    # Colons (:) forbidden in study, home, or room as this would break
    # our unique interaction later on.
    for (v in c("variable", "study", "home", "room")) {
        tmp <- subset(x, variable != "datetime", select = v, drop = TRUE)
        idx <- which(!grepl("^[a-z][a-z0-9_]{0,}$", tmp, ignore.case = TRUE))
        if (length(idx) > 0)
            stop("values in variable `", v, "` must only contain letters (lower or upper case), ",
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
#' @seealso annex_variable_definition, annex_room_definition, annex_country_definition
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
        stop(red $ bold(sprintf("Found illegal variable name%s in 'STAT'.\n", ifelse(length(idx) == 1, "", "s")),
             "  Not allowed: ", paste(sprintf("'%s'", x[idx]), collapse = ", "), "\n",
             get_row_info(idx), "\n\n",
             "  Allowed are: ",
             paste(sprintf("'%s'", allowed_variables), collapse = ", "), ".", sep = ""))

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
#' names, or fails. Not case sensitive for checking, but
#' will return everything in uppercase (\code{toupper(x)}).
#'
#' @seealso annex_variable_definition, annex_room_definition, annex_country_definition
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
    idx <- which(!is.na(x) & !grepl(pattern, x, ignore.case = TRUE))
    if (length(idx) > 0)
        stop(red $ bold(sprintf("Found illegal room name%s in 'STAT'.\n", ifelse(length(idx) == 1, "", "s")),
             "  Not allowed: ", paste(sprintf("'%s'", x[idx]), collapse = ", "), "\n",
             get_row_info(idx), "\n\n",
             "  Allowed are: ",
             paste(sprintf("'%s'", allowed_rooms), collapse = ", "), "\n",
             "  followed by up to two digits",
             sprintf("(e.g., '%1$s', '%1$s1', '%1$s2' or '%1$s35')", allowed_rooms[[1]]), sep = ""))


    # Return
    return(toupper(x))
}



#' Variable definition information
#'
#' The template not only contains the definition of the allowed variables,
#' it also states whether or not additional information is required (or
#' optional), an upper and lower bound to be considered 'valid' plus
#' (is specified) a series of allowed units.
#' Used to prepare the data and convert to annex standard units, quality checks,
#' as well as validation.
#'
#' @param as_list logical. If \code{FALSE} (default) a \code{data.frame}
#'        will be returned, if \code{TRUE} a list (see Details).
#'
#' @details If \code{as_list = TRUE} a list of lists is returned, else
#' a \code{data.frame}.
#'
#' **List:** The name of the list corresponds to the name of the variable,
#' whereas each entry contains a list with a logical flag if additional
#' information in the META sheet is \code{required} as well as a numeric
#' \code{lower} and \code{upper} bound which defines in which range
#' an observation is considered to be valid. Can be \code{NA} if not
#' specified (both or one of them). \code{allowed_units} contains \code{NA}
#' (unspecified) or a character wich one or multiple comma separated units
#' specifications.
#'
#' If \code{as_list = FALSE} (default) the same information is returned
#' as a \code{data.frame} containing the same information.
#'
#' @return Returns either a \code{data.frame} or \code{list} of \code{lists}
#' which contains the allowed (defined) variables.
#'
#' @seealso annex_variable_definition, annex_room_definition, annex_country_definition
#'
#' @author Reto Stauffer
#' @export
annex_variable_definition <- function(as_list = FALSE) {
    as_list <- as.logical(as_list)[1L]
    # Path to XLSX file to be read
    template_xlsx <- system.file("template/template.xlsx", package = "annex", mustWork = TRUE)
    tmp <- suppressMessages(read.xlsx(template_xlsx, sheet = "Definitions", sep.names = " "))
    required <- c("Variable" = "name",
                  "Additional information required" = "required",
                  "Lower bound" = "lower",
                  "Upper bound" = "upper",
                  "Allowed units" = "allowed_units")
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


#' Room definition information
#'
#' The template contains a series of base abbrevations allowed to define
#' a room alongside the 'long name'. This function returns the definition
#' as a \code{data.frame}.
#'
#' @return \code{data.frame} with base room abbrevation, long description,
#' plus a series of examples of valid room labels.
#'
#' @seealso annex_variable_definition, annex_room_definition, annex_country_definition
#'
#' @author Reto Stauffer
#' @export
annex_room_definition <- function() {
    # Path to XLSX file to be read
    template_xlsx <- system.file("template/template.xlsx", package = "annex", mustWork = TRUE)
    tmp <- suppressMessages(read.xlsx(template_xlsx, sheet = "Definitions", sep.names = " "))
    required <- c("Measurement location"             = "name",
                  "Measurement location description" = "long_name")
    stopifnot(names(required) %in% names(tmp))

    # Extracting information needed
    res <- subset(tmp, select = names(required))
    names(res) <- unname(required)
    res <- subset(res, !is.na(name))

    # Adding a few examples
    fn <- function(x) paste(c(x, sprintf("%s%d", x, c(1, 2, 25))), collapse = ", ")
    res$examples_valid_labels <- sapply(res$name, fn)

    return(res)
}

