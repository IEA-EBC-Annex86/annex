

#' Checking Annex config object
#'
#' An 'annex config object' is a simple data.frame which
#' can be created using Rs standard features. The function
#' is checking if the content of the object matches the
#' requirements of being an annex config object.
#' This check will be performed in \code{link{annex()}} automatically,
#' but can also be done by the user manually.
#'
#' @return Invisibly returns a (possibly modified) version of \code{x}
#' containing only the required columns in a specific order.
#'
#' @details The function checks if the config object is set up properly
#' and contains the required information for preparing the annex data.
#'
#' Fails if:
#' \itemize{
#'      \item the input is not a \code{data.frame}
#'      \item variables are missing ('column', 'variable', 'study', 'home', 'room')
#'      \item configuration for \code{variable = "datetime"} is missing
#'      \item there is no definition for variables (datetime only)
#'      \item the config contains missing values in the required variables
#'      \item column name for \code{variable = "datetime"} is missing
#'      \item the variable 'column' is not unique
#'      \item there are duplicated entries for the combination of
#'            variable/study/home/room (must be unique)
#'      \item variables study, home, or room contain the character \code{:} (not allowed)
#'}
#'
#' @author Reto Stauffer
#' @export
check_config <- function(x) {
    stopifnot(is.data.frame(x))
    stopifnot(NROW(x) > 0)

    # Required columns
    required_cols <- c("column", "variable", "study", "home", "room")
    if (!all(required_cols %in% names(x)))
        stop("not all required columns (", paste(required_cols, collapse = ", "), ") ",
             "exist in the 'config object'. Missing: ",
             paste(required_cols[!required_cols %in% names(x)], collapse = ", "))

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
    for (v in c("study", "home", "room")) {
        if (any(grepl("\\:", x[, v]))) stop("colons (':') not allowed in variable ", v)
    }

    # Invisible return reduced to the required columns
    invisible(x[, required_cols])
}
