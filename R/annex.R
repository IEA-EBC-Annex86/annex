

#' Annex Creator Function
#'
#' Creates an object of class `c("annex", "data.frame")` required to calculate
#' the statistics.
#'
#' @param formula the formula to specify how the data set is set up.
#'        See 'Details' for more information.
#' @param data \code{data.frame} containing the obervations/data.
#' @param tz character, time zone definition (e.g., \code{"Europe/Berlin"} or
#'        \code{"UTC"}); required.  \code{OlsonNames()} returns a list of possible
#'        time zones. The correct time zone is important to properly calculate month and time of day.
#' @param duplicate.action \code{NULL} or a function which returns
#'        a single numeric value. Used to handle possible duplicates, see
#'        'Details'.
#' @param meta \code{NULL} (default) or a \code{list} with information
#'        about study, home, and room (see section 'Duplicates').
#' @param verbose logical, defaults to \code{FALSE}. Can be set
#'        to \code{TRUE} to increase verbosity.
#'
#' @details
#' In case the data set provided on \code{data} does only contain data
#' of one study, home, and room, the fomula has two parts, looking
#' e.g., as follows:
#'
#' \itemize{
#'    \item \code{T + RH ~ datetime}
#' }
#'
#' The left hand side of the formula (left of \code{~}) specifies
#' the names of the variables of the observations to be processed,
#' the right hand side is the name of the variable containing the
#' time information (must be of class \code{POSIXt}). In this case,
#' the \code{meta} argument is required to provide information about
#' the study, home, and room.
#'
#' If the grouping information is already in the data set,
#' the analysis can be performed depending on the group information,
#' typically:
#' 
#' \itemize{
#'    \item \code{T + RH ~ datetime | study + home + room}
#' }
#'
#' The latter allows to process observations from different studies, homes, and/or rooms
#' all in one go.
#'
#' @section Duplicates:
#'
#' Duplicated records can distort the statistics and should be handled properly.
#' For each unique \code{study, home, room} only one observation (row) for a specific
#' date and time should exist.
#'
#' As there is no general way to deal with such duplicates, the function \code{annex}
#' (as well as \code{annex_prepare}) by default throws a warning for the user if such
#' duplicates exist (\code{duplicate.action = NULL}; default argument).
#'
#' However, the package allows the user to provide a custom \code{duplicate.action}
#' function, e.g., \code{mean}, \code{min}, \code{max}, ... This function must return
#' one single numeric value (or an NA) when applied to a vector. If a function is provided,
#' the \code{annex} function does the following:
#'
#' * Checks if there are any duplicates. If not, no changes are made. Else ...
#' * Checking if the function is valid (returns single numeric or NA). If not, 
#'   an error will be thrown.
#' * Takes the measurements of each duplicate; if all values are missing,
#'   an `NA` will be returned. Else the users \code{duplicate.action} is applied
#'   to all remaining non-missing values. I.e., if \code{duplicate.action = mean}
#'   the average of all non-missing values will be used.
#'
#' @examples
#' # Create artificial data set for testing; typically this information is read
#' # from a file or any other data connection.
#' data <- data.frame(datetime = as.POSIXct("2022-01-01 00:00", tz = "Europe/Berlin") + -10:10 * 3600,
#'                    T  = round(rnorm(21, mean = 20, sd = 2), 2),
#'                    RH = round(runif(21, 40, 100), 2))
#' 
#' res1 <- annex(T + RH ~ datetime, data = data,
#'               meta = list(study = "example", home = "ex", room = "BED"),
#'               tz = "Europe/Berlin")
#' head(res1, n = 3)
#' 
#' # The meta information can also be added to `data` removing the need
#' # to specify the `meta` argument and allows to mix data from different
#' # studies and rooms. Appending study, room, and home to `data`:
#' data <- transform(data,
#'                   study = "example",
#'                   home  = "ex",
#'                   room  = "BED")
#' head(data)
#' res2 <- annex(T + RH ~ datetime | study + home + room,
#'               data = data, tz = "Europe/Berlin")
#' head(res2, n = 3)
#'
#' @seealso annex_prepare, annex_stats
#' @importFrom Formula is.Formula as.Formula
#' @author Reto Stauffer
#' @export
annex <- function(formula, data, tz, duplicate.action = NULL, meta = NULL, verbose = FALSE) {
    if (!is.Formula(formula)) formula <- as.Formula(formula)
    if (inherits(data, "tbl_df")) data <- as.data.frame(data)
    stopifnot(is.character(tz), length(tz) == 1L, nchar(tz) > 0)
    stopifnot(is.data.frame(data), isTRUE(verbose) | isFALSE(verbose))
    stopifnot(is.null(duplicate.action) || is.function(duplicate.action))

    # -------------------------------------------------
    # Parsing formula
    f <- annex_parse_formula(formula, verbose = verbose)

    # If group is empty, 'meta' needs to contain
    # an element 'study', 'room', and 'home'
    if (is.null(f$group)) {
        if (!is.list(meta))
            stop("if grouping is not given in formula, 'meta' must be specified (see ?annex 'Details')")
        f$group <- c("study", "room", "home")
        if (any("ID" %in% names(data)))
            warning("variables 'ID' will get overwritten (see ?annex Details)!")
        for (n in f$group) {
            if (!n %in% names(meta)) stop("missing '", n, "' in 'meta'")
            if (length(meta[[n]]) != 1) stop("'meta$", n, "' must be of length 1")
            data[[n]] <- rep(meta[[n]], nrow(data))
        }
    }
    formula <- as.Formula(sprintf("%s ~ %s | %s", paste(f$vars, collapse = " + "),
                                  f$time, paste(f$group, collapse = " + ")))
    # Now we are sure all variables are in data

    # -------------------------------------------------
    # All variables available in the data frame?
    if (!all(f$vars %in% names(data)))
        stop("not all ", paste(f$vars, collapse = ", "), " in object 'data'")
    if (!is.null(f$group) && !all(f$group %in% names(data)))
        stop("not all ", paste(f$group, collapse = ", "), " in object 'data'")

    # Date information
    if (!length(f$time) == 1) {
        stop("invalid argument for the date/time information provided in formula")
    } else if (!f$time %in% names(data)) {
        stop("'", f$time, "' varialbe not found in object 'data'")
    } else if (any(is.na(data[[f$time]]))) {
        stop("`data$datetime` contains missing values")
    }

    # Data check: f$time must be POSIXt
    if (!inherits(data[[f$time]], "POSIXt"))
        stop("variable '", f$time, "' must be of class POSIXt, not ", class(data[[f$time]]), "\n")

    # variables must be numeric
    for (n in f$vars) {
        if (!is.numeric(data[[n]])) stop("variable '", n, "' must be numeric")
    }

    # Remove all columns from 'data' not needed
    data <- data[, c(f$group, f$time, f$vars)]

    # -------------------------------------------------
    # Splitting the data; checking for duplicates
    data <- annex_handle_duplicates(data, formula, duplicate.action, verbose)

    # -------------------------------------------------
    # Appending month and tod
    tmp  <- annex_add_year_month_and_tod(data[[f$time]], tz = tz)
    data <- cbind(data, as.data.frame(tmp))

    # Reordering the data
    data <- subset(data, select = c(f$time, f$group, "year", "month", "tod", f$vars))

    # -------------------------------------------------
    # Now the object should be ready
    # Appending class, formula information, return
    class(data) <- c("annex", class(data))
    attr(data, "formula") <- formula
    return(data)
}


#' Calculate year, month and time of day
#'
#' Calculates year, month and the time of day categories based
#' on input argument \code{x} with respect to the time zone
#' specified by the user.
#'
#' @param x object of class \code{POSIXt}.
#' @param tz time zone (character). Important to properly calculate
#'        month and time of day.
#'
#' @return List with three elements \code{year} (integer), \code{month} (factor) and
#' \code{tod} (factor).
#'
#' @examples
#' x <- as.POSIXct("2022-01-01", tz = "UTC") + 0:10 * 3600
#' annex:::annex_add_year_month_and_tod(x, tz = "Europe/Berlin")
#' annex:::annex_add_year_month_and_tod(x, tz = "US/Central")
#'
#' @author Reto Stauffer
annex_add_year_month_and_tod <- function(x, tz) {
    stopifnot(inherits(x, "POSIXt"))
    stopifnot(is.character(tz), length(tz) == 1L)
    #categorize time to specific season and time of day (tod)
    month  <- factor(as.integer(format(x, "%m", tz = tz)), 0:12, c("all", 1:12))
    year   <- as.factor(as.integer(format(x, "%Y", tz = tz)))
    tod    <- cut(as.integer(format(x, "%H", tz = tz)),
                  breaks = c(-Inf, 6, 22, Inf),
                  labels = c("23-07", "07-23", "23-07"))
    return(list(year = year, month = month, tod = tod))
}

#' Parsing Formula
#'
#' Function used to test and parse a formula used
#' in different functions in the annex package.
#'
#' @param f object of class \code{Formula}.
#' @param verbose logical, defaults to \code{FALSE}. Can be set
#'        to \code{TRUE} to increase verbosity.
#'
#' @return Returns a list with three components, namely `vars` (the variables to aggregate),
#' `time` (name of the datetime variable) and `group` (grouping variables).
#'
#' @examples
#' require("Formula")
#' annex:::annex_parse_formula(Formula(T + RH ~ datetime | study + room + home))
#'
#' @seealso annex
#' @importFrom Formula is.Formula Formula
#'
#' @author Reto Stauffer
annex_parse_formula <- function(f, verbose = FALSE) {
    stopifnot(is.Formula(f))
    stopifnot(length(length(f)) == 2, length(f)[2] %in% 1:2)

    # Parsing the formula
    vars  <- sapply(attr(terms(f, rhs = FALSE), "variable"), format)[-1]
    time  <- attr(terms(f, lhs = FALSE, rhs = 1),     "term.labels")
    group <- if (length(f)[2] == 2) attr(terms(f, lhs = FALSE, rhs = 2), "term.labels") else NULL

    # Verbose mode
    if (verbose) {
        cat("Formula in use:\n")
        cat("    Variables:     ", paste(vars, collapse = ", "), "\n")
        cat("    Time info on:  ", time, "\n")
        cat("    Grouping:      ", if (is.null(group)) "None" else paste(group, collapse = ", "), "\n")
    }

    return(list(vars = vars, time = time, group = group))
}


#' Check Regularity of an Annex Series
#'
#' \code{is.regular} is a regular function for checking whether a series
#' of observations has an underlying regularity or is even
#' strictly regular. Evaulate for each group of an annex object.
#'
#' @param x object of class \code{annex}.
#' @param strict logical, defaults to \code{TRUE}. If \code{FALSE},
#'        regularity (but not strict regularity) will be checked.
#' @param \dots currently unused.
#'
#' @return Returns a named logical vector where the name is
#' a combination of the grouping (study, home, room), the
#' content the result of checking regularity.
#'
#' @method is.regular annex
#' @importFrom zoo zoo is.regular
#' @author Reto stauffer
#' @export
is.regular.annex <- function(x, strict = TRUE, ...) {
    f <- annex_parse_formula(attr(x, "formula"))
    # Splitting data set
    tmp <- split(x, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    res <- logical(0)
    for (k in tmp) {
        name <- paste(as.vector(k[1, f$group]), collapse = " - ")
        res[name] <- is.regular(zoo(NA, k[[f$time]]), strict = strict)
    }
    return(res)
}



#' Annex Summary
#'
#' Numeric summary of an annex object.
#'
#' @param object an object of class \code{annex}.
#' @param type character, one of \code{"default"} (default) or \code{"statistic"}.
#'        If \code{type = "statistics"} the result of \code{annex_stats(object)} will be printed.
#' @param \dots currently unused.
#'
#' @return Returns \code{NULL} (invisible).
#'
#' @method summary annex
#' @author Reto stauffer
#' @export
summary.annex <- function(object, type = "default", ...) {
    type <- match.arg(type, c("statistics", "default"))
    stopifnot("formula" %in% names(attributes(object)))
    if (type == "default") {
        f <- annex_parse_formula(attr(object, "formula"))
        # Splitting data set
        tmp <- split(object, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
        for (k in tmp) {
            cat(paste(f$group, as.vector(k[1, f$group]), sep = " = ", collapse = ", "), "\n")
            cat("Number of observations: ", nrow(k), "\n")
            k <- structure(k[, !names(k) %in% f$group], class = class(k)[2])
            print(summary(k))
            cat("\n")
        }
    } else {
        print(annex_stats(object, ...))
    }
    invisible(NULL)
}


# --------------------------------------------------
# S3 classes to keep class and formula attribute

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @importFrom utils head
#' @author Reto Stauffer
#' @rdname annex
#' @export
head.annex <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @importFrom utils tail
#' @author Reto Stauffer
#' @rdname annex
#' @export
tail.annex <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @author Reto Stauffer
#' @rdname annex
#' @export
subset.annex <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))











#' Internal Function for Handling Possible Duplicates
#'
#' This function is called inside `annex()`.
#'
#' @param x data.frame to be processed.
#' @param formula object of class `Formula`, the formula provided to `annex()`.
#' @param duplicate.action can be `NULL` (no dedicated handling for duplicates)
#'        of a function. If function it must return a single numeric value,
#'        will be tested to be able to provide a useful error for the user if needed.
#' @param verbose logical, verbosity (defaults to `FALSE`).
#'
#' @return Returns a `data.frame` similar to argument `x` with possibly
#' modified content (depending on how to deal with duplicates if any).
#'
#' @importFrom Formula is.Formula
#
#' @author Reto
annex_handle_duplicates <- function(x, formula, duplicate.action, verbose = FALSE) {
    stopifnot(is.data.frame(x))
    stopifnot(is.null(duplicate.action) || is.function(duplicate.action))
    stopifnot(is.Formula(formula))
    stopifnot(isTRUE(verbose) || isFALSE(verbose))

    # Deparsing the formula
    f <- annex_parse_formula(formula)
    stopifnot(all(unlist(f) %in% names(x)))

    # Splitting data
    x <- split(x, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)

    # In any case we are first checking if there are any duplicates
    dups <- vector("list", length(x))
    for (i in seq_along(x)) {
        if (anyDuplicated(x[[i]][[f$time]])) {
            t <- x[[i]][1, f$group]
            dups[i] <- paste0("variable '", f$time, "' contains duplicated time stamps for ",
                             paste(names(t), as.vector(t), sep = " = ", collapse = ", "))
        }
    }

    # If length(dups) == 0 we have no duplicates and can return x as is. Else proceed
    if (length(dups) > 0) {
        # If duplicate.action is NULL we throw a series of errors for the user,
        # but leave the data set `x` untouched.
        if (is.null(duplicate.action)) {
            for (d in unlist(dups)) warning(d[[1]])
        # Else we need to check the function, and then apply the function to deal
        # with the duplicates we've found.
        } else {
            # Make sure the user provided a function which returns a single
            # numeric (or NA) needed for handling the data.
            test_data <- list(c(1), c(NA), c(NA, NA, NA), c(NA, 1, 2, 3), c(1, 2, 3))
            for (td in test_data) {
                res <- duplicate.action(td)
                if (!length(res) == 1 || (!is.na(res) && !is.numeric(res))) {
                    stop("the function provided on `duplicate.action` does not return a single numeric (or NA) ",
                         "when calling `fun(", deparse(td), ")`. Please adjust.")
                }
            }
            # We now assume the function does what we need.
            dup_formula <- formula(sprintf("cbind(%s) ~ %s", paste(f$vars, collapse = ", "),
                                   paste(c(f$time, f$group), collapse = " + ")))

            # The function we use (removes missing values; then applies the user function)
            fn <- function(x, fun) {
                x <- na.omit(x)
                if (length(x) == 0) NA_real_ else fun(x)
            }

            # Loop over the blocks and call `aggregate()` for those where duplicates exist
            # If is.null(dups[[i]]) there are no duplicates, skip.
            for (i in seq_along(x)) {
                if (!is.null(dups[[i]])) {
                    x[[i]] <- aggregate(dup_formula, data = x[[i]], FUN = fn, fun = duplicate.action, na.action = na.pass)
                }
            }
        }
    }

    # Re-combine and return
    x <- do.call(rbind, x)
    rownames(x) <- NULL
    return(x)
}
