

#' Annex Creator
#'
#' Creates an object of class `c("annex", "data.frame")`.
#'
#' @param formula the formula to specify how the data set is set up.
#'        see 'Details' for more information.
#' @param data \code{data.frame} containing the obervations/data.
#' @param meta \code{NULL} (default) or a \code{list} with information
#'        about study, home, and room (see 'Details').
#' @param verbose logical, defaults to \code{FALSE}. Can be set
#'        to \code{TRUE} to increase verbosity.
#'
#' @details
#' In case the data set provided on \code{x} does only contain data
#' of one study, home, and room, the fomula has two parts, looking
#' e.g., as follows:
#'
#' \begin{itemize}
#'    \item \code{co2 + voc ~ datetime}
#' \end{itemize}
#'
#' The left hand side of the formula (left of \code{~}) specifies
#' the names of the variables of the observations to be processed,
#' the right hand side is the name of the variable containing the
#' time information (must be of class \code{POSIXt}). In this case,
#' the \code{meta} argument is required to provide information about
#' the study, home, and room.
#'
#' TODO(R): I would like to implement the following incase
#' the information is in the data frame \code{x}, this would allow
#' to process multiple at once.
#' 
#' \begin{itemize}
#'    \item \code{co2 + voc ~ datetime | study + home + room}
#' \end{itemize}
#'
#' @importFrom Formula is.Formula as.Formula
#' @author Reto Stauffer
#' @export
annex <- function(formula, data, meta = NULL, verbose = FALSE) {
    if (!is.Formula(formula)) formula <- as.Formula(formula)
    stopifnot(is.data.frame(data), isTRUE(verbose) | isFALSE(verbose))

    # -------------------------------------------------
    # Parsing formula
    f <- annex_parse_formula(formula, verbose = verbose)

    # If group is empty, 'meta' needs to contain
    # an element 'study', 'room', and 'home'
    if (is.null(f$group)) {
        if (!is.list(meta))
            stop("if grouping is not given in formula, 'meta' must be specified (see ?annex 'Details')")
        f$group <- c("study", "room", "home")
        if (any(f$group %in% names(data)))
            warning("variables ", paste(f$group, collapse = ", "), " will get overwritten (see ?annex Details)!")
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
    }

    # Data check: f$time must be POSIXt
    if (!inherits(data[[f$time]], "POSIXt"))
        stop("variable '", f$time, "' must be of class POSIXt, not ", class(data[[f$time]]), "\n")

    # variables must be numeric
    for (n in f$vars) {
        if (!is.numeric(data[[n]])) stop("variable '", n, "' must be numeric")
    }

    # -------------------------------------------------
    # Checking for duplicated time stamps
    # Splitting the data; checking for duplicates
    tmp <- split(data, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    for (t in tmp) {
        if (anyDuplicated(t[[f$time]])) {
            t <- t[1, f$group]
            warning("variable '", f$time, "' contains duplicated time stamps ",
                    paste(names(t), as.vector(t), sep = " = ", collapse = ", "))
        }
    }

    # -------------------------------------------------
    # Appending season and tod
    tmp <- annex_add_season_and_tod(data[[f$time]])
    data <- cbind(data, as.data.frame(tmp))

    # Reordering the data
    data <- subset(data, select = c(f$time, f$group, "season", "tod", f$vars))

    # -------------------------------------------------
    # Now the object should be ready
    # Appending class, formula information, return
    class(data) <- c("annex", class(data))
    attr(data, "formula") <- formula
    return(data)
}


#' Calculate season and time of day
#'
#' Calculates season and the time of day categories based
#' on input argument \code{x}.
#'
#' @param x object of class \code{POSIXt}.
#'
#' @return List with two elements \code{season} (factor) and \code{tod} (factor).
#'
#' @author Reto Stauffer
annex_add_season_and_tod <- function(x) {
    stopifnot(inherits(x, "POSIXt"))
    #categorize time to specific season and time of day (tod)
    season <- cut(as.integer(format(x, "%m%d")),
                  breaks = c(-Inf, 329, 620, 922, 1220, Inf),
                  labels = c("12-02", "03-05", "06-08", "09-11", "12-02"))

    tod    <- cut(as.integer(format(x, "%H")),
                  breaks = c(-Inf, 6, 22, Inf),
                  labels = c("23-07", "07-23", "23-07"))
    return(list(season = season, tod = tod))
}

#' Parsing Formula
#'
#' Function used to test and parse a formula used
#' in different functions in the Annex package.
#'
#' @param f object of class \code{Formula}.
#' @param verbose logical, defaults to \code{FALSE}. Can be set
#'        to \code{TRUE} to increase verbosity.
#'
#' @importFrom Formula is.Formula
#'
#' @author Reto Stauffer
annex_parse_formula <- function(f, verbose = FALSE) {
    stopifnot(is.Formula(f))
    stopifnot(length(length(f)) == 2, length(f)[2] %in% 1:2)

    # Parsing the formula
    vars  <- attr(terms(f, lhs = TRUE,  rhs = FALSE), "term.labels")
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


#' Check Regularity of an Annex series
#'
#' \code{is.regular} is a regular function for checking whether a series
#' of observations has an underlying regularity or is even
#' strictly regular. Evaulate for each group of an Annex object.
#'
#' @param x object of class \code{annex}.
#' @param strict logical, defaults to \code{TRUE}. If \code{FALSE},
#'        regularity (but not strict regularity) will be checked.
#'
#'
#' @return Returns a named logical vector where the name is
#' a combination of the grouping (study, home, room), the
#' content the result of checking regularity.
#'
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



#' Annex summary
#'
#' Numeric summary of an annex object.
#'
#' @param object an object of class \code{annex}.
#' @param \dots currently unused.
#'
#' @return Returns \code{NULL} (invisible).
#'
#' @author Reto stauffer
#' @export
summary.annex <- function(object, ...) {
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
    invisible(NULL)
}







