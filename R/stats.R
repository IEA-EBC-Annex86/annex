
#' Calculate Statistics on Annex object
#'
#' @param object an object of class \code{annex}.
#' @param format character, either \code{"wide"} (default) or \code{"long"}.
#' @param \dots currently unused.
#' @param probs \code{NULL} (default; see Details) or a numeric vector of probabilities 
#'        with values in \code{[0,1]} (Values will be rounded to closest 3 digits).
#'
#' @details
#' The function allows to return the statistics in a wide format or long format.
#' Both can be used when calling [annex::annex_write_stats()], but he long/wide
#' format can be handy fur custom applications (e.g., plotting, ...).
#'
#' Argument \code{probs} will be forwarded to the [stats::quantile()] function.
#' If \code{probs = NULL} (default) the empirical quantiles will be calculated
#' from \code{0} (the minimum) up to \code{1} (the maximum) in an interval of
#' \code{0.05} (five percent steps), including quantiles \code{0.005},
#' \code{0.025}, \code{0.975} and \code{0.995}. Can be specified differently
#' by the user if needed, however, this no longer yields the standard statistics
#' and the validation will report a problem.
#'
#' TODO(R): Include this check in the STAT validation function.
#'
#' @return Returns an object of class \code{c("annex_stats", "data_frame")}.
#'
#' @seealso annex_stats_reshape annes_write_stats
#'
#' @importFrom dplyr bind_rows
#' @import stats
#' @author Reto Stauffer
#' @export
annex_stats <- function(object, format = "wide", ..., probs = NULL) {
    stopifnot(inherits(object, "annex"))
    format <- match.arg(format, c("wide", "long"))
    f <- annex_parse_formula(attr(object, "formula"))

    # probs is used to get the quantiles. By default probs will
    # be seq(0, 1, by = 0.05) including (0.005, 0.025 and 0.975, 0.995)
    # for the 95 and 99 percent width/interval.
    stopifnot(is.null(probs) || is.numeric(probs))
    if (is.numeric(probs)) {
        if (!all(probs >= 0 & probs <= 1) && !any(is.na(probs)))
            stop("argument `probs` must be numeric [0, 1] without missing values")
        probs <- unique(sort(round(probs, 3)))
    } else {
        probs <- sort(c(c(0.005, 0.995, 0.025, 0.975), seq(0, 1, by = 0.05)))
    }

    # Functions to apply to calculate the stats
    # Note that `probs` is scoped!
    get_quantiles <- function(x, digits = 4) {
        tmp   <- abs(round(probs, digits = 2) - probs) < sqrt(.Machine$double.eps)
        names <- ifelse(tmp, sprintf("p%02.0f", 100 * probs), sprintf("p%04.1f", 100 * probs))
        return(round(setNames(quantile(x, probs = probs, na.rm = TRUE), names), digits = digits))
    }

    # Helper function to caluclate the shape
    shape <- function(x, na.rm = TRUE) {
        mean(x, na.rm = na.rm)^2 * ((1 - mean(x, na.rm = na.rm)) / var(x, na.rm = na.rm) - 1 / mean(x, na.rm = na.rm))
    }

    # Reshaping result of aggregate()
    convert <- function(var, data, f, gx = c("season", "tod")) {
        res <- as.data.frame(data[, var])
        res <- cbind(transform(data[, c(f$group, gx)], variable = var), res)
        return(res)
    }

    # List of functions to be applied; must all return a named vector
    functionlist <- list(
        function(x) c(Mean   = mean(x, na.rm = TRUE)),
        function(x) c(Sd     = sd(x, na.rm = TRUE)),
        function(x) c(N      = length(x), NAs = sum(is.na(x))),
        function(x) get_quantiles(x)
        )

    # Applies all the functions of the functionlist to an input x
    # Warning: uses scoping (functionlist)
    get_stats <- function(x) do.call(c, lapply(functionlist, function(FUN, x) FUN(x), x = x))

    # Aggregate the data
    object_split  <- split(object, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    fn <- function(x, f, gx = c("season", "tod")) {
        # In case everything is NA; return NULL straight away
        if (sum(!is.na(x[, f$vars])) == 0) return(NULL)
        # Drop columns without non-missing values
        check_na <- sapply(x[, f$vars], function(x) sum(!is.na(x)))
        #use_var  <- f$var[f$var %in% names(check_na)[check_na > 0]]
        use_var <- f$var
        af <- sprintf("cbind(%s) ~ %s", paste(use_var, collapse = ", "),
                      paste(c(f$group, gx), collapse = " + "))
        # Else perform the aggregation
        x <- aggregate(formula(af), x, get_stats, na.action = na.pass)
        return(lapply(use_var, convert, data = x, f = f, gx = gx))
    }

    # Splitting data; aggregate data
    res <- unlist(lapply(object_split, fn, f = f), recursive = FALSE)
    res <- if (length(res) == 1) res[[1]] else bind_rows(res)

    # Same but grouping only by study|home|room|season (all day long)
    res_all_day    <- fn(object, f, c("season"))
    res_all_day    <- transform(do.call(rbind, res_all_day), tod  = "all")
    # Same but grouping only by study|home|room|tod (all season long)
    res_all_season <- fn(object, f, c("tod"))
    res_all_season <- transform(do.call(rbind, res_all_season), season = "all")

    res <- bind_rows(list(res_all_day, res_all_season, res))
    for (n in c("study", "home", "room", "season", "tod", "variable")) {
        res[[n]] <- factor(res[[n]])
    }

    # Sort columns
    first <- c("study", "home", "room", "season", "tod", "variable")
    res <- res[, c(first[first %in% names(res)], names(res)[!names(res) %in% first])]

    # Structuring return; by default 'wide' format
    res <- structure(res, row.names = seq_len(NROW(res)),
                     class = c("annex_stats", "annex_stats_wide", class(res)),
                     formula = attr(object, "formula"))

    # Reshape to long format if required
    if (format == "long") res <- annex_stats_reshape(res)
    return(res)
}

#' Reshaping Annex Stats Objects
#'
#' @param x object of class \code{annex_stats} as returned
#'        by [annex::annex_stats()].
#' @param format \code{NULL} by default or one of \code{"long"}
#'        or \code{"wide"} (see Details).
#'
#' @return Returns a reshaped version of the input. If the
#' Object provided on `x` inherits \code{annex_stats_wide} (wide format)
#' the long format will be returned and vice versa if \code{format = NULL}.
#' If the format is specified as either \code{"long"} or \code{"wide"}
#' the long or wide format will be returned (possibly an unmodified version
#' of the input if the input is already in the desired format).
#'
#' @seealso annex_stats
#' @importFrom tidyr pivot_longer pivot_wider
#' @author Reto Stauffer
#' @export
annex_stats_reshape <- function(x, format = NULL) {
    stopifnot(inherits(x, "annex_stats"))
    stopifnot(is.null(format) || (is.character(format) && length(format) == 1L))
    if (!is.null(format)) format <- match.arg(format, c("long", "wide"))

    is_long <- inherits(x, "annex_stats_long")
    formula <- attr(x, "formula")           # we need it later
    f       <- annex_parse_formula(formula) # deparse
    # Identify grouping variable (plus season, tod, and variable if existing)
    idvar   <- c("season", "tod", "variable")
    idvar   <- c(f$group, idvar[idvar %in% names(x)])

    # Reshape to wide format
    if (is_long && (is.null(format) || format == "wide")) {
        x        <- pivot_wider(x, names_from = "stats", values_from = "value")
        x        <- structure(x, class = c("annex_stats", "annex_stats_wide", "data.frame"),
                              formula = formula)
    # Reshape to long format
    } else if (!is_long && (is.null(format) || format == "long")) {
        varying  <- names(x)[!names(x) %in% idvar]
        x        <- pivot_longer(x, cols = varying, names_to = "stats")
        x        <- structure(x, class = c("annex_stats", "annex_stats_long", "data.frame"),
                              formula = formula)
    }
    return(x)
}

