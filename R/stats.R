
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

    # Adding quality flag
    add_quality_flag <- function(x, variables) {
        vinfo <- annex_variable_definition(as_list = TRUE)
        for (v in variables) {
            info <- vinfo[[v]]
            if (is.null(tmp)) stop(sprintf("got an invalid variable `%s`", v))
            # New variable name: quality_<varaible_name> for "quality flag"
            nv <- sprintf("quality_%s", v)
            # Else checking range
            if (!is.na(info$lower) && !is.na(info$upper)) {
                x[[nv]] <- x[[v]] >= info$lower & x[[v]] <= info$upper
            } else if (!is.na(info$lower)) {
                x[[nv]] <- x[[v]] >= info$lower
            } else if (!is.na(info$upper)) {
                x[[nv]] <- x[[v]] <= info$upper
            } else {
                x[[nv]] <- !is.na(x[[v]])
            }
        }
        return(x)
    }
    object <- add_quality_flag(object, f$var)

    # Adding interval since last observation; done on a variable level
    add_interval_since_last <- function(x, variables) {
        for (v in variables) {
            # New variable name: interval_<varaible_name> for "interval"
            nv <- sprintf("interval_%s", v)
            x[[nv]] <- NA_integer_
            idx <- which(!is.na(x[[v]]))
            x[idx, nv] <- as.integer(c(NA_integer_, as.numeric(diff(object$datetime[idx]), units = "secs")))
        }
        return(x)
    }
    object <- add_interval_since_last(object, f$var)

    # Reshaping result of aggregate()
    convert <- function(var, data, f, gx = c("season", "tod")) {
        res <- as.data.frame(data[, var])
        res <- cbind(transform(data[, c(f$group, gx)], variable = var), res)
        return(res)
    }

    # List of functions to be applied; must all return a named vector
    functionlist <- list(
        function(x) c(Mean   = round(mean(x, na.rm = TRUE), digits = 4)),
        function(x) c(Sd     = round(sd(x, na.rm = TRUE), digits = 4)),
        function(x) c(N      = length(x), NAs = sum(is.na(x))),
        function(x) get_quantiles(x)
        )

    # Applies all the functions of the functionlist to an input x
    # Warning: uses scoping (functionlist)
    get_stats <- function(x) do.call(c, lapply(functionlist, function(FUN, x) FUN(x), x = x))
    get_interval_stats <- function(x) {
        c("min" = min(x), "max" = max(x))
    }

    # Aggregate the data
    object_split  <- split(object, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    fn <- function(x, f, gx = c("season", "tod")) {
        # In case everything is NA; return NULL straight away
        if (sum(!is.na(x[, f$vars])) == 0) return(NULL)
        # Drop columns without non-missing values
        check_na <- sapply(x[, f$vars], function(x) sum(!is.na(x)))

        # Create formula to aggregate and perform aggregation for the observations
        # fd: formula/aggregation for 'data' (the observations themselves)
        fd <- sprintf("cbind(%s) ~ %s", paste(f$vars, collapse = ", "),
                      paste(c(f$group, gx), collapse = " + "))
        res1 <- aggregate(formula(fd), x, get_stats, na.action = na.pass)

        # fq: formula/aggregation for 'quality'
        fq <- sprintf("cbind(%s) ~ %s",
                      paste(sprintf("quality_%s", f$vars), collapse = ", "),
                      paste(c(f$group, gx), collapse = " + "))
        res2 <- aggregate(formula(fq), x, function(x) mean(x, na.rm = TRUE))

        # fi: formula/aggregation for 'interval'
        fi <- sprintf("cbind(%s) ~ %s",
                      paste(sprintf("interval_%s", f$vars), collapse = ", "),
                      paste(c(f$group, gx), collapse = " + "))
        res3 <- aggregate(formula(fi), x, get_interval_stats, na.action = na.pass)
        print(head(res3))
        stop('xyz')
        print(head(x))
        stop('x')

        return(lapply(f$vars, convert, data = x, f = f, gx = gx))
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

    ## Removing rows (wide format) where the number of values is the same
    ## as the number of missing values, i.e., no data at all.
    res <- subset(res, N > NAs)

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

# --------------------------------------------------
# S3 classes to keep class and formula attribute

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @importFrom utils head
#' @author Reto Stauffer
#' @rdname annex
#' @export
head.annex_stats <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @importFrom utils tail
#' @author Reto Stauffer
#' @rdname annex
#' @export
tail.annex_stats <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))

#' @param x object of class \code{annex}.
#' @param \dots arguments to be passed to or from other methods.
#' @author Reto Stauffer
#' @rdname annex
#' @export
subset.annex_stats <- function(x, ...)
    structure(NextMethod(), class = class(x), formula = attr(x, "formula"))
