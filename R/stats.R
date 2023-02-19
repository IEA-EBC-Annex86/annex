
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
#' \code{0.01} (one percent steps), including quantiles \code{0.005},
#' \code{0.025}, \code{0.975} and \code{0.995}. Can be specified differently
#' by the user if needed, however, this no longer yields the standard statistics
#' and the validation will report a problem.
#'
#' @section Statistics:
#'
#' **Grouping:** Statistics are calculated on different subsets (or groups),
#' typically \code{study}, \code{home}, \code{room}, \code{year}, \code{month},
#' \code{tod} (time of day).  However, this set can vary depending on the users
#' function call to \code{annex} (see argument \code{formula}).
#'
#' \code{annex_stats} calculates a series of data/quality flags as well as statistical
#' measures.
#'
#' **Quality:** \code{quality_lower} and \code{quality_upper} contain the fraction of
#' observations (in percent) falling below the lower and upper defined threshold
#' (see \code{annex_variable_definition}).
#' \code{quality_start} and \code{quality_end} contain the day (date only)
#' where the first non-missing observation was given for the current group; used to
#' estimate \code{Nestim} (see below).
#'
#' **Interval:** Time increments of all non-missing observations are calculated in seconds.
#' The \code{interval_} columns show the five digit summary plus the arithmetic mean of these
#' intervals. \code{interval_Median} is used to calculate estimate \code{Nestim} (see below).
#'
#' **Nestim:** Number of estimated observations (see section below)
#' **N:** Number of non-missing observations
#' **NAs:** Number of missing observations (\code{NA} in the data set)
#' **Mean:** \deqn{\bar{x} = \frac{1}{N} \sum_{i = 1}^N x_i}{x_bar = mean(x)} (arithmetic mean)
#' **Sd:** \deqn{\text{sd}(x) = \sqrt{\frac{1}{N - 1} \sum_{i = 1}^N \big( (x_i - \bar{x})^2\big)}}{sd(x)}
#' **p:** Probabilites for different quantiles. \code{p00} represents the overall minimum,
#' \code{p50} the median, \code{p100} the overall maximum of all non-missing values. Uses
#' the empirical quantile function with \code{type = 7} (default; see \code{quantile}).
#'
#' @section Estimated number of observations:
#'
#' TODO(R): Explain
#' 
#' @return Returns an object of class \code{c("annex_stats", "data_frame")}.
#'
#' @seealso annex_stats_reshape annes_write_stats
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @import stats
#' @author Reto Stauffer
#' @export
annex_stats <- function(object, format = "wide", ..., probs = NULL) {
    stopifnot(inherits(object, "annex"))
    format <- match.arg(format, c("wide", "long"))
    f <- annex_parse_formula(attr(object, "formula"))

    data_tz <- attr(object[[f$time]], "tz")

    # To be able to calculate intevals between measurements we need
    # to split the data according to f$group, sort them, calculate differences,
    # and combine the object once again.
    # Adding interval since last observation; done on a variable level
    add_interval_since_last <- function(x, f) {
        # Helper function
        fn <- function(x) {
            x <- x[order(x[[f$time]]), ]
            for (v in f$vars) {
                # New variable name: interval_<varaible_name> for "interval"
                nv <- sprintf("interval_%s", v)
                x[[nv]] <- NA_integer_
                idx <- which(!is.na(x[[v]]))
                x[idx, nv] <- as.integer(c(NA_integer_, as.numeric(diff(object$datetime[idx]), units = "secs")))
            }
            return(x)
        }
        fi <- formula(paste("~ ", paste(f$group, collapse = " + ")))
        x <- do.call(rbind, lapply(split(object, fi, drop = TRUE), fn))
        rownames(x) <- NULL
        return(x)
    }

    # probs is used to get the quantiles. By default probs will
    # be seq(0, 1, by = 0.05) including (0.005, 0.025 and 0.975, 0.995)
    # for the 95 and 99 percent width/interval.
    stopifnot(is.null(probs) || is.numeric(probs))
    if (is.numeric(probs)) {
        if (!all(probs >= 0 & probs <= 1) && !any(is.na(probs)))
            stop("argument `probs` must be numeric [0, 1] without missing values")
        probs <- unique(sort(round(probs, 3)))
    } else {
        probs <- sort(c(c(0.005, 0.995, 0.025, 0.975), seq(0, 1, by = 0.01)))
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
    add_quality_flags <- function(x, variables) {
        vinfo <- annex_variable_definition(as_list = TRUE)
        for (v in variables) {
            info <- vinfo[[v]]
            if (is.null(info)) stop(sprintf("got an invalid variable `%s`", v))
            # New variable name: quality_(lower|upper)_<varaible_name> for "quality flag"
            nvl <- sprintf("quality_lower_%s", v)
            x[[nvl]] <- if (is.na(info$lower)) NA else x[[v]] < info$lower
            nvu <- sprintf("quality_upper_%s", v)
            x[[nvu]] <- if (is.na(info$upper)) NA else x[[v]] > info$upper
        }
        return(x)
    }

    # Reshaping result of aggregate()
    convert <- function(var, data, f, gx = c("year", "month", "tod"), prefix = NULL) {
        res   <- as.data.frame(data[, if (is.null(prefix)) var else paste(prefix, var, sep = "_")])
        if (!is.null(prefix)) names(res) <- paste(prefix, names(res), sep = "_")
        res <- cbind(transform(data[, c(f$group, gx)], variable = var), res)
        return(res)
    }

    # Helper functon to convert the 'tod' labels into hours.
    # Input is expected to be converted to character and must
    # contain "^[0-9]+-[0-9]+$", e.g., "07-23" (7 to 23 o'clock)
    # or "23-07" (23 to 7 o'clock). Extracts this information
    # and returns it as a numeric value in hours. The two examples
    # would give 16 and 8.
    tod_to_hours <- function(x) {
        x <- as.character(x)
        stopifnot(grepl("^[0-9]+-[0-9]+$", x))
        tmp <- regmatches(x, gregexpr("[0-9]+", x))
        stopifnot(is.list(tmp), length(tmp) == length(x))
        stopifnot(all(sapply(tmp, length) == 2))
        fn <- function(u) {
            if (diff(u) < 0) u <- u - c(24, 0)
            round(diff(u / 12 * pi) / pi * 12, 2)
        }
        # Returns time difference in hours 
        res <- sapply(tmp, function(x) fn(as.numeric(x)))

    }

    # List of functions to be applied; must all return a named vector
    functionlist <- list(
        function(x) c(N      = length(x), NAs = sum(is.na(x))),
        function(x) c(Mean   = round(mean(x, na.rm = TRUE), digits = 4)),
        function(x) c(Sd     = round(sd(x, na.rm = TRUE), digits = 4)),
        function(x) get_quantiles(x)
        )

    # Applies all the functions of the functionlist to an input x
    # Warning: uses scoping (functionlist)
    get_stats <- function(x) do.call(c, lapply(functionlist, function(FUN, x) FUN(x), x = x))
    get_interval_stats <- function(x) {
        tmp <- unname(quantile(x, probs = 0:4/4, na.rm = TRUE))
        c(Min = tmp[1], Q1 = tmp[2], Median = tmp[3],
          Mean = mean(tmp, na.rm = TRUE), Q3 = tmp[4], Max = tmp[5])
    }

    # ---------------------------------------
    # Modify and aggregate the data
    # ---------------------------------------
    object <- add_interval_since_last(object, f)
    object <- add_quality_flags(object, f$var)

    # Splitting the data into the groups given by the user (e.g., ~ study + home + room)
    object_split  <- split(object, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)


    # This function gets part of the original data; already
    # split into groups. We further split the data according to the
    # 'gx' argument (temporal groups) to calculate a series of additional
    # variables such as quality flags.
    aggfun <- function(x, f, gx = c("year", "month", "tod")) {
        # In case everything is NA; return NULL straight away
        if (sum(!is.na(x[, f$vars])) == 0) return(NULL)

        # ---------------------------------------
        # Calculating statistics
        # ---------------------------------------
        # Create formula to aggregate and perform aggregation for the observations
        # fd: formula/aggregation for 'data' (the observations themselves)
        fstats <- sprintf("cbind(%s) ~ %s", paste(f$vars, collapse = ", "),
                          paste(c(f$group, gx), collapse = " + "))
        stats  <- aggregate(formula(fstats), x, get_stats, na.action = na.pass)
        stats  <- do.call(rbind, lapply(f$vars, convert, data = stats, f = f, gx = gx))

        # ---------------------------------------
        # Calculate quality information
        # ---------------------------------------
        qual <- list()
        for (n in c("lower", "upper")) {
            qcols <- sprintf(paste("quality", n, "%s", sep = "_"), f$var)
            fq    <- sprintf("cbind(%s) ~ %s", paste(qcols, collapse = ", "),
                             paste(c(f$group, gx), collapse = " + "))
            qual[[n]]  <- aggregate(formula(fq), x, function(x) 1e2 * round(mean(x, na.rm = TRUE), 4), na.action = na.pass)
            qual[[n]]  <- pivot_longer(qual[[n]], names_to = "variable",
                                  values_to = paste("quality", n, sep = "_"), cols = qcols)
            qual[[n]]$variable <- sub(sprintf("^quality_%s_", n), "", qual[[n]]$variable)
        }
        qual <- do.call("merge", unname(qual))

        # Calculate date of first observation within this group as well as last
        # observation in the group. Therefore, two new interactions are calculated
        # (tmp_ic based on 'x' and qual_ic based on 'qual'). These are temporary
        # vectors used for subsetting/index calculation only.
        tmp_ic  <- interaction(x[c(f$group, gx)], sep = "-")
        qual_ic <- interaction(qual[c(f$group, gx)], sep = "-")
        qual$quality_end <- qual$quality_start <- NA

        # For each interaction: extract the observations and datetime information from
        # the original object 'x'; drop missing values, and check range of the datetime
        # variable to get first/last observation within this block. Stores final result
        # into qual$quality_start and qual$quality_end.
        for (ic in unique(tmp_ic)) {
            for (v in f$vars) {
                minmax <- na.omit(subset(x, tmp_ic == ic, select = c(f$time, v)))
                if (NROW(minmax) == 0) next  # No non-missing values
                # Else extract date range
                date_range <- range(minmax[[f$time]])

                # Store the data
                idx <- which(qual_ic == ic & qual$variable == v)
                stopifnot(length(idx) == 1, !is.na(idx))
                qual$quality_start[idx] <- as.Date(min(date_range), tz = data_tz)
                qual$quality_end[idx]   <- as.Date(max(date_range), tz = data_tz)
            }
        }


        # ---------------------------------------
        # Calculating/guessing observation intervals/frequency
        # ---------------------------------------
        # fi: formula/aggregation for 'interval'
        finter <- sprintf("cbind(%s) ~ %s",
                      paste(sprintf("interval_%s", f$vars), collapse = ", "),
                      paste(c(f$group, gx), collapse = " + "))
        inter <- aggregate(formula(finter), x, get_interval_stats, na.action = na.pass)
        inter <- do.call(rbind, lapply(f$vars, convert, data = inter, f = f, gx = gx, prefix = "interval"))

        # ---------------------------------------
        # Estimate number of possible observations
        # ---------------------------------------
        # Merging quality information, interval information and observation statistics
        tmp   <- Reduce(merge, list(qual, inter)); rm(list = c("qual", "inter"))
        ndays <- 1 + as.numeric(tmp$quality_end - tmp$quality_start, units = "days")

        # Account for 'tod'; estimate number of observations possible for that
        # specific time period.
        if ("tod" %in% names(tmp)) {
            tod_hours <- tod_to_hours(tmp$tod)
            tmp$Nestim <- ndays * tod_hours * 3600 / tmp$interval_Median
        # Else assume it is 'all day long' (24 hours of possible observations)
        } else if (all(c("year") %in% names(tmp))) {
            # No 'tod' (all day long) but on a monthly basis.
            tmp$Nestim <- ndays * 86400 / tmp$interval_Median
        } else {
            stop("Don't know how to handle this one [annex package needs to be extended]\n")
        }

        # Enforce date
        tmp <- transform(tmp,
                         quality_start = as.Date(quality_start, origin = "1970-01-01"),
                         quality_end = as.Date(quality_end, origin = "1970-01-01"))

        return(Reduce(merge, list(tmp, stats)))
    }

    # Splitting data; aggregate data
    res <- lapply(object_split, aggfun, f = f)
    res <- if (length(res) == 1) res[[1]] else bind_rows(res)

    # Same but grouping only by study|home|room|year|month (all day long)
    res_all_day  <- aggfun(object, f, c("year", "month"))
    res_all_day  <- transform(res_all_day, tod  = "all")

    # Same but grouping only by study|home|room|year|tod (all year long)
    res_all_year <- aggfun(object, f, c("year", "tod"))
    res_all_year <- transform(res_all_year, month = "all")

    # Combine results
    res <- bind_rows(list(res_all_day, res_all_year, res))

    # Factorize and sort
    first <- c("study", "home", "room", "year", "month", "tod", "variable")
    for (n in first) res[[n]] <- factor(res[[n]])
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
    # Identify grouping variable (plus month, tod, and variable if existing)
    idvar   <- c("year", "month", "tod", "variable")
    idvar   <- c(f$group, idvar[idvar %in% names(x)])

    # Reshape to wide format
    if (is_long && (is.null(format) || format == "wide")) {
        x        <- pivot_wider(x, names_from = "stats", values_from = "value")
        # Convert quality_start and quality_end back to 'Date'
        x <- transform(x,
                       quality_start = as.Date(quality_start, origin = "1970-01-01"),
                       quality_end   = as.Date(quality_end,   origin = "1970-01-01"))
        x <- structure(x, class = c("annex_stats", "annex_stats_wide", "data.frame"),
                       formula = formula)
    # Reshape to long format
    } else if (!is_long && (is.null(format) || format == "long")) {
        # Need to convert 'Date' to numeric
        x <- transform(x,
                       quality_start = as.numeric(quality_start),
                       quality_end   = as.numeric(quality_end))
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
