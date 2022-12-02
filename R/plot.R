
#' Standard plot for annex objects
#'
#' TODO(R)
#'
#' @param x an object of class \code{annex}.
#' @param bygroup logical, by default the subplots are
#'        build up on different variables. If \code{TRUE},
#'        all varables from one group will be plotted in one subplot.
#' @param start the start time of the period of interest.
#' @param end the end time of the period of interest.
#' @param \dots currently unused.
#'
#' @importFrom zoo zoo index coredata
#' @author Reto Stauffer
#' @method plot annex
#' @export
plot.annex <- function(x, bygroup = FALSE, start = NULL, end = NULL, ...) {
    stopifnot(inherits(x, "annex"))
    f <- annex_parse_formula(attr(x, "formula"))
    stopifnot(isTRUE(bygroup) | isFALSE(bygroup))

    # Check/evaluate start and end time
    x_tz <- format(x[[f$time]][1], "%Z")
    if (!is.null(start)) { stopifnot(length(start) == 1L); start <- as.POSIXct(start, tz = x_tz) }
    if (!is.null(end))   { stopifnot(length(end) == 1L);   end   <- as.POSIXct(end,   tz = x_tz) }

    # Splitting the data set
    tozoo <- function(x, start, end) {
        # TODO(R): Make time series strictly regular -> that kills your machine
        # on the test data set. Even needed/wanted?
        #####dr <- range(x[, f$time], na.rm = TRUE) # range
        #####dd <- min(diff(x[, f$time]))           # time difference
        #####merge(zoo(x[, f$vars], x[, f$time]), zoo(, seq(dr[1], dr[2], by = dd)))
        window(zoo(x[, f$vars], x[, f$time]), start = start, end = end)
    }
    tmp <- split(x, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    names(tmp) <- lapply(tmp, function(x) sprintf("%s:%s:%s", x[1, "study"], x[1, "home"], x[1, "room"]))
    tmp <- lapply(tmp, tozoo, start = start, end = end)

    # Datetime range
    datetime_range <- range(x[, f$time], na.rm = TRUE)
    datetime_range <- c(max(datetime_range[1], start), min(datetime_range[2], end))

    # Number of data sets, number of variables
    n_sets <- length(tmp)
    n_vars <- length(f$vars)

    # Setting plot parameters
    hold <- par(no.readonly = TRUE); on.exit(par(hold))
    par(mar = c(3, 5, 1, 1))

    # Plot by group (often not the most useful variant)
    if (bygroup) {
        par(mfrow = c(n_sets, 1))
        for (i in seq_along(tmp)) {
            plot(tmp[[i]], screen = 1, col = seq_len(NCOL(x)),
                 xlab = NA, ylab = "value")
            mtext(side = 1, names(tmp)[i])
            legend("topleft", legend = names(tmp[[i]]), lty = 1,
                   bty = "n", col = seq_len(NCOL(x)))
        }
    # Else by variable
    } else {
        getlim <- function(tmp, v) if (!v %in% names(x)) NA else range(x[, v], na.rm = TRUE)
        par(mfrow = c(n_vars, 1))
        is_first <- TRUE
        for (v in f$vars) {
            ylim <- range(unlist(lapply(tmp, getlim, v = v)), na.rm = TRUE)
            if (any(is.na(ylim))) stop("no non-missing data for variable '", v, "'")
            # Start plotting
            # Base plot
            plot(zoo(0, datetime_range), type = "n", ylim = ylim, xlab = NA, ylab = v)
            # Adding observations
            for (i in seq_along(tmp)) {
                ###cat("testing ", v, "  ", paste(names(tmp[[i]]), collapse = ", "), "\n")
                if (!v %in% names(tmp[[i]])) next # Variable does not exist in this group
                lines(index(tmp[[i]]), coredata(tmp[[i]])[, v], col = i)
            }
            if (is_first) {
                legend("topleft", legend = names(tmp),
                       col = seq_along(tmp), bty = "n", lty = 1,
                       ncol = 1 + (length(tmp) - 1) %/% 10)
                is_first <- FALSE
            }
        }
    }

    invisible(NULL)
}

#' Standard plot for annex_stats objects
#'
#' TODO(R): How can one plot this 5d object?
#'
#' @param x an object of class \code{annex}.
#' @param \dots currently unused.
#'
#' @author Reto Stauffer
#' @method plot annex_stats
plot.annex_stats <- function(x, ...) {
    stopifnot(inherits(x, "annex_stats"))
    f <- annex_parse_formula(attr(x, "formula"))

    tmp <- split(x, formula(paste("~ ", paste(f$group, collapse = " + "))), drop = TRUE)
    cat("This is a 5 dimensional data set; how to plot?")
}



