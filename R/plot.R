
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
#' @import graphics
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
#' Experimental function for objects of class \code{annex_stats}.
#'
#' @param x an object of class \code{annex_stats}
#' @param tod time of day
#' @param by should the statistics be plotted across years or year + month
#' @param ncol number of columns in plot
#' @param ask one of \code{NULL}, \code{TRUE}, or \code{FALSE}. Auto-detected if \code{NULL}
#' @param \dots currently unused.
#'
#' @author Reto Stauffer
#' @method plot annex_stats
#' @importFrom zoo as.yearmon
#' @export
plot.annex_stats <- function(x, tod = c("07-23", "23-07", "all"), by = c("year", "yearmon"), ncol = 1L, ask = NULL, ...) {
    stopifnot(inherits(x, "annex_stats"))
    stopifnot(is.null(ask) || (isTRUE(ask) || isFALSE(ask)))

    # Experimental function
    message("[!] Experimental function")

    f <- annex_parse_formula(attr(x, "formula"))
    mytod <- match.arg(tod)
    by    <- match.arg(by)

    # Keep user settings
    hold  <- par(no.readonly = TRUE); on.exit(par(hold))

    if (tod == "all" & by == "year")
        stop("combination `tod = \"all\"` and `by = \"year\"` not allowed")

    # Number of columns/rows to plot
    ncol  <- as.integer(ncol)[1L]
    stopifnot(is.integer(ncol), ncol >= 1L)

    # home/room combinations
    x$home_room <- interaction(x$home, x$room, drop = TRUE)
    message("There are ", nlevels(x$home_room), " unique home/room 'units'")

    if (nlevels(x$home_room) > 1 && is.null(ask)) ask <- TRUE
    if (ask) par(ask = TRUE)

    for (hr in levels(x$home_room)) {
        # Current home/room
        tmp <- subset(x, home_room == hr & tod == mytod)

        # For x axis
        if (by == "year") {
            tmp <- subset(tmp, month == "all")
            xat <- sort(unique(as.integer(as.character(tmp$year))))
            xwidth <- 1
        } else if (by == "yearmon") {
            tmp <- subset(tmp, month != "all" & tod == mytod)
            tmp$yearmon <- as.yearmon(paste(tmp$year, tmp$month, sep = "-"), format = "%Y-%m")
            xat <- sort(unique(tmp$yearmon))
            xwidth <- 1 / 12
        }

        vars <- unique(tmp$variable)
        nrow <- ceiling(length(vars) / ncol) # Number of rows for plot
        par(mfcol = c(nrow, ncol), oma = c(5.1, 0, 4.1, 2), mar = c(0, 5.1, 0, 0), xaxt = "n")

        counter <- 0
        for (v in vars) {
            counter <- counter + 1L
            ylim <- with(subset(tmp, variable == v), range(p00, p100))
            plot(NA, xlim = as.numeric(range(xat)) + c(-.5, .5) * xwidth, ylim = ylim, ylab = NA)

            sapply(xat, function(y) add_boxplot(subset(tmp[tmp[[by]] == y, ], variable == v), y, xwidth))
            mtext(v, side = 2, line = 3)

            if (counter %% nrow == 0 | counter == length(vars)) {
                par(xaxt = "s"); axis(side = 1, line = 0, at = xat, labels = xat); par(xaxt = "n")
            }
        }

        # Adding title
        mtext(side = 3, line = 1, paste(tmp$home[1], tmp$room[1], sep = " - "), font = 2, cex = 1.2,
              outer = TRUE)
    }
}

add_boxplot <- function(x, y, w) {
    if (nrow(x) == 0) return(NULL)
    y <- as.numeric(y)
    stopifnot(nrow(x) == 1)
    lines(c(y, y), c(x$p00, x$p100))
    sapply(c(x$p00, x$p100), function(xx) lines(y + c(-0.2, 0.2) * w, c(xx, xx)))
    rect(y - w * 0.3, x$p25, y + w * 0.3, x$p75, col = "gray80")
    lines(y + c(-0.3, 0.3) * w, c(x$p50, x$p50), lwd = 2)
}


