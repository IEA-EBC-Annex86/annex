
#' Calculate Statistics on Annex object
#'
#' @param object an object of class \code{annex}.
#' @param format character, either \code{"wide"} (default) or \code{"long"}.
#' @param \dots currently unused.
#'
#' @return Returns an object of class \code{c("annex_stats", "data_frame")}.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_rows
#' @author Reto Stauffer
#' @export
annex_stats <- function(object, format = "wide", ...) {
    stopifnot(inherits(object, "annex"))
    format <- match.arg(format, c("wide", "long"))
    f <- annex_parse_formula(attr(object, "formula"))

    # Functions to apply to calculate the stats
    get_summary <- function(x, digits = 4)
        return(round(setNames(quantile(x, p = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE),
                              c("Min", "p2.5", "p25", "p50", "p75", "p97.5", "Max")), digits = digits))

    # Helper function to caluclate the shape
    shape <- function(x, na.rm = TRUE) {
        mean(x, na.rm = na.rm)^2 * ((1 - mean(x, na.rm = na.rm)) / var(x, na.rm = na.rm) - 1 / mean(x, na.rm = na.rm))
    }

    # Reshaping result of aggregate()
    convert <- function(var, data, f, gx = c("season", "tod")) {
        res <- as.data.frame(data[, var])
        res <- cbind(transform(data[, c(f$group, gx)], variable = var), res)
        if (format == "long") {
            idvar   <- c(f$group, "season", "tod", "variable")
            varying <- names(res)[!names(res) %in% idvar]
            res     <- as.data.frame(pivot_longer(res, cols = varying, names_to = "stats"))
        }
        return(res)
    }

    # List of functions to be applied; must all return a named vector
    functionlist <- list(
        get_summary,
        function(x) c(Sd     = sd(x, na.rm = TRUE)),
        function(x) c(Mean   = mean(x, na.rm = TRUE)),
        function(x) c(N      = length(x), NAs = sum(is.na(x))),
        function(x) c(shape1 = shape(x)),
        function(x) c(shape2 = shape(x) * (1 / mean(x, na.rm = TRUE) - 1)),
        function(x) c(exp_param = 1 / mean(x, na.rm = TRUE))
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

    rownames(res) <- NULL
    class(res) <- c("annex_stats", paste("annex_stats", format, sep = "_"), class(res))
    attr(res, "formula") <- attr(object, "formula")
    return(res)
}

