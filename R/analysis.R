
# Script containing a series of functions used to analyze the data.
# Likely to be changed or extended in the future.

# DEVELOPMENT STATUS!

#' Get (Mixed) Empirical Distribution
#'
#' @param x a named vector or `data.frame` with empirical quantiles and
#'        sample size (optional). See Section 'Details' for more information.
#' @param `...` see Section 'Details'.
#' @param verbose Logical, if `TRUE` some additional output is shown.
#'
#' @details
#' This function returns an `annex_edist` (annex empirical distribution) object
#' which can be based on a single empirical distribution, or a (weighted) mixed
#' distribution. `annex_stats` stores the empirical distribution of our measurements
#' given a series of quantiles as well as the sample size the quantiles are based on.
#' This function allows a series of different inputs.
#'
#' **Named numeric vector:** `x` can be a named numeric vector. In this case,
#' all the names of the vector must be unique and follow
#' `^p[0-9]{1,3}(\.[0-9]{,2})?$`. Some examples: `p00` (minimum), `p2` (2_th_
#' percentile or 0.02 quantile), `p02.5` (2.5_th_ percentile or 0.025
#' quantile), `p05` (5_th_ percentile or quantile 0.05) etc. up to `p100`
#' (maximum). Minimum (`p00`) and maximum (`p100`) _must_ be given, the
#' quantiles in between are handled flexible.
#'
#' @author Reto Stauffer
annex_dist <- function(x, ..., verbose = TRUE) {
    x <- annex_dist_process_input(x, ..., verbose = verbose)

}


#' Helper Function to Prepare User Input on `annex_dist`
#'
#' Helper function handling the different input types allowed when calling
#' `annex_process_input`. Check man page for `annex_process_input` for details.
#'
#' @return Returns prepared (standardized) list for further processing.
#'
#' @author Reto Stauffer
annex_dist_process_input <- function(x, ..., verbose = TRUE) {

    dots <- as.list(match.call())[-1] # -1: Ignore function itself
    dots <- list(...)

    # Helper function to check (and convert!) names.
    # Must follow `^p[0-9]{2}(\.[0-9]{,2})?$` or `N`.
    #
    # @return Returns updated names with format p[0-9]{2-3}(\\.[0-9]{3})?,
    #         only allows for three digits after the coma to be considered
    #         as unique.
    check_and_fix_names <- function(x) {
        # Must be named
        if (is.null(names(x)))
            stop("got unnamed input (elements must be named)")
        # Missing values are not allowed
        if (any(is.na(x)))
            stop("missing values are not allowed")

        # Pattern for percentiles
        ppat <- "p[0-9]{1,3}(\\.[0-9]{1,2})?" # pattern for percentiles
        idx <- which(!grepl(sprintf("^(%s|N)$", ppat), names(x)))
        if (length(idx) > 0)
            stop("found non-allowed names, namely: ", paste(names(x)[idx], collapse = ", "))
        # Names must be unique; first check for dups, we 
        idx <- which(duplicated(names(x)))
        if (length(idx) > 0)
            stop("found duplicate names (not allowed):", paste(names(x)[idx], collapse = ", "))
        # Converting names; streamlining format and check if anything is duplicate.
        idx <- grep(sprintf("^(%s)$", ppat), names(x))
        if (length(idx) == 0)
            stop("not found any elements named `p.*`)")
        ppat2 <- sprintf("(?!=p)%s$", gsub("^p", "", ppat))
        pnum  <- as.numeric(regmatches(names(x)[idx], regexpr(ppat2, names(x)[idx], perl = TRUE)))
        # Rename (streamline)
        names(x)[idx] <- sprintf("p%05.2f", pnum)
        # If any of the names are not unique, throw error.
        idx <- which(duplicated(names(x)))
        if (length(idx) > 0)
            stop("duplicated names after streamlining (check man page). ",
                 "Got: ", paste(names(x)[idx], collapse = ", "), " at least twice")

        return(names(x))
    }

    # If `x` is data.frame we simply split each observation (row) into
    # an element of our list `res`.
    if (is.data.frame(x)) {
        # All must be numeric
        if (!all(sapply(x, is.numeric)))
            stop("argument `x` (data.frame) must only contain numerics")
        # Checking names of the data.frame and split each row of the DF
        # into a numeric vector and keep that list.
        names(x) <- check_and_fix_names(x)
        x   <- x[, order(names(x))] # Sort (not yet mandatory but to be nice)
        fn  <- function(i, x) setNames(as.numeric(unlist(x[i, ])), names(x))
        res <- lapply(seq_len(nrow(x)), fn, x = x)
        names(res) <- sprintf("dist%d", seq_len(length(res))) # Renaming
    # Else checking the vector `x` and append that as first distribution
    # (or element) on `res`.
    } else {
        # Must be numeric and named
        if (is.null(x))
            stop("argument `x` must be a named vector")
        if (!is.numeric(x))
            stop("argument `x` must be numeric")
        names(x) <- check_and_fix_names(x)

        # Initial check survived, append this (re-) named vector as first
        # entry in our `res` list.
        res <- list(dist1 = x)
    }
    
    # List if arguments to be ignored
    ignore_args <- c("verbose")

    # If length(dots) is larger than 0, append them as well
    if (length(dots) > 0) {

        # Looping over all additional args (must be numeric vectors, no missing
        # values, proper names).
        for (i in seq_along(dots)) {
            if (!is.null(names(dots)) && names(dots)[i] %in% ignore_args) next
            tmp <- dots[[i]]
            # Checking type (must be named numeric vector)
            if (!is.vector(tmp) || !is.numeric(tmp) || is.null(names(tmp)))
                stop("additional arguments handed over to `annex_dist` must ",
                     "be numeric named numeric vectors")
            # Checking and fixing elemnt naming
            names(tmp) <- check_and_fix_names(tmp)
            name <- paste0("dist", length(res) + 1)
            res[[name]] <- tmp
        }

    }

    # ---------------------------------------------------------------
    # After the initial check `res` should be a list of named numeric
    # vectors. We now need to ensure that all have an 'N' or none
    # carries that along to properly calcualte a weighted mixed
    # distribution (if requested).
    check_n <- sapply(res, function(x) "N" %in% names(x))
    # Mixed bag of distributions with and without 'N'
    if (!all(check_n) && !all(!check_n))
        stop("some distributions contain `N` (sample size), others do not which ",
             "is not allowed. Either all or none must provide `N` used for weighting.")

    # Else we can now convert res into a list where the
    # first entry is $weights (sum up to 1) and the rest containing
    # data.frames with the distributions (quantile + cdf)
    if (all(!check_n)) {
        tmp <- list(weights = rep(1 / length(res), length(res)))
    } else {
        tmp <- sapply(res, function(x) x[["N"]])
        tmp <- list(weights = tmp / sum(tmp))
    }
    
    # Extracting quantiles from the named vectors, convert it into
    # data.frames, and check the content.
    convert_and_check_vector <- function(x) {
        x <- x[!names(x) == "N"]
        df <- data.frame(q = as.numeric(regmatches(names(x), regexpr("[0-9\\.]+", names(x)))),
                         d = unname(x))
        df <- df[order(df$q), ]
        if (anyDuplicated(df$q)) {
            print(x)
            stop("found duplicated quantiles in one of the distributions")
        }
        if (any(diff(df$d) < 0)) {
            print(x)
            stop("cdf of one of the distributions not monotonitcally increasing")
        }
        return(df)
    }
    for (n in names(res)) tmp[[n]] <- convert_and_check_vector(res[[n]])
    res <- tmp; rm(tmp)
    print(res)

    stop("check that all or none carry along 'N'")
    invisible(3)
}




