


#' Prepare Annex Data
#'
#' TODO(R)
#'
#' @param x data.frame, the data itself.
#' @param config data.frame, config information (see
#'        [annex::annex_check_config()]).
#' @param quiet logical, default \code{FALSE}. If set \code{TRUE},
#'        messages and warnings will be suppressed.
#'
#' @return Prepared \code{data.frame} for further processing with
#'         the annex package.
#'
#' @importFrom dplyr bind_rows
#' @author Reto Stauffer
#' @export
annex_prepare <- function(x, config, quiet = FALSE) {
    if (inherits(x, "tbl_df")) x <- as.data.frame(x)
    stopifnot(is.data.frame(x), is.data.frame(config))

    # Checking config; this would fail if it does not contain 
    # the expected information.
    config <- annex_check_config(config)

    # Check if there are columns described in 'config' which are not
    # existing in the data set. If we find these, we'll drop a warning
    # but are OK to continue.
    idx <- which(!config$column %in% names(x))
    if (length(idx) > 0 & !quiet)
        warning("Defined columns in `config` not present in `x`: ",
                paste(sprintf("'%s'", config$column[idx]), collapse = ", "))

    # Inform the user that not all columns are used
    idx <- which(!names(x) %in% config$column)
    if (length(idx) > 0 & !quiet)
        message("Columns in `x` not in `config` (will be ignored): ",
                paste(sprintf("'%s'", names(x)[idx]), collapse = ", "))

    # Subsetting the data set to the requested (defined) columns
    idx <- which(names(x) %in% config$column)
    if (length(idx) <= 1) stop("no data columns in `config` found in object `x`")
    x <- x[, idx]

    # Ensure that all "data columns" are numeric. 
    # - if numeric: good
    # - if not numeric and only contains missing values (e.g., NA but integer)
    #   we coerce the entire variable/column to NA_real_.
    # - else we will throw an error.
    original_vars <- unique(subset(config, variable != "datetime", select = column, drop = TRUE))
    original_vars <- original_vars[original_vars %in% names(x)]
    for (ov in original_vars) {
        if (!is.numeric(x[, ov]) && all(is.na(x[, ov]))) {
            x[, ov] <- NA_real_
        } else if (!is.numeric(x[, ov])) {
            stop(sprintf("column/variable `%s` is not numeric (class %s). Please check and convert manually.",
                         ov, paste(class(x[, ov]), collapse = ", ")))
        }
    }

    # Preparing the data set
    # (1) Find all unique variables
    vars <- unique(subset(config, variable != "datetime", select = variable, drop = TRUE))

    # Convert units if needed
    for (n in names(x)) {
        cnf <- as.list(subset(config, column == n))
        if (!is.na(cnf$unit)) {
            x[, n] <- do.call(sprintf("convert_unit_%s", variable_basename(cnf$variable)),
                              list(x = x[, n], from = cnf$unit))
        }
    }

    # (2) Rename column names in x
    split_data <- function(current_ID) {
        # Variables to be processed; extract data from x
        cnf <- subset(config, variable == "datetime" | ID == current_ID)
        idx <- which(names(x) %in% cnf$column)
        tmp <- x[, idx]

        # Renaming
        names(tmp) <- cnf$variable[match(names(tmp), cnf$column)]

        # Adding identifier
        current_ID <- setNames(strsplit(current_ID, ":")[[1]], c("study", "home", "room")) # split
        for (n in names(current_ID)) tmp[[n]] <- current_ID[[n]]

        # Ordering columns and return
        first_cols <- c("datetime", "study", "home", "room")
        tmp <- tmp[, c(first_cols, sort(names(tmp)[!names(tmp) %in% first_cols]))]
        if (any(is.na(tmp$datetime)))
            stop("`datetime` (originally column `",
                 subset(config, variable == "datetime", select = column, drop = TRUE),
                 "`) contains missing values")

        # Return chunk
        return(tmp)
    }

    # Create unique IDs; ensure 'datetime' is excluded.
    config <- transform(config, ID = interaction(study, home, room, sep = ":"))
    config$ID[config$variable == "datetime"] <- NA # Ensure this one is NA
    tmp <- lapply(levels(droplevels(config$ID)), split_data)
    tmp <- bind_rows(tmp)

    # Stop if `datetime` is not of class POSIXt
    if (!inherits(tmp$datetime, "POSIXt")) {
        print(names(tmp))
        orig <- subset(config, variable == "datetime")$column
        stop("variable `datetime` (originally column `", orig, "`) must be of class POSIXt")
    }
    return(tmp)

}


