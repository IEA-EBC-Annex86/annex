


#' Prepare Annex Data
#'
#' TODO(R)
#'
#' @param x data.frame, the data itself.
#' @param config data.frame, config information (see
#'        \code{link{check_config()}}).
#'
#' @return Prepared \code{data.frame} for further processing with
#'         the annex package.
#'
#' @importFrom dplyr bind_rows
#' @author Reto Stauffer
#' @export
annex_prepare <- function(x, config) {
    stopifnot(is.data.frame(x), is.data.frame(config))

    # Checking config; this would fail if it does not contain 
    # the expected information.
    config <- check_config(config)

    # Next we need to check if all columns in 'data' are described
    # in 'config'.
    idx <- which(!names(x) %in% config$column)
    if (length(idx) > 0)
        stop("cannot find config for data columns: ", paste(names(x)[idx], collapse = ", "))

    # Preparing the data set
    # (1) Find all unique variables
    vars <- unique(subset(config, variable != "datetime", select = variable, drop = TRUE))

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
        return(tmp[, c(first_cols, sort(names(tmp)[!names(tmp) %in% first_cols]))])
    }

    # Create unique IDs; ensure 'datetime' is excluded.
    config <- transform(config, ID = interaction(study, home, room, sep = ":"))
    config$ID[config$variable == "datetime"] <- NA # Ensure this one is NA
    tmp <- lapply(levels(droplevels(config$ID)), split_data)
    return(bind_rows(tmp))
}
