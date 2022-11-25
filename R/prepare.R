


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
    # Append unque identifier for study/home/room
    config$ID <- with(config, interaction(study, home, room, sep = ":", drop = TRUE))
    config$ID[config$variable == "datetime"] <- NA # Ensure this one is NA
    config$ID <- droplevels(config$ID)

    # Next we need to check if all columns in 'data' are described
    # in 'config'.
    idx <- which(!names(x) %in% config$column)
    if (length(idx) > 0)
        stop("cannot find config for data columns: ", paste(names(x)[idx], collapse = ", "))

    # Preparing the data set
    # (1) Find all unique variables
    vars <- unique(subset(config, variable != "datetime", select = variable, drop = TRUE))

    # (2) Rename column names in x
    split_data <- function(targetID) {
        idx <- which(config$ID == targetID | config$variable == "datetime")
        tmp <- x[, idx]
        names(tmp) <- config$variable[match(names(tmp), config$column)]
        tmp$ID <- targetID

        # Ordering columns and return
        first_cols <- c("datetime", "ID")
        return(tmp[, c(first_cols, sort(names(tmp)[!names(tmp) %in% first_cols]))])
    }
    tmp <- sapply(levels(config$ID), split_data)
    return(bind_rows(tmp))
}
