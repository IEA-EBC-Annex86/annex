
#' Reading Annex Statistics from XLSX
#'
#' The final data set (annex statistics) are written into an XLSX
#' file. This function allows to read one or multiple of these files
#' into R.
#'
#' @param file character, name of the file(s) to be imported.
#'        Must end on XLSX (not case sensitive).
#' @param raw logical. If \code{FALSE} a single \code{data.frame}
#'        will be returned, containing the statistics from 
#'        all file(s). If set \code{TRUE} the raw information
#'        of the XLSX file(s) is returned (see Details).
#' @param validate logical, if \code{TRUE} (default) the function
#'        first validates if the XLSX file is valid given the current
#'        version of annex.
#'
#' @details The XLSX files (if valid) contain a series of sheets,
#' namely 'STATS', 'META-Study', 'META-Home', 'META-Room', and
#' 'META-Variable' containing data information. This function
#' has two different returns (depending on \code{combine}).
#'
#' If \code{raw = FALSE} (default) only the 'STAT' sheet is
#' imported. If there is more than one \code{file} the information
#' from all files will be combined in one \code{data.frame} which
#' is returned.
#'
#' If \code{raw = TRUE} all the information from all sheets is
#' read and stored in a named list. If there are multiple files,
#' the content of the different sheets are combined. The return
#' is a named list of length 5 containing a \code{data.frame} each.
#' For usability purposes the sheet names will be modified,
#' replacing "-" (XLSX) with "_" (names of list).
#'
#' @return An object of class \code{c("annex_xlsx_stats", "data.frame")}
#' or a named list of \code{data.frame}s. Depends argument \code{raw},
#' see section 'Details'.
#'
#' @importFrom openxlsx getSheetNames read.xlsx
#' @author Reto Stauffer
#' @export
annex_read_stats <- function(file, raw = FALSE, validate = TRUE) {
    stopifnot(is.character(file))
    stopifnot(isTRUE(raw) || isFALSE(raw))
    stopifnot(isTRUE(validate) || isFALSE(validate))

    warning("This is a function used for development purposes (0.2-9; Dec 2023)")

    file <- unique(file) # Avoid duplicated files
    xlsx_check  <- grepl("\\.xlsx$", file, ignore.case = TRUE)
    if (any(!xlsx_check))
        stop(red("Not all `file`s ending on XLSX:", paste(file[!xlsx_check], collapse = ", ")))
    xlsx_exists <- file.exists(file)
    if (any(!xlsx_exists))
        stop(red("File/files not existing:", paste(file[!xlsx_exists], collapse = ", ")))
    rm(xlsx_exists, xlsx_check)

    # Checking if files are valid
    if (validate) {
        # Trying to get the user from the file first
        for (f in file) {
            usr <- read.xlsx(f, sheet = "STAT", rows = 1:2, cols = 1)[[1]]
            if (!grepl("^[0-9]+$", usr))
                stop("Problems identifying 'user' from 'STAT' sheet (not integer)")
            usr <- as.integer(usr)
            check_val <- annex_validate(f, user = usr, quiet = TRUE)
            if (!check_val)
                stop("File \"", f, "\" not valid, annex_validate() returns FALSE")
        }
    }

    # Reading all data from all sheets
    fn_read_sheet <- function(f, s) {
        tmp <- getSheetNames(f)
        if (!all(s %in% tmp))
            stop("File \"", f, "\" does not contain expected sheet ", s)
        # Reading all sheets as is
        return(read.xlsx(f, sheet = s))
    }

    if (raw) {
        sheets <- c("STAT", "META-Study", "META-Home", "META-Room", "META-Variable")
    } else {
        sheets <- "STAT"
    }

    res <- setNames(vector("list", length(sheets)), sheets)
    for (s in sheets) {
        res[[s]] <- do.call(rbind, lapply(file, fn_read_sheet, s = s))
    }

    # Renaming for usability in R
    names(res) <- gsub("-", "_", names(res))
    class(res$STAT) <- c("annex_xlsx_stats", class(res$STAT))
    return(if (raw) res else res$STAT)

}
