# -------------------------------------------------------------------
# Testing Small Sample Size
#
# It is possible that only very few valid observations fall into
# a period for which we calculate the statistics; in the worst case
# the number of valid observations is simply 1 (N - NAs == 1).
#
# We decided to remove the summary statistics for cases where the
# number of valid observations is < 10. This tinytest creates a
# minimal example and also tests validation to ensure that
# the validator flags these cases correctly.
# -------------------------------------------------------------------

if (interactive()) {
    library("tinytest")
    library("annex")
}


# Creating a simulated data set w/ 5-min observations (Europe/Berlin time
# (CET)) for a series of rooms with some artificial temperature observations
# for three rooms; from all observations being available to 
# all missing in different proportions.
#
# All observations from 10:00 local time to 14:00 local time (only during daytime).
#
# Room LIV1: All data available (48 observations per day)
# Room LIV2: Only 30 observations per day.
# Room LIV3: Only 30 valid observations per day.
# Room LIV4: Only 29 observations per day.
# Room LIV5: Only 29 valid observations per day.
# Room LIV6: Only  1 observations per day.
# Room LIV7: Only  1 valid observations per day.
# Room LIV8: Only  0 valid observations per day.
#
# This is the 'prepared df' format which can directly be used with annex().
times <- seq(from = as.POSIXct("2023-01-01 10:00:00", tz = "Europe/Berlin"),
             to   = as.POSIXct("2023-01-01 15:00:00", tz = "Europe/Berlin"),
             by   = 5 * 60)
expect_equal(length(times), 61L,
             info = "Checking length of simulated time data set")

# Helper function
# (1) Create artificial random temperatures around 20 degrees
# (2) Set elements to NA until we only have N valid (non-missing) values
# (3) Create data.frame
# (4) if na.rm = TRUE: delete entries with missing values.
# Returns a data.frame in the 'prepared' format which can be used with annex()
fn <- function(times, room, N, na.rm) {
    stopifnot(inherits(times, "POSIXt"), length(times) > 0)
    stopifnot(is.character(room), length(room) == 1)
    stopifnot(is.numeric(N), N >= 0)
    stopifnot(isTRUE(na.rm) || isFALSE(na.rm))

    # (1) Simulate temperature
    tmp <- as.numeric(format(times, "%H%M")) / 2400 * 2 * pi
    tmp <- round(-cos(tmp) * 2 + 20 + rnorm(length(tmp), mean = 0, sd = 1), 1)

    # (2) If N < length(tmp): Setting elements to NA
    if (N < length(tmp)) {
        idx <- sample(seq_along(tmp), length(tmp) - N)
        tmp[idx] <- NA
    }
    res <- data.frame(study = "tinytest", home = "minimal", room = room,
                      datetime = times, T = tmp)
    return(if (na.rm) na.omit(res) else res)
}

# Configuration for simulating the data
cnf <- "room,N,na.rm
LIV1, 999, FALSE
LIV2,  30, TRUE
LIV3,  30, FALSE
LIV4,  29, TRUE
LIV5,  29, FALSE
LIV6,   1, TRUE
LIV7,   1, FALSE
LIV8,   0, FALSE"
cnf <- read.csv(text = cnf, colClasses = list(room = "character", N = "integer", na.rm = "logical"))

data <- list()
set.seed(6020)
for (i in seq_len(nrow(cnf))) data[[i]] <- fn(times, cnf$room[i], cnf$N[i], cnf$na.rm[i])
data <- do.call(rbind, data)

# Testing my own data set
tmp <- split(data, data$room)
expect_equivalent(sapply(tmp, function(y) sum(!is.na(y$T))),
                  c(61, 30, 30, 29, 29, 1, 1, 0),
                  info = "Checking number of non-missing values in demo data set")
expect_equivalent(sapply(tmp, function(y) sum(is.na(y$T))),
                  c(0, 0, 31, 0, 32, 0, 60, 61),
                  info = "Checking number of missing values in demo data set")
expect_identical(attr(data$datetime, "tzone"), "Europe/Berlin",
                 info = "Checking time zone of demo data set")


# -------------------------------------------------------------------
# Prepare the data set and calculate the statistics
# -------------------------------------------------------------------
expect_silent(prepared_df <- annex(T ~ datetime | study + home + room, data = data, tz = "Europe/Berlin"),
              info = "Prepare annex data.frame")
expect_identical(nrow(prepared_df), nrow(data),
                 info = "Checking dimension of prepared annex df; should be the same as 'data'")

# Calculating stats
stats <- annex_stats(prepared_df)
expect_silent(stats <- annex_stats(prepared_df),
              info = "Calculating annex statistics")


# -------------------------------------------------------------------
# Prepare the data set and calculate the statistics
# -------------------------------------------------------------------

# Check: if N - NAs < 10 we should not have certain statistics. This
# includes 'Mean', 'Sd', and all percentiles, and we also removed
# the guessed intervals (as this guess might be way off) including 'Nestim'.
expect_identical(length(idx <- which(stats$N - stats$NAs < annex:::minsamplesize)), 24L,
                 info = "Count/check rows where N - NAs < 10")

# If N - NAs is lower than annex:::minsamplesize, Mean and Sd must be NA
idx <- which(stats$N - stats$NAs < annex:::minsamplesize)
expect_identical(idx,
                 as.integer(unlist(lapply(c(4, 11, 18, 25, 32, 39), function(x) x + seq.int(0, 3)))),
                 info = "Rows/entries where N - NAs is 'small' (smaller than annex:::minsamplesize)")
# Check that Mean and Sd is missing
expect_true(all(is.na(stats$Mean[idx])),
            info = "Checking Mean == NA if N - NAs too small")
expect_true(all(is.na(stats$Sd[idx])),
            info = "Checking Sd == NA if N - NAs too small")

# If N - NAs is == 1 we can't guess an intervall. Check that all interval_* entries
# as well as Nestim is NA.
cols <- names(stats)[grepl("^(interval_.*|Nestim)$", names(stats))]
idx  <- which(stats$N - stats$NAs == 1)
expect_true(all(is.na(stats[idx, cols])),
            info = "Checking interval_* and Nestim == NA if N - NAs == 1")

# -------------------------------------------------------------------
# Save to stats file
# -------------------------------------------------------------------
tmpfile <- tempfile(fileext = ".xlsx")
expect_silent(annex_write_stats(stats, tmpfile, user = 999),
              info = "Writing statistics to file")
expect_true(annex:::annex_validate_sheet_STAT(tmpfile, user = 999, quiet = TRUE),
            info = "Testing XLSX sheet 'STAT' (only this one)")

# -------------------------------------------------------------------
# Testing error checks. Therefore we make a copy of the xlsx file
# and manually manipulate some of the entries in the sheet 'STAT'
# to see if annex_validate_sheet_STAT() finds them.
# -------------------------------------------------------------------
require("openxlsx")
tmpfile2 <- tempfile(fileext = ".xlsx")

savewb <- function(wb, dst, data) {
    writeData(wb, sheet = "STAT", x = data, colNames = FALSE, startRow = 2, startCol = 1)
    saveWorkbook(wb, dst, overwrite = TRUE)
}

# Setting Mean of first data row to NA (must not be NA)
wb  <- loadWorkbook(tmpfile)
tmp <- readWorkbook(wb, sheet = "STAT")
tmp$Mean[1] <- NA
savewb(wb, tmpfile2, tmp)
expect_error(annex:::annex_validate_sheet_STAT(tmpfile2, user = 999, quiet = TRUE),
             pattern = ".*missing values.*in column 'Mean'.*",
             info = "Missing 'Mean' altough N - NAs > required minimum")

# Setting Sd of first data row to NA (must not be NA)
wb  <- loadWorkbook(tmpfile)
tmp <- readWorkbook(wb, sheet = "STAT")
tmp$Sd[1] <- NA
savewb(wb, tmpfile2, tmp)
expect_error(annex:::annex_validate_sheet_STAT(tmpfile2, user = 999, quiet = TRUE),
             pattern = ".*missing values.*in column 'Sd'.*",
             info = "Missing 'Mean' altough N - NAs > required minimum")

# Same if Sd is negative
wb  <- loadWorkbook(tmpfile)
tmp <- readWorkbook(wb, sheet = "STAT")
tmp$Sd[1] <- -1
savewb(wb, tmpfile2, tmp)
expect_error(annex:::annex_validate_sheet_STAT(tmpfile2, user = 999, quiet = TRUE),
             pattern = ".*missing values.*in column 'Sd'.*",
             info = "Missing 'Mean' altough N - NAs > required minimum")








