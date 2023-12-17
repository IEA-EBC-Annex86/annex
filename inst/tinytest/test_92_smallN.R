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


# Creating a simulated data set w/ 10-min observations (Europe/Berlin time
# (CET)) for a series of rooms with some artificial temperature observations
# for three rooms; from all observations being available to 
# all missing in different proportions.
#
# All observations from 10:00 local time to 14:00 local time (only during daytime).
#
# Room LIV1: All data available (48 observations per day)
# Room LIV2: Only 10 observations per day.
# Room LIV3: Only 10 valid observations per day.
# Room LIV4: Only  9 observations per day.
# Room LIV5: Only  9 valid observations per day.
# Room LIV6: Only  1 observations per day.
# Room LIV7: Only  1 valid observations per day.
# Room LIV8: Only  0 valid observations per day.
#
# This is the 'prepared df' format which can directly be used with annex().
times <- seq(from = as.POSIXct("2023-01-01 10:00:00", tz = "Europe/Berlin"),
             to   = as.POSIXct("2023-01-01 15:00:00", tz = "Europe/Berlin"),
             by   = 10 * 60)
expect_equal(length(times), 31L,
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
LIV2,  10, TRUE
LIV3,  10, FALSE
LIV4,   9, TRUE
LIV5,   9, FALSE
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
                  c(31, 10, 10, 9, 9, 1, 1, 0),
                  info = "Checking number of non-missing values in demo data set")
expect_equivalent(sapply(tmp, function(y) sum(is.na(y$T))),
                  c(0, 0, 21, 0, 22, 0, 30, 31),
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
expect_silent(stats <- annex_stats(prepared_df),
              info = "Calculating annex statistics")

# -------------------------------------------------------------------
# Prepare the data set and calculate the statistics
# -------------------------------------------------------------------

# Check: if N - NAs < 10 we should not have certain statistics. This
# includes 'Mean', 'Sd', and all percentiles, and we also removed
# the guessed intervals (as this guess might be way off) including 'Nestim'.
expect_identical(length(idx <- which(stats$N - stats$NAs < 10)), 12L,
                 info = "Find rows where N - NAs < 10")

# Find 'inveral_*', 'Nestim', 'Mean', 'Sd' and all 'p*' cols.
cols <- names(stats)[grepl("^(interval_.*|Nestim|Mean|Sd|p[0-9\\.]+)$", names(stats))]
expect_identical(length(cols), 114L,
                 info = "Identify columns which should be NA")
expect_true(all(is.na(stats[idx, cols])),
            info = "Check if the cells which should be NA are indeed NA")

# -------------------------------------------------------------------
# Save to stats file
# -------------------------------------------------------------------
tmpfile <- tempfile(fileext = ".xlsx")
expect_silent(annex_write_stats(stats, tmpfile, user = 999),
              info = "Writing statistics to file")
expect_true(annex:::annex_validate_sheet_STAT(tmpfile, user = 999, quiet = TRUE),
            info = "Testing XLSX sheet 'STAT' (only this one)")




