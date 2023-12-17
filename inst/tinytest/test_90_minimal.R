

if (interactive()) {
    library("tinytest")
    library("annex")
}

# Serves as our CSV file for the configuration
config_txt <- "
column,               variable,    study,    unit,     home,     room
abcd0001-home3_co2,   CO2,         Reto,     %,        H3,       BAT1
abcd0002-home3_co2,   CO2,         Reto,     %,        H3,       BAT2
abcd0003-home3_co2,   CO2,         Reto,     ppm,      H3,       KIT
abcd0004-home3_temp,  T,           Reto,     C,        H3,       BAT1
timeOfLoggingInUTC,   datetime,    Reto,     NA,       NA,       NA
abcd0005-home3_temp,  T,           Reto,     C,        H3,       BAT2
abcd0116-home3_temp,  T,           Reto,     K,        H3,       KIT
abcd0117-home9_temp,  RH,          Reto,     %,        H9,       KIT
abcd0301-home9_co2,   CO2,         Reto,     ppm,      H9,       LAU
abcd0302-home9_temp,  T,           Reto,     F,        H9,       LAU
abcd0303-home9_RH,    RH,          Reto,     %,        H9,       LAU
abcd0304-home9_Radon, Radon,       Reto,     Bq/m3,    H9,       LAU
"
# If not correct, this would throw an error
config <- read.csv(text = config_txt, sep = ",", strip.white = TRUE)
annex_check_config(config)

# Serves as our data set; set up in a very specific way to check
# if the `annex_stats` calcs are fine and the mapping with the
# config file works as expected. Time: Two periods, once
# at night and once during the day (local time) on two differnt
# days with an interval of 10 mins to check the interval/Nestim guesses).
# Each period has exactely N = 20 measurements which will be generated below
# to thest the calculation of the statistics. The second period is also
# set in a way (assuming UTC) to check if the 'tz' conversion works as
# expected; summer time; UTC -> CEST +2h
N <- 20
tms <- c(as.POSIXct("2023-12-24 00:00:00", tz = "UTC") + (seq_len(N) - 1) * 10 * 60,
         as.POSIXct("2023-07-01 05:30:00", tz = "UTC") + (seq_len(N) - 1) * 10 * 60)

# All the data we have will be 
data <- data.frame(timeOfLoggingInUTC = tms)
for (i in seq_len(nrow(config))) {
    tmp    <- as.list(config[i, ]) # Convert to list for convenience
    if (tmp$variable == "datetime") next # Skipping timeOfLoggingInUTC

    # Generate some data; always the "same" (uniform sequence from
    # seq(10, length.out = N, by = 0.5) such that we can
    # easily check the calculations later.
    values <- rep(seq(10, length.out = N, by = 0.5), 2)
    if (tmp$unit == "K") values <- values + 273.15
    if (tmp$unit == "%" & tmp$variable == "CO2") values <- values / 1e4
    data[[tmp$column]] <- values
}
# Setting the data for the room = LAU columns to NA.
# Once numeric NA, once logical NA, once character NA to check
# the `annex_prepare` checks.
data[["abcd0301-home9_co2"]]  <- NA_real_
data[["abcd0302-home9_temp"]] <- NA
data[["abcd0303-home9_RH"]]   <- NA_character_
# Store the data for room = LAU variable = Radon as character,
# This should cause an error in annex_prepare.
data[["abcd0304-home9_Radon"]] <- as.character(data[["abcd0304-home9_Radon"]])



# -------------------------------------------------------------------
# Checking if my 'data set' is set up properly
# -------------------------------------------------------------------
expect_identical(attr(data$timeOfLoggingInUTC, "tz"), "UTC")

expect_inherits(data[["abcd0301-home9_co2"]],   "numeric")
expect_inherits(data[["abcd0302-home9_temp"]],  "logical")
expect_inherits(data[["abcd0303-home9_RH"]],    "character")
expect_inherits(data[["abcd0304-home9_Radon"]], "character")

expect_true(all(is.na(data[["abcd0301-home9_co2"]])))
expect_true(all(is.na(data[["abcd0302-home9_temp"]])))
expect_true(all(is.na(data[["abcd0303-home9_RH"]])))
expect_true(sum(is.na(data)) == 3 * nrow(data))


# -------------------------------------------------------------------
# Testing annex_prepare
# -------------------------------------------------------------------
expect_error(annex_prepare(data, config = config),
             pattern = ".*`abcd0304-home9_Radon` is not numeric.*",
             info = "One column is character, causes an error")
data[["abcd0304-home9_Radon"]] <- NULL # Removing that column

# Now it should work
expect_warning(prep <- annex_prepare(data, config = config),
             pattern = ".*Defined columns in `config` not present.*'abcd0304-home9_Radon'.*",
             info = "Warning as config contains variables not present in data")

## Checking return
expect_inherits(prep, "data.frame")
expect_identical(dim(prep), c(200L, 7L))
expect_identical(names(prep), c("datetime", "study", "home", "room", "CO2", "T", "RH"))
expect_inherits(prep$datetime, "POSIXct")
expect_true(all(sapply(subset(prep, select = c(study, room, home)), class) == "character"))
expect_true(all(sapply(subset(prep, select = c(CO2, T, RH)), class) == "numeric"))

# We constructed 'data' in a way that we only expect values
# 10:15 and NA. Check if TRUE.
tmp <- unlist(subset(prep, select = c(CO2, T, RH)))
tmp <- unique(sort(round(tmp, 10), , na.last = TRUE))
expect_equal(tmp, c(seq(10, length.out = N, by = 0.5), NA))


# -------------------------------------------------------------------
# What if `config` does not contain all the information?
# Drop all temperature varaibles from `config`
# -------------------------------------------------------------------
config2 <- subset(config, variable != "T" & variable != "Radon")
expect_message(prep2 <- annex_prepare(data, config = config2),
               pattern = ".*Columns in `x` not in `config` \\(will be ignored\\).*",
               info = "Should not process T as not in config")
expect_inherits(prep2, "data.frame")
expect_identical(dim(prep2), c(200L, 6L))
expect_identical(names(prep2), c("datetime", "study", "home", "room", "CO2", "RH"))
rm(prep2)
rm(config2)



# -------------------------------------------------------------------
# Create annex object
# -------------------------------------------------------------------
expect_silent(annex_df <- annex(T + RH + CO2 ~ datetime | study + home + room, data = prep, tz = "CET"),
              info = "Create annex object")
expect_true("annex_df" %in% ls())
expect_inherits(annex_df, "annex",      info = "Checking class of annex object")
expect_inherits(annex_df, "data.frame", info = "Checking class of annex object")
expect_identical(dim(annex_df), c(200L, 10L), info = "Dimension of annex object")

# Adds a series of new columns, check if we got what we want
expect_identical(names(annex_df),
                 c("datetime", "study", "home", "room", "year", "month", "tod", "T", "RH", "CO2"),
                 info = "Checking order and existence of variable names")

expect_inherits(annex_df$datetime, "POSIXct")
expect_true(all(sapply(subset(annex_df, select = c(study, home, room)), class) == "character"))
expect_true(all(sapply(subset(annex_df, select = c(year, month, tod)), class) == "factor"))
expect_true(all(sapply(subset(annex_df, select = c(T, RH, CO2)), class) == "numeric"))
expect_true(all(annex_df$year == 2023))
expect_true(all(annex_df$month %in% c(7, 12)))

# We've constructed very specific times above (data$timeOfLoggingInUTC)
# such that we can check if the time zone is handled correctly.
# data$timeOfLoggingInUTC contains observations for summer 05:30, 06:00, ...
# which should fall into tod = "07-23" as we handle the data in CET
# (Central Europe Time) which, during summer, is +2 ahead of UTC (+1 in winter).
#
# If it worked as expected all obervations for 2023-07-01 should fall into 07-23,
# all those for 2023-12-24 into 23-07.
expect_true(all(subset(annex_df, as.Date(annex_df$datetime) == as.Date("2023-07-01"))$tod == "07-23"),
            info = "Checking time zone handling/conversion")
expect_true(all(subset(annex_df, as.Date(annex_df$datetime) != as.Date("2023-07-01"))$tod == "23-07"),
            info = "Checking time zone handling/conversion")
                
# The observations should be unchanged and all in 10:15 or NA
tmp <- unlist(subset(annex_df, select = c(CO2, T, RH)))
tmp <- unique(sort(round(tmp, 10), , na.last = TRUE))
expect_equal(tmp, c(seq(10, length.out = N, by = 0.5), NA))




# -------------------------------------------------------------------
# Calculating statistics
# -------------------------------------------------------------------
expect_silent(stats <- annex_stats(annex_df))

# My CO2 values are all below lower bound, check if identified properly
expect_true(all(subset(stats, variable == "CO2")$quality_lower == 100),
            info = "Lower bound violation for CO2")
expect_true(all(subset(stats, variable != "CO2")$quality_upper == 0),
            info = "No violation for RH + T")
expect_true(all(stats$quality_upper == 0),
            info = "No upper bound violation for all the data")

# Interval (auto-detected), here the most important is the Median
# as it is the one we use to calculate the number of expected values.
expect_true(all(stats$interval_Min == 10 * 60))
expect_true(all(stats$interval_Q1 == 10 * 60))
expect_true(all(stats$interval_Median == 10 * 60))

expect_true(all(stats$N   == 20), info = "We have 20 observations for each row")
expect_true(all(stats$NAs ==  0), info = "No missing values")
expect_true(all(stats$NAs ==  0), info = "No missing values")

# For 07-23 (16 hours) we expect Nestim == 32 as our interval is 30 min (1800s)
# For 23-07 (8 hours) we expect Nestim == 16
# For all (24 hours) we expect Nestim = 48
expect_true(all(subset(stats, tod == "07-23")$Nestim == 16 * 6))
expect_true(all(subset(stats, tod == "23-07")$Nestim ==  8 * 6))
expect_true(all(subset(stats, tod ==  "all")$Nestim  == 24 * 6))

# Due to construction, the rest of the stats is identical
# for all variables and all based on 10:15. E.g, the 
# Mean == mean(10:15) == 12.5. Let's test if the calculations
# are correct
tmp <- seq(10, length.out = N, by = 0.5)
expect_equal(stats$Mean, rep(mean(tmp), nrow(stats)),
             tolerance = 0.001,
             info = "Checking calculated mean")
expect_equal(stats$Sd, rep(sd(tmp), nrow(stats)),
             tolerance = 0.001,
             info = "Checking calculated standard deviation")

qtiles <- 100 * sort(c(seq(0, 1, by = 0.01), 0.005, 0.025, 0.975, 0.995))
for (x in qtiles) {
    n <- if (round(x - round(x), 2) == 0) sprintf("p%02.0f", x) else sprintf("p%04.1f", x)
    expect_equivalent(stats[[n]], rep(quantile(tmp, x / 100), nrow(stats)),
                 tolerance = 0.001, info = "Checking calculated percentiles")
}




# -------------------------------------------------------------------
# Write statistics to disc
# -------------------------------------------------------------------
tmpfile <- tempfile(fileext = ".xlsx")
expect_silent(annex_write_stats(stats, tmpfile, user = 999),
              info = "Writing statistics to XLSX")
expect_error(annex_write_stats(stats, tmpfile, user = 999, overwrite = FALSE),
             info = "Testing error as overwrite = FALSE and file exists")
expect_error(annex_write_stats(stats, tmpfile, user = 999, overwrite = TRUE),
             info = "Testing overwrite = TRUE")



