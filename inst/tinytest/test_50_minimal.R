

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

# Serves as our data set; set up in a very specific way to check if the
# `annex_stats` calcs are fine and the mapping with the config file works as
# expected. Time: Four periods, once at night and once during the day (local
# time) on two differnt days in two different years each with an interval of 1
# min to check the interval/Nestim guesses). Each period has exactely N = 30
# (we need >= 30) measurements which will be generated below to thest the
# calculation of the statistics. The second period is also set in a way
# (assuming UTC) to check if the 'tz' conversion works as expected; summer
# time; UTC -> CEST +2h
N <- 61
tms <- c(as.POSIXct("2022-12-24 00:00:00", tz = "UTC") + (seq_len(N) - 1) * 1 * 60,
         as.POSIXct("2022-07-01 05:30:00", tz = "UTC") + (seq_len(N) - 1) * 1 * 60,
         as.POSIXct("2023-12-24 00:00:00", tz = "UTC") + (seq_len(N) - 1) * 1 * 60,
         as.POSIXct("2023-07-01 05:30:00", tz = "UTC") + (seq_len(N) - 1) * 1 * 60)

# All the data we have will be 
data <- data.frame(timeOfLoggingInUTC = tms)
for (i in seq_len(nrow(config))) {
    tmp    <- as.list(config[i, ]) # Convert to list for convenience
    if (tmp$variable == "datetime") next # Skipping timeOfLoggingInUTC

    # Generate some data; always the "same" (uniform sequence from
    # seq(10, length.out = N, by = 0.5) such that we can
    # easily check the calculations later.
    #
    # For CO2 we are going with seq(0.01, 0.03) % and
    # seq(100, 300) ppm which is below our lower bound but
    # allows us to check our lower bound violation calculation.
    if (config$variable[i] == "CO2" & config$unit[i] == "%") {
        # -> percent to ppm
        values <- rep(seq(0.01, 0.03, length.out = N), 2)
    } else if (config$variable[i] == "CO2") {
        # ppm
        values <- rep(seq(100, 300, length.out = N), 2)
    } else {
        values <- rep(seq(10, length.out = N, by = 0.5), 2)
        if (tmp$unit == "K") values <- values + 273.15
        if (tmp$unit == "%" & tmp$variable == "CO2") values <- values / 1e4
    }

    data[[tmp$column]] <- values
}
summary(data)

# Setting the data for the room = LAU columns to NA.
# Once numeric NA, once logical NA, once character NA to check
# the `annex_prepare` checks.
data[["abcd0301-home9_co2"]]  <- NA_real_
data[["abcd0302-home9_temp"]] <- NA
data[["abcd0303-home9_RH"]]   <- NA_character_
# Store the data for room = LAU variable = Radon as character,
# This should cause an error in annex_prepare.
data[["abcd0304-home9_Radon"]] <- as.character(data[["abcd0304-home9_Radon"]])

head(data)


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
## 1220 = 5 unique (room:home) * 4 periods * 61 observations
expect_identical(dim(prep), c(1220L, 7L))
expect_identical(names(prep), c("datetime", "study", "home", "room", "CO2", "T", "RH"))
expect_inherits(prep$datetime, "POSIXct")
expect_true(all(sapply(subset(prep, select = c(study, room, home)), class) == "character"))
expect_true(all(sapply(subset(prep, select = c(CO2, T, RH)), class) == "numeric"))

# We constructed 'data' in a way that we only expect values
# 10:15 and NA. Check if TRUE.
tmp <- unlist(subset(prep, select = c(T, RH)))
tmp <- unique(sort(round(tmp, 10), , na.last = TRUE))
expect_equal(tmp, c(seq(10, length.out = N, by = 0.5), NA))

# So far, the units are in % and ppm, thus we need
# to compare against 0.04:0.10 and 0.04:0.10/1e5 here! 
# This is all below the lower limit, but that's what we test later.
tmp <- unlist(subset(prep, select = CO2))
tmp <- round(tmp, 5) # five digits
tmp <- unique(sort(tmp, na.last = TRUE))
expect_equal(tmp, c(seq(100, 300, length.out = N), NA), tol = 1e-5)

# -------------------------------------------------------------------
# What if `config` does not contain all the information?
# Drop all temperature varaibles from `config`
# -------------------------------------------------------------------
config2 <- subset(config, variable != "T" & variable != "Radon")
expect_message(prep2 <- annex_prepare(data, config = config2),
               pattern = ".*Columns in `x` not in `config` \\(will be ignored\\).*",
               info = "Should not process T as not in config")
expect_inherits(prep2, "data.frame")
expect_identical(dim(prep2), c(1220L, 6L))
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
expect_identical(dim(annex_df), c(1220L, 10L), info = "Dimension of annex object")

# Adds a series of new columns, check if we got what we want
expect_identical(names(annex_df),
                 c("datetime", "study", "home", "room", "year", "month", "tod", "T", "RH", "CO2"),
                 info = "Checking order and existence of variable names")

expect_inherits(annex_df$datetime, "POSIXct")
expect_true(all(sapply(subset(annex_df, select = c(study, home, room)), class) == "character"))
expect_true(all(sapply(subset(annex_df, select = c(year, month, tod)), class) == "factor"))
expect_true(all(sapply(subset(annex_df, select = c(T, RH, CO2)), class) == "numeric"))
expect_true(all(annex_df$year == 2022 | annex_df$year == 2023))
expect_identical(sum(annex_df$year == 2022), 610L)
expect_identical(sum(annex_df$year == 2023), 610L)
expect_true(all(annex_df$month %in% c(7, 12)))

# We've constructed very specific times above (data$timeOfLoggingInUTC)
# such that we can check if the time zone is handled correctly.
# data$timeOfLoggingInUTC contains observations for summer 05:30, 06:00, ...
# which should fall into tod = "07-23" as we handle the data in CET
# (Central Europe Time) which, during summer, is +2 ahead of UTC (+1 in winter).
#
# If it worked as expected all obervations for 202*-07-01 should fall into 07-23,
# all those for 202*-12-24 into 23-07.
idx <- as.Date(annex_df$datetime) %in% as.Date(c("2022-07-01", "2023-07-01"))
expect_true(all(annex_df$tod[idx] == "07-23"),
            info = "Checking time zone handling/conversion")
idx <- as.Date(annex_df$datetime) %in% as.Date(c("2022-12-24", "2023-12-24"))
expect_true(all(annex_df$tod[idx] == "23-07"),
            info = "Checking time zone handling/conversion")
                
# The observations should be unchanged and all in 10:15 or NA
tmp <- unlist(subset(annex_df, select = c(T, RH)))
tmp <- unique(sort(round(tmp, 10), , na.last = TRUE))
expect_equal(tmp, c(seq(10, length.out = N, by = 0.5), NA))


tmp <- unlist(subset(annex_df, select = CO2))
tmp <- round(tmp, 5) # five digits
tmp <- unique(sort(round(tmp, 10), na.last = TRUE))
expect_equal(tmp, c(seq(100, 300, length.out = N), NA), tol = 1e-5)



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
expect_true(all(stats$interval_Min    == 1 * 60))
expect_true(all(stats$interval_Q1     == 1 * 60))
expect_true(all(stats$interval_Median == 1 * 60))

# Checking where year, month, tod == "all" and sum up the occurrence.
# - No 'all' -> 61 observations
# - 1 'all' -> still N = 61 observations
# - 2 'all' -> N = 122 observations as we combine pereiods
# - 3 'all' -> N = 244 as we combine all data (4 * 61)
count <- rowSums(subset(stats, select = c(year, month, tod)) == "all")
expect_true(all(count %in% seq.int(0, 3)))
expect_true(all(stats$N %in% (c(1L, 2L, 4L) * 61L)))
expect_true(all(stats$N[count  < 2] ==     61), info = "N = 61")
expect_true(all(stats$N[count == 2] == 2 * 61), info = "N = 122")
expect_true(all(stats$N[count == 3] == 4 * 61), info = "N = 244")

# No missing values
expect_true(all(stats$NAs ==   0), info = "No missing values")

# For 07-23 (16 hours) we expect Nestim == 32 as our interval is 30 min (1800s)
# For 23-07 (8 hours) we expect Nestim == 16
# For all (24 hours) we expect Nestim = 48
# Only valid for count < 2 (where only one of year, month, tod == "all")
expect_true(all(subset(stats, count < 2 & tod == "07-23")$Nestim == 16 * 60)) # 1/minute
expect_true(all(subset(stats, count < 2 & tod == "23-07")$Nestim ==  8 * 60)) # 1/minute
# I do not check Nestim for the rest and hope it works :)

# Warning: Only testing rows where maximum 1 of year, month, tod is == "all"
# Due to construction, the rest of the stats is identical
# for all variables == T and all based on 10:15. E.g, the 
# Mean == mean(10:15) == 12.5. Let's test if the calculations
# are correct
count <- rowSums(subset(stats, select = c(year, month, tod)) == "all")
tmp_T <- seq(10, length.out = N, by = 0.5)
idx_T <- which(stats$variable == "T" & count < 2)
expect_equal(stats$Mean[idx_T], rep(mean(tmp_T), length(idx_T)),
             tolerance = 0.001,
             info = "Checking calculated mean T")
expect_equal(stats$Sd[idx_T], rep(sd(tmp_T), length(idx_T)),
             tolerance = 0.01,
             info = "Checking calculated standard deviation T")

qtiles <- 100 * sort(c(seq(0, 1, by = 0.01), 0.005, 0.025, 0.975, 0.995))
for (x in qtiles) {
    n <- if (round(x - round(x), 2) == 0) sprintf("p%02.0f", x) else sprintf("p%04.1f", x)
    expect_equivalent(stats[[n]][idx_T], rep(quantile(tmp_T, x / 100), length(idx_T)),
                 tolerance = 0.001, info = "Checking calculated percentiles T")
}

# Same for variable == CO2 but with seq(100, 300)
tmp_CO2 <- seq(100, 300, length.out = N)
idx_CO2 <- which(stats$variable == "CO2" & count < 2)
expect_equal(stats$Mean[idx_CO2], rep(mean(tmp_CO2), length(idx_CO2)),
             tolerance = 0.001,
             info = "Checking calculated mean CO2")
expect_equal(stats$Sd[idx_CO2], rep(sd(tmp_CO2), length(idx_CO2)),
             tolerance = 0.001,
             info = "Checking calculated standard deviation CO2")

qtiles <- 100 * sort(c(seq(0, 1, by = 0.01), 0.005, 0.025, 0.975, 0.995))
for (x in qtiles) {
    n <- if (round(x - round(x), 2) == 0) sprintf("p%02.0f", x) else sprintf("p%04.1f", x)
    expect_equivalent(stats[[n]][idx_CO2], rep(quantile(tmp_CO2, x / 100), length(idx_CO2)),
                 tolerance = 0.001, info = "Checking calculated percentiles CO2")
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

# Validation check (STAT only)
#expect_false(annex:::annex_validate_sheet_STAT(tmpfile, user = 999))



