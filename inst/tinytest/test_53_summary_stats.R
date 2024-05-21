

if (interactive()) {
    library("tinytest")
    library("annex")
}

# -------------------------------------------------------------------
# Setting up our config object
# -------------------------------------------------------------------
config_txt <- "
column,               variable,    study,    unit,     home,     room
timeOfMeasurement,    datetime,    NA,       NA,       NA,       NA
foo,                  CO2,         Reto,     %,        H3,       BAT1
"

expect_silent(config <- read.csv(text = config_txt, strip.white = TRUE))

# -------------------------------------------------------------------
# (1) Simulating data; only daytime, multiple days, same month, same year
# -------------------------------------------------------------------
dt <- as.POSIXct("2024-01-02 10:00") + 0:9 * 3600
dt <- unlist(lapply(0:5, function(x, dt) dt + x * 86400, dt = dt))
dt <- as.POSIXct(dt, tz = "Europe/Vienna")

# Random data (not important; not tested)
raw <- data.frame("timeOfMeasurement" = dt,
                  "foo" = round(runif(length(dt), 350, 1000)))

prepared_df <- annex_prepare(raw, config)
annex_df    <- annex(CO2 ~ datetime | study + home + room, prepared_df, tz = "Europe/Vienna")
stats       <- annex_stats(annex_df)
head(stats[, 1:10])

# One room, one variable, one month, one year, only day-time,
# so `stats` must contain one row + overall statistics (= 2 rows)
expect_identical(nrow(stats), 2L,
                 info = "(1) Number of rows in stats object.")
expect_identical(as.character(stats$year), c("2024", "all"),
                 info = "(1) Checking `year` in stats object.")
expect_identical(as.character(stats$month), c("1", "all"),
                 info = "(1) Checking `month` in stats object.")
expect_identical(as.character(stats$tod), c("07-23", "all"),
                 info = "(1) Checking `tod` in stats object.")

# Cleaning up
rm(dt, raw, prepared_df, annex_df, stats)

# -------------------------------------------------------------------
# (2) Simulating data; only daytime, multiple days, MULTIPLE (TWO) MONTHS, same year
# -------------------------------------------------------------------
dt <- as.POSIXct("2024-01-02 10:00") + 0:9 * 3600
dt <- unlist(lapply(0:1, function(x, dt) dt + (31 * x) * 86400, dt = dt))
dt <- as.POSIXct(dt, tz = "Europe/Vienna")

# Random data (not important; not tested)
raw <- data.frame("timeOfMeasurement" = dt,
                  "foo" = round(runif(length(dt), 350, 1000)))

prepared_df <- annex_prepare(raw, config)
annex_df    <- annex(CO2 ~ datetime | study + home + room, prepared_df, tz = "Europe/Vienna")
stats       <- annex_stats(annex_df)
head(stats[, 1:10])

# One room, one varaible, only day-time, but we have observations
# for two months; thus expecting statistics for month 1, 2, and "all"
# (over both months) + the overall statistics = 4 rows of data
expect_identical(nrow(stats), 4L,
                 info = "(2) Number of rows in stats object.")
expect_identical(as.character(stats$year), rep(c("2024", "all"), c(3L, 1L)),
                 info = "(2) Checking `year` in stats object.")
expect_identical(as.character(stats$month), c("all", "1", "2", "all"),
                 info = "(2) Checking `month` in stats object.")
expect_identical(as.character(stats$tod), rep(c("07-23", "all"), c(3L, 1L)),
                 info = "(2) Checking `tod` in stats object.")

# Cleaning up
rm(dt, raw, prepared_df, annex_df, stats)


# -------------------------------------------------------------------
# (3) Simulating data; only daytime, multiple days, same month (Jan), MULTIPLE YEARS
# -------------------------------------------------------------------
dt <- c(as.POSIXct("2024-01-02 10:00") + 0:9 * 3600,
        as.POSIXct("2025-01-02 10:00") + 0:9 * 3600)

# Random data (not important; not tested)
raw <- data.frame("timeOfMeasurement" = dt,
                  "foo" = round(runif(length(dt), 350, 1000)))

prepared_df <- annex_prepare(raw, config)
annex_df    <- annex(CO2 ~ datetime | study + home + room, prepared_df, tz = "Europe/Vienna")

stats       <- annex_stats(annex_df)
head(stats[, 1:10])

# One room, one varaible, only day-time, but we have observations,
# same month, but we have multiple years. Thus expecting statistics
# for 2024, 2025 + the overall statistics = 3 rows
expect_identical(nrow(stats), 3L,
                 info = "(3) Number of rows in stats object.")
expect_identical(as.character(stats$year), c("2024", "2025", "all"),
                 info = "(3) Checking `year` in stats object.")
expect_identical(as.character(stats$month), rep(c("1", "all"), c(2L, 1L)),
                 info = "(3) Checking `month` in stats object.")
expect_identical(as.character(stats$tod), rep(c("07-23", "all"), c(2L, 1L)),
                 info = "(3) Checking `tod` in stats object.")

# Cleaning up
rm(dt, raw, prepared_df, annex_df, stats)


# -------------------------------------------------------------------
# (4) Simulating data; day-time and night-time, one month, one year
# -------------------------------------------------------------------
dt <- as.POSIXct("2024-01-02 00:00") + 0:23 * 3600

# Random data (not important; not tested)
raw <- data.frame("timeOfMeasurement" = dt,
                  "foo" = round(runif(length(dt), 350, 1000)))

prepared_df <- annex_prepare(raw, config)
annex_df    <- annex(CO2 ~ datetime | study + home + room, prepared_df, tz = "Europe/Vienna")

stats       <- annex_stats(annex_df)
head(stats[, 1:10])

# One room, one variable, measurements for day-time and night-time,
# so we expect tod = 'all' + overall statistics = 4 rows
expect_identical(nrow(stats), 4L,
                 info = "(4) Number of rows in stats object.")
expect_identical(as.character(stats$year), rep(c("2024", "all"), c(3L, 1L)),
                 info = "(4) Checking `year` in stats object.")
expect_identical(as.character(stats$month), rep(c("1", "all"), c(3L, 1L)),
                 info = "(4) Checking `month` in stats object.")
expect_identical(as.character(stats$tod), c("all", "07-23", "23-07", "all"),
                 info = "(4) Checking `tod` in stats object.")

# Cleaning up
rm(dt, raw, prepared_df, annex_df, stats)

####devtools::load_all("../../"); stats       <- annex_stats(annex_df); head(stats[, 1:10])
