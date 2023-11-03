

if (interactive()) {
    library("tinytest")
    library("annex")
}

# Test data set with duplicated entries for testing
# the feature on how to deal with such data; a feature
# added in annex version 0.2-9+

# Setting up the data set (prepared df; manually)
data <- "
datetime;         study; home; room; CO2; T;      note
2023-11-03 15:00; test;  test; BED1;  500;  25.5;  15:00 has no duplicate
2023-11-03 15:00; test;  test; BED2;  400;  18.2;  15:00 has no duplicate
2023-11-03 16:00; test;  test; BED1;  520;  24.8;  ---
2023-11-03 16:00; test;  test; BED2;  434;  18.1;  ---
2023-11-03 16:00; test;  test; BED1;   NA;    NA;  duplicated 16:00, all missing
2023-11-03 16:00; test;  test; BED2;   NA;    NA;  duplicated 16:00, all missing
2023-11-03 17:00; test;  test; BED1;  540;  24.7;  ---
2023-11-03 17:00; test;  test; BED2;  427;  18.0;  ---
2023-11-03 17:00; test;  test; BED1;   NA;  24.7;  duplicated 17:00, partially missing
2023-11-03 17:00; test;  test; BED2;  427;    NA;  duplicated 17:00, partially missing
2023-11-03 18:00; test;  test; BED1;  535;  25.1;  ---
2023-11-03 18:00; test;  test; BED2;  413;  18.3;  ---
2023-11-03 18:00; test;  test; BED1; -535; -25.1;  duplicated 18:00, different values (neg)
2023-11-03 18:00; test;  test; BED2; -413; -18.3;  duplicated 18:00, different values (neg)
"

data <- read.csv(text = data, sep = ";", header = TRUE, strip.white = TRUE)
data$datetime <- as.POSIXct(data$datetime, tz = "UTC")


# -----------------------------------------------------------------
# Testing default use
# -----------------------------------------------------------------
expect_warning(annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET"),
               pattern = ".*variable 'datetime' contains duplicated time stamps for.*room = BED1.*",
               info = "Checking warning for BED1")
expect_warning(annex_df <- annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET"),
               pattern = ".*variable 'datetime' contains duplicated time stamps for.*room = BED2.*",
               info = "Checking warning for BED2")
expect_true("annex_df" %in% ls()) # Make sure object has been created

# Checking default
expect_warning(tmp <- annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET",
                            duplicate.action = NULL))
expect_equal(annex_df, tmp)
rm(tmp)



# -----------------------------------------------------------------
# Based on annex_df with duplicate.action = NULL; checking stats
# -----------------------------------------------------------------
expect_warning(stats <- annex_stats(annex_df),
               pattern = "You have 3 duplicated entries in datetime for.*BED1.*")
expect_warning(stats <- annex_stats(annex_df),
               pattern = "You have 3 duplicated entries in datetime for.*BED2.*")
expect_true("stats" %in% ls()) # Make sure object has been created


expect_identical(dim(stats), c(12L, 127L), info = "Statistics dimension check")
expect_true(all(stats$N == 7),
            info = "Check if N == 7 (counts duplicates)")
expect_true(all(stats$NAs == 1 | stats$NAs == 2),
            info = "Check count for number of duplicates")

# For tod == all we should get the overall
# mean(..., na.rm = TRUE) for each of the two rooms. 
b1 <- subset(data, room == "BED1")
expect_equal(subset(stats, room == "BED1" & tod == "all" & variable == "T")$Mean,
             mean(b1$T, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED1" & tod == "all" & variable == "T")$Sd,
             sd(b1$T, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED1" & tod == "all" & variable == "CO2")$Mean,
             mean(b1$CO2, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED1" & tod == "all" & variable == "CO2")$Sd,
             sd(b1$CO2, na.rm = TRUE), tolerance = 0.001)

b2 <- subset(data, room == "BED2")
expect_equal(subset(stats, room == "BED2" & tod == "all" & variable == "T")$Mean,
             mean(b2$T, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED2" & tod == "all" & variable == "T")$Sd,
             sd(b2$T, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED2" & tod == "all" & variable == "CO2")$Mean,
             mean(b2$CO2, na.rm = TRUE), tolerance = 0.001)
expect_equal(subset(stats, room == "BED2" & tod == "all" & variable == "CO2")$Sd,
             sd(b2$CO2, na.rm = TRUE), tolerance = 0.001)

rm(annex_df)
rm(stats)
rm(b1)
rm(b2)

# -----------------------------------------------------------------
# Checking duplicate.action = min
# -----------------------------------------------------------------
expect_silent(annex_df <- annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET",
                                duplicate.action = min))
expect_true("annex_df" %in% ls()) # Make sure object has been created

expect_silent(stats <- annex_stats(annex_df))
expect_true("stats" %in% ls()) # Make sure object has been created

# We take the minmum in case we have duplicated entries.
# Thus, the lowest percentile (p00; absolute min) should now
# be negative in all cases.
expect_true(all(stats$p00 < 0),
            info = "Absolute minimum (p00) should now be negative")

# In addition, we should now always have N = 4 and NAs = 0 as
# we were able to remove all missing values.
expect_true(all(stats$N == 4), info = "Sample size after aggregating duplicates")
expect_true(all(stats$NAs == 0), info = "Missing values after aggregating duplicates")

rm(annex_df)
rm(stats)



# -----------------------------------------------------------------
# Expecting errors if the user function does not return
# numeric, or not a single numeric or Na
# -----------------------------------------------------------------
myfun <- function(x) return(c())
expect_error(annex_df <- annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET",
                                duplicate.action = myfun),
             pattern = ".*does not return a single numeric \\(or NA\\).*",
             info = "Checking error message for invalid function")

myfun <- identity
expect_error(annex_df <- annex(CO2 + T ~ datetime | study + room + home, data = data, tz = "CET",
                                duplicate.action = myfun),
             pattern = ".*does not return a single numeric \\(or NA\\).*",
             info = "Checking error message for invalid function")






