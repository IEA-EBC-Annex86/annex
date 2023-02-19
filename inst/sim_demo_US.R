# -----------------------------------------------------------
# Small script so simulate some demo data in non SI units
# for testing the conversion.
#
# Create 'data/demo_US.xlsx'
# -----------------------------------------------------------

library("zoo")
library("openxlsx")

rm(list = objects())
OUTFILE = "demos/demo_US.xlsx"

if (file.exists(OUTFILE))
    stop("Output file `", OUTFILE, "` exists; I don't run the script again")

# -----------------------------------------------------------
# 1. Create time vector; we will later "shoot shome holes" in it.
# -----------------------------------------------------------
index <- seq(as.POSIXct("2028-12-28 12:30", tz = "UTC"),
             as.POSIXct("2029-01-03 06:20", tz = "UTC"),
             by = 600)

#OlsonNames()
# Target time zone (used for storing the data and calculating time of day)
target_tz <- "US/Central"
#print(head(index))
#print(format(head(index), "%d-%m-%Y %H:%M", tz = target_tz))

# -----------------------------------------------------------
# 2. Simulating four different variables
# -----------------------------------------------------------
# T_inside:    Mimiking room temperature (inside)
# RH_inside:   Mimiking some kind of relative humidity; inside
# T_EXH:       Temperature of an exhaust plume for demonstration
# PRESSURE:    Air pressure

set.seed(6020)
todrad    <- as.integer(format(index, "%H%M%S", tz = target_tz)) / 120000 * pi
T_inside  <- sin(todrad - pi/3) * 4 + 20 + rnorm(length(todrad), mean = 0, sd = 0.1)
RH_inside <- 110 - rpois(length(todrad), lambda = sin(todrad) + 3) * 7
T_EXH     <- ((sin(todrad + pi/3) + 1) / 2)^3 * 20 + 40 + rnorm(length(todrad), mean = 0, sd = 0.2)
PRESSURE  <- cumsum(2 * rnorm(length(todrad))) + 900

# This is all in 'annex standard units'
data <- zoo(cbind(T_inside, RH_inside, T_EXH, PRESSURE), index)


# -----------------------------------------------------------
# 3. Shooting a few holes into the time series
# -----------------------------------------------------------
idx1 <- which(index(data) > as.POSIXct("2028-12-30 11:00") &
              index(data) < as.POSIXct("2028-12-31 02:00"))
idx2 <- which(index(data) > as.POSIXct("2029-01-02 03:30") &
              index(data) < as.POSIXct("2029-01-02 07:15"))
data <- data[-c(idx1, idx2), ]

idx  <- which(index(data) > as.POSIXct("2028-12-30 11:00") &
              index(data) < as.POSIXct("2028-12-31 03:20"))
data$T_inside[idx] <- NA
idx  <- which(index(data) > as.POSIXct("2029-01-01 22:00") &
              index(data) < as.POSIXct("2029-01-02 07:20"))
data$RH_inside[idx] <- NA

data$PRESSURE[index(data) > as.POSIXct("2028-12-31 12:00")] <- NA


# Converting data
# T_inside: to Fahrenheit
# RH_inside: unitless
# PESSURE: mmHg
usdata <- transform(data,
                    T_inside  = T_inside * 1.8 + 32,
                    RH_inside = RH_inside / 100,
                    PRESSURE  = round(PRESSURE * 0.75006156130264, 4))

#plot(data)
newnames <- c("temperature kitchen", "rel hum kitchen", "exhaust temperature [Cels]", "air pressure")
final <- data.frame(loggertime = format(index(usdata), "%m/%d/%Y %H:%M:%S", tz = target_tz),
                    setNames(as.data.frame(usdata), newnames))

meta <- data.frame(column = names(final),
                   variable = c("datetime", "T", "RH", "T", "Pressure"),
                   study  = "demo_US",
                   home   = "top_flat",
                   room   = c(NA, "KIT", "KIT", "EHA", "AMB"),
                   unit   = c(NA, "F", "-", "C", "mmHg"))

write.xlsx(list("meta_config" = meta,
                "airquality_data" = final), OUTFILE)
