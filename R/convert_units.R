# Set of functions to convert units for the variables in /inst/template/template.xlsx
# Reto Stauffer, Gabriel Rojas

# These are hidden functions (not intended to be used by the end user).
# For each variable which has defined units (or one fixed defined unit)
# such a conversion unit must exist as it is called when preparing
# the annex object (`annex_prepare()`) to ensure that the data is
# in the 'standardized annex unit'.

# CO2: convert to ppm
convert_unit_CO2 <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("ppm", "%"))
    stop("Don't know how to convert from \"", from, "\" to ppm")
  
  return(if (from == "%") x * 10000 else x)
}

# CO: convert to ug/m3
convert_unit_CO <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("ug/m3", "mg/m3", "ppm", "ppb"))
    stop("Don't know how to convert from \"", from, "\" to ug/m3")
  
  if (from == "mg/m3") {
    x <- x * 1000
  } else if (from == "ppm" | from == "ppb") {
    # conversion via ideal gas law: g/m3= ppm/1e6 * Molmass [g/mol]* assumed pressure for conversion / gas constant / assumed press for conversion
    x <- x * 28.01 * 101325 / 8.314 / 298.15
    x <- if (from == "ppb") round(x / 1000, 2) else round(x, 2)
  }
  return(x)
}

# HCHO: convert to ug/m3
convert_unit_HCHO <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("ug/m3", "mg/m3", "ppm", "ppb"))
    stop("Don't know how to convert from \"", from, "\" to ug/m3")
  
  if (from == "mg/m3") {
    x <- x * 1000
  } else if (from == "ppm" | from == "ppb") {
    # conversion via ideal gas law: g/m3= ppm/1e6 * Molmass [g/mol]* assumed pressure for conversion / gas constant / assumed press for conversion
    x <- x * 30.03 * 101325 / 8.314 / 298.15
    x <- if (from == "ppb") round(x / 1000, 2) else round(x, 2)
  }
  return(x)
}

# UFP: check units
convert_unit_UFP <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("#/cm3"))
    stop("Don't know how to convert from \"", from, "\" to #/cm3")
  
  return(x)
}


# O3: convert to ug/m3
convert_unit_O3 <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("ug/m3", "mg/m3", "ppm", "ppb"))
    stop("Don't know how to convert from \"", from, "\" to ug/m3")
  
  if (from == "mg/m3") {
    x <- x * 1000
  } else if (from == "ppm" | from == "ppb") {
    # conversion via ideal gas law: g/m3= ppm/1e6 * Molmass [g/mol]* assumed pressure for conversion / gas constant / assumed press for conversion
    x <- x * 48.0 * 101325 / 8.314 / 298.15
    x <- if (from == "ppb") round(x / 1000, 2) else round(x, 2)
  }
  return(x)
}

# Relative humidity: convert to percent
convert_unit_RH <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("%", "-"))
    stop("Don't know how to convert from \"", from, "\" to percent")
  
  return(if (from == "-") 100 * x else x)
}

# Radon: convert to Bq/m3
convert_unit_Radon <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("Bq/m3", "pCi/L"))
    stop("Don't know how to convert from \"", from, "\" to Bq/m3")
  # 1 pCi/L = 37 Bq/m3, from https://en.wikipedia.org/wiki/Radon
  return(if (from == "pCi/L") x * 37 else x)
}

# Pressure: convert to hPa
convert_unit_Pressure <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("hPa", "mmHg"))
        stop("Don't know how to convert from \"", from, "\" to hPa")

    return(if (from == "mmHg") x / 0.75006156130264 else x)
}

# Temperature: convert to Celsius
convert_unit_T <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("C", "K", "F"))
        stop("Don't know how to convert from \"", from, "\" to Celsius")

    if (from == "K") {
        x <- x - 273.15
    } else if (from == "F") {
        x <- round((x - 32.) / 1.8, 2)
    }
    return(x)
}


# --------------------------------------------------------------
# Some units have one (and only one) unit which can/must be
# set. This 'convert' function simply checks that the unit
# is set correctly and returns 'x' as is.
# Used for Fungi, Ions, SolRad.
# This function shall never be used directly!
# --------------------------------------------------------------
convert_unit_keepasis <- function(x, from, mustbe) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  stopifnot(is.character(mustbe), length(mustbe) == 1)
  if (from != mustbe)
    stop("Don't know how to convert from \"", from, "\" to ", mustbe)
  
  return(x)
}

# Fungi: convert to CFU/m3
convert_unit_Fungi <- function(x, from)
  convert_unit_keepasis(x, from, "CFU/m3")

# Ions: 1/cm3; no conversion
convert_unit_Ions <- function(x, from)
  convert_unit_keepasis(x, from, "1/cm3")

# SolRad: W/m2; no conversion
convert_unit_SolRad <- function(x, from)
  convert_unit_keepasis(x, from, "W/m2")

# --------------------------------------------------------------
# A series of variables convert from/to "milligrams per cubic
# meter" (mg/m3) to "micrograms per cubic meter" (um/m3).
# This is a generic function taking over this job used
# for NO2, NOx, TVOC, PM* etc (see below)
# This function shall never be used directly!
# --------------------------------------------------------------
convert_unit_ugm3 <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("ug/m3", "mg/m3"))
    stop("Don't know how to convert from \"", from, "\" to ug/m3")
  return(if (from == "mg/m3") x * 1000 else x)
}
  
# NO2: convert to ug/m3
convert_unit_NO2 <- function(x, from)
    convert_unit_ugm3(x, from)

# NOx: convert to ug/m3
convert_unit_NOx <- function(x, from)
    convert_unit_ugm3(x, from)

# TVOC: convert to ug/m3
convert_unit_TVOC <- function(x, from)
    convert_unit_ugm3(x, from)

# VOC: convert to ug/m3
convert_unit_VOC <- function(x, from)
    convert_unit_ugm3(x, from)

# PM1: convert to ug/m3
convert_unit_PM1 <- function(x, from)
    convert_unit_ugm3(x, from)

# PM25: convert to ug/m3
convert_unit_PM25 <- function(x, from)
    convert_unit_ugm3(x, from)

# PM10: convert to ug/m3
convert_unit_PM10 <- function(x, from)
    convert_unit_ugm3(x, from)


# --------------------------------------------------------------
# Converting airflow from "cubic feet per minute" or
# "cubic meter per hour" to "liters per second" (default unit)
# This function shall never be used directly!
# --------------------------------------------------------------
convert_unit_Flow <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("m3/h", "cfm"))
    stop("Don't know how to convert from \"", from, "\" to l/s")
  
  if (from == "m3/h") {
      x <- x / 3.6 ## x * 1000 l/m3 / 3600 s/h
  } else {
      x <- x * 0.471947
  }
  return(round(x, 3))
}








