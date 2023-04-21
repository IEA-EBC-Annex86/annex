# Set of functions to convert units for the variables in /inst/template/template.xlsx
# Reto Stauffer, Gabriel Rojas

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

# Generic function mg/m3 to ug/m3
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

# PM1: convert to ug/m3
convert_unit_PM1 <- function(x, from)
    convert_unit_ugm3(x, from)


# PM25: convert to ug/m3
convert_unit_PM25 <- function(x, from)
    convert_unit_ugm3(x, from)

# PM10: convert to ug/m3
convert_unit_PM10 <- function(x, from)
    convert_unit_ugm3(x, from)

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

# Fungi: convert to CFU/m3
convert_unit_Fungi <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("CFU/m3"))
    stop("Don't know how to convert from \"", from, "\" to CFU/m3")
  
  return(x)
}

# Ions: convert to 1/cm3
convert_unit_Ions <- function(x, from) {
  stopifnot("input vector must be numeric" = is.numeric(x))
  stopifnot("argument \"from\" must be character" = is.character(from))
  stopifnot("argument \"from\" must be length 1" = length(from) == 1)
  if (!from %in% c("1/cm3"))
    stop("Don't know how to convert from \"", from, "\" to 1/cm3")
  
  return(x)
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

# Pressure: convert to hPa
convert_unit_Pressure <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("hPa", "mmHg"))
        stop("Don't know how to convert from \"", from, "\" to hPa")

    return(if (from == "mmHg") x / 0.75006156130264 else x)
}


