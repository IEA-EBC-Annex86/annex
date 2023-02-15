


# We always convert to Celsius
convert_unit_T <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("C", "K", "F"))
        stop("Don't know how to convert temperature from \"", from, "\" to Celsius")

    # Kelvin -> Celsius
    if (from == "K") {
        x <- x - 273.15
    } else if (from == "F") {
        x <- round((x - 32.) / 1.8, 2)
    }
    return(x)
}

# Relative humidity: always convert to percent
convert_unit_RH <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("%", "-"))
        stop("Don't know how to convert RH from \"", from, "\" to percent")

    # Unitless to percent
    return(if (from == "-") 100 * x else x)
}


# Pressure: always to hPa
convert_unit_Pressure <- function(x, from) {
    stopifnot("input vector must be numeric" = is.numeric(x))
    stopifnot("argument \"from\" must be character" = is.character(from))
    stopifnot("argument \"from\" must be length 1" = length(from) == 1)
    if (!from %in% c("hPa", "mmHg"))
        stop("Don't know how to convert Pressure from \"", from, "\" to hPa")

    # Unitless to percent
    return(if (from == "mmHg") x / 0.75006156130264 else x)
}


