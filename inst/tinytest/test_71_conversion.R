# -------------------------------------------------------
# Checking unit conversions.
# -------------------------------------------------------

if (interactive()) {
    library("tinytest")
    library("annex")
}


# -------------------------------------------------------
# Temperature conversion. Converts:
# - F (Fahrenheit) -> C (Celsius)
# - K (Kelvin) -> C (Celsius)
# -------------------------------------------------------
expect_error(annex:::convert_unit_T(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_T(32),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_T("foo", from = "F"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_T(TRUE, from = "F"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_T(32, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_T(32, from = rep("F", 2)),
             info = "Argument 'from' of length != 1")

expect_identical(annex:::convert_unit_T(NA_real_, from = "F"), NA_real_,
             info = "Converting single NA")

# Fahrenheit to Celsius
expect_identical(annex:::convert_unit_T(32, from = "F"),
             0,
             info = "Converting single value (Fahrenheit)")
expect_identical(annex:::convert_unit_T(c(NA, 32, NA), from = "F"),
             c(NA_real_, 0, NA_real_),
             info = "Converting mixed missing/non-missing values (Fahrenheit)")
expect_identical(annex:::convert_unit_T(c(-58, 32, 122, 212), from = "F"),
             seq(-50, 100, by = 50),
             info = "Converting range of values (Fahrenheit)")

# Kelvin to Celsius
expect_identical(annex:::convert_unit_T(273.15, from = "K"),
             0,
             info = "Converting single value (Kelvin)")
expect_identical(annex:::convert_unit_T(c(NA, 273.15, NA), from = "K"),
             c(NA_real_, 0, NA_real_),
             info = "Converting missing/non-missing values (Kelvin)")
expect_identical(annex:::convert_unit_T(seq(-50, 100, by = 50) + 273.15, from = "K"),
             seq(-50, 100, by = 50),
             info = "Converting range of values (Kelvin)")


# -------------------------------------------------------
# Relative humidity conversion. Converts:
# - "-" (unitless; [0-1]) -> Percent
# -------------------------------------------------------
expect_error(annex:::convert_unit_RH(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_RH(0.5),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_RH("foo", from = "-"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_RH(TRUE, from = "-"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_RH(0.5, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_RH(0.5, from = rep("-", 2)),
             info = "Argument 'from' of length != 1")

expect_identical(annex:::convert_unit_RH(NA_real_, from = "-"), NA_real_,
             info = "Converting single NA")

# [0-1] to Percent
expect_identical(annex:::convert_unit_RH(0.5, from = "-"),
             50,
             info = "Converting single value")
expect_identical(annex:::convert_unit_RH(c(NA, 0.5, NA), from = "-"),
             c(NA_real_, 50, NA_real_),
             info = "Converting mixed missing/non missing")
expect_identical(annex:::convert_unit_RH(seq(-0.5, 1.5, by = 0.1), from = "-"),
             seq(-0.5, 1.5, by = 0.1) * 1e2,
             info = "Converting range of values")


# -------------------------------------------------------
# Pressure conversion. Testing
# - mmHg (millimeter mercury) -> hPa (hectopascal)
# -------------------------------------------------------
expect_error(annex:::convert_unit_Pressure(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_Pressure(0.5),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_Pressure("foo", from = "mmHg"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_Pressure(TRUE, from = "mmHg"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_Pressure(600, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_Pressure(500, from = rep("-", 2)),
             info = "Argument 'from' of length != 1")

expect_identical(annex:::convert_unit_Pressure(NA_real_, from = "mmHg"), NA_real_,
             info = "Converting single NA")

expect_equal(annex:::convert_unit_Pressure(600, from = "mmHg"),
             799.9343, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_Pressure(c(NA, 600, NA), from = "mmHg"),
             c(NA_real_, 799.9343, NA_real_), tol = 1e-4,
             info = "Converting mixed missing/non missing")
expect_equal(annex:::convert_unit_Pressure(seq(500, 1000, by = 100), from = "mmHg"),
             c(666.6119, 799.9343, 933.2567, 1066.579, 1199.901, 1333.224), tol = 1e-4,
             info = "Converting range of values")


# -------------------------------------------------------
# CO2 Conversion
# -------------------------------------------------------
expect_error(annex:::convert_unit_CO2(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_CO2(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_CO2("foo", from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_CO2(TRUE, from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_CO2(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_CO2(50, from = rep("%", 2)),
             info = "Argument 'from' of length != 1")

expect_identical(annex:::convert_unit_CO2(NA_real_, from = "%"), NA_real_,
             info = "Converting single NA")

expect_equal(annex:::convert_unit_CO2(60, from = "ppm"),
             60, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_CO2(60, from = "%"),
             60 * 1e4, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_CO2(c(NA, 12.34, NA), from = "%"),
             c(NA_real_, 12.34 * 1e4, NA_real_), tol = 1e-4,
             info = "Converting mixed missing/non missing")
expect_equal(annex:::convert_unit_CO2(seq(50, 100, by = 10), from = "%"),
             c(50, 60, 70, 80, 90, 100) * 1e4,
             info = "Converting range of values")


# -------------------------------------------------------
# NO2/NOX/VOC/TVOC/PM1/PM25/PM10 all use the same
# conversion routine (convert_unit_ugm3)
# -------------------------------------------------------
expect_error(annex:::convert_unit_ugm3(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_ugm3(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_ugm3("foo", from = "mg/m3"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_ugm3(TRUE, from = "mg/m3"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_ugm3(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_ugm3(50, from = rep("mg/m3", 2)),
             info = "Argument 'from' of length != 1")

expect_identical(annex:::convert_unit_ugm3(NA_real_, from = "mg/m3"), NA_real_,
             info = "Converting single NA")

expect_equal(annex:::convert_unit_ugm3(60, from = "ug/m3"),
             60, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_ugm3(60, from = "mg/m3"),
             60000, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_ugm3(c(NA, 12.34, NA), from = "mg/m3"),
             c(NA_real_, 12340, NA_real_), tol = 1e-4,
             info = "Converting mixed missing/non missing")
expect_equal(annex:::convert_unit_ugm3(seq(50, 100, by = 10), from = "mg/m3"),
             c(50, 60, 70, 80, 90, 100) * 1e3,
             info = "Converting range of values")

# Compare NO2 and NOx conversion check as they both work the very same
x <- seq(0, 1000, length.out = 50)
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_NO2(x,  "mg/m3"),
             info = "Check that convert NO2")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_NOx(x, "mg/m3"),
             info = "Check that convert NOx")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_TVOC(x, "mg/m3"),
             info = "Check that convert TVOC")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_VOC(x, "mg/m3"),
             info = "Check that convert VOC")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_PM1(x, "mg/m3"),
             info = "Check that convert PM1")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_PM25(x, "mg/m3"),
             info = "Check that convert PM25")
expect_equal(annex:::convert_unit_ugm3(x, "mg/m3"),
             annex:::convert_unit_PM10(x, "mg/m3"),
             info = "Check that convert PM10")



# -------------------------------------------------------
# O3 Conversion
# -------------------------------------------------------
expect_error(annex:::convert_unit_O3(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_O3(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_O3("foo", from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_O3(TRUE, from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_O3(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_O3(50, from = rep("%", 2)),
             info = "Argument 'from' of length != 1")

expect_equal(annex:::convert_unit_O3(c(NA, 60), from = "ug/m3"),
             c(NA, 60), tol = 1e-4,
             info = "Converting single value from ug/m3 -> ug/m3")
expect_equal(annex:::convert_unit_O3(c(NA, 60), from = "mg/m3"),
             c(NA, 60000), tol = 1e-4,
             info = "Converting single value from mg/m3 -> ug/m3")
expect_equal(annex:::convert_unit_O3(c(NA, 60), from = "ppm"),
             c(NA, 117723.8), tol = 0, # Returned rounded to two digits
             info = "Converting single value from ppm -> ug/m3")
expect_equal(annex:::convert_unit_O3(c(NA, 60), from = "ppb"),
             c(NA, round(117723.8 / 1000, 2)), tol = 0, # Returned rounded to two digits
             info = "Converting single value from bbp -> ug/m3")



# -------------------------------------------------------
# CO Conversion
# -------------------------------------------------------
expect_error(annex:::convert_unit_CO(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_CO(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_CO("foo", from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_CO(TRUE, from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_CO(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_CO(50, from = rep("%", 2)),
             info = "Argument 'from' of length != 1")

expect_equal(annex:::convert_unit_CO(c(NA, 60), from = "ug/m3"),
             c(NA, 60), tol = 1e-4,
             info = "Converting single value from ug/m3 -> ug/m3")
expect_equal(annex:::convert_unit_CO(c(NA, 60), from = "mg/m3"),
             c(NA, 60000), tol = 1e-4,
             info = "Converting single value from mg/m3 -> ug/m3")
expect_equal(annex:::convert_unit_CO(c(NA, 60), from = "ppm"),
             c(NA, 68696.74), tol = 0, # Returned rounded to two digits
             info = "Converting single value from ppm -> ug/m3")
expect_equal(annex:::convert_unit_CO(c(NA, 60), from = "ppb"),
             c(NA, round(68696.74 / 1000, 2)), tol = 0, # Returned rounded to two digits
             info = "Converting single value from bbp -> ug/m3")


# -------------------------------------------------------
# HCHO Conversion
# -------------------------------------------------------
expect_error(annex:::convert_unit_HCHO(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_HCHO(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_HCHO("foo", from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_HCHO(TRUE, from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_HCHO(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_HCHO(50, from = rep("%", 2)),
             info = "Argument 'from' of length != 1")

expect_equal(annex:::convert_unit_HCHO(c(NA, 60), from = "ug/m3"),
             c(NA, 60), tol = 1e-4,
             info = "Converting single value from ug/m3 -> ug/m3")
expect_equal(annex:::convert_unit_HCHO(c(NA, 60), from = "mg/m3"),
             c(NA, 60000), tol = 1e-4,
             info = "Converting single value from mg/m3 -> ug/m3")
expect_equal(annex:::convert_unit_HCHO(c(NA, 60), from = "ppm"),
             c(NA, 73650.95), tol = 0, # Returned rounded to two digits
             info = "Converting single value from ppm -> ug/m3")
expect_equal(annex:::convert_unit_HCHO(c(NA, 60), from = "ppb"),
             c(NA, round(73650.95 / 1000, 2)), tol = 1e-4, # Returned rounted to two digits
             info = "Converting single value from bbp -> ug/m3")

# -------------------------------------------------------
# Radon Conversion
# -------------------------------------------------------
expect_error(annex:::convert_unit_Radon(),
             info = "Testing using no arguments")
expect_error(annex:::convert_unit_Radon(60),
             info = "Testing using no 'from' argument")
expect_error(annex:::convert_unit_Radon("foo", from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_Radon(TRUE, from = "%"),
             info = "Non-numeric input")
expect_error(annex:::convert_unit_Radon(60, from = "foo"),
             info = "Invalid value for 'from'")
expect_error(annex:::convert_unit_Radon(50, from = rep("%", 2)),
             info = "Argument 'from' of length != 1")

expect_equal(annex:::convert_unit_Radon(c(NA, 60), from = "Bq/m3"),
             c(NA, 60), tol = 1e-4,
             info = "Converting single value from Bq/m3 -> Bq/m3")
expect_equal(annex:::convert_unit_Radon(c(NA, 60), from = "pCi/L"),
             c(NA, 2220), tol = 1e-4,
             info = "Converting single value from pCi/L -> Bq/m3")













