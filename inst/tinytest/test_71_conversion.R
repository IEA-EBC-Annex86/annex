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

# [0-1] to Percent
expect_equal(annex:::convert_unit_Pressure(600, from = "mmHg"),
             799.9343, tol = 1e-4,
             info = "Converting single value")
expect_equal(annex:::convert_unit_Pressure(c(NA, 600, NA), from = "mmHg"),
             c(NA_real_, 799.9343, NA_real_), tol = 1e-4,
             info = "Converting mixed missing/non missing")
expect_equal(annex:::convert_unit_Pressure(seq(500, 1000, by = 100), from = "mmHg"),
             c(666.6119, 799.9343, 933.2567, 1066.579, 1199.901, 1333.224), tol = 1e-4,
             info = "Converting range of values")






