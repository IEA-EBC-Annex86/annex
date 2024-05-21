# -------------------------------------------------------
# Checking function 'annex_check_config'
# -------------------------------------------------------

if (interactive()) {
    library("tinytest")
    library("annex")
}


# -------------------------------------------------------
# Testing helper function to get room and variable
# definition as well as checking for allowed room labels
# and variable names
# -------------------------------------------------------

# Checking 'room definition'
expect_error(annex_room_definition(TRUE), info = "No input args allowed")
expect_silent(x <- annex_room_definition())
expect_inherits(x, "data.frame",
            info = "Testing return value")
expect_identical(names(x), c("name", "long_name", "examples_valid_labels"),
            info = "Check if expected variables are in the data.frame")

# Mixing lower/uppercase to test for non-case sensitivity
to_test <- c("Bed", "bEd99", "BED00", "lau1", "LaU2", "LAU33")
expect_error(annex:::check_for_allowed_rooms(),
             info = "No input provided")
expect_error(annex:::check_for_allowed_rooms(3),
             info = "Input is not character")
expect_error(annex:::check_for_allowed_rooms(factor("BED")),
             info = "Input is not character")
expect_error(annex:::check_for_allowed_rooms(character()),
             info = "Input is not of length > 0")
expect_error(annex:::check_for_allowed_rooms(c(to_test, "foobar")),
             pattern = "Not allowed: 'foobar'",
             info = "Last value of input is not an allowed type")
expect_error(annex:::check_for_allowed_rooms(c("foo", "bar")),
             pattern = "Not allowed: 'foo', 'bar'",
             info = "Input is none of the allowed room labels")
expect_silent(x <- annex:::check_for_allowed_rooms(to_test))
expect_identical(toupper(to_test), annex:::check_for_allowed_rooms(to_test),
                 info = "Return should be identical in case all labels are allowed")


# Checking 'variable definition'
expect_error(annex_variable_definition("a"),
             info = "Input must be logical or be allowed to be coerced to logical")
expect_error(annex_variable_definition(TRUE, 3),
             info = "Only one input argument allowed")

# Testing proper use
expect_silent(x1 <- annex_variable_definition()) # Default
expect_silent(x2 <- annex_variable_definition(as_list = FALSE)) # should be identical to default
expect_silent(x3 <- annex_variable_definition(as_list = TRUE))  # return list
expect_identical(x1, x2, info = "Testing default behaviour")
expect_inherits(x1, "data.frame",
            info = "Testing return value")
expect_identical(names(x1),
            c("name", "required", "allowed", "lower", "upper", "allowed_units"),
            info = "Testing if return contains expected variables")
expected_classes <- c("name" = "character", "required" = "logical",
                      "lower" = "numeric", "upper" = "numeric", "allowed_units" = "character")
for (i in seq_along(expected_classes))
    expect_inherits(x1[[names(expected_classes)[i]]], expected_classes[i],
                    info = "Testing variable classes")
expect_true(all(!is.na(x1$name) & nchar(x1$name) > 0 & !is.na(x1$required)),
            info = "Check that all names are there and required flag is set")

expect_inherits(x3, "list",
             info = "Testing return type list")
expect_identical(names(x3), x1$name,
             info = "Testing if the list contains the same variables as the data.frame")
expect_true(all(sapply(x3, function(x) isTRUE(x$required) || isFALSE(x$required))),
            info = "Test if all required flags are TRUE or FALSE")
expect_true(all(sapply(x3, function(x) is.numeric(x$lower))),
            info = "Test if all lower bounds are numeric")
expect_true(all(sapply(x3, function(x) is.numeric(x$upper))),
            info = "Test if all upper bounds are numeric")

# Mixing lower/uppercase to test for non-case sensitivity
to_test <- c("co2", "pM1", "RADON", "sOLRaD")
expect_error(annex:::check_for_allowed_variables(),
             info = "No input provided")
expect_error(annex:::check_for_allowed_variables(3),
             info = "Input is not character")
expect_error(annex:::check_for_allowed_variables(factor("CO2")),
             info = "Input is not character")
expect_error(annex:::check_for_allowed_variables(character()),
             info = "Input is not of length > 0")
expect_error(annex:::check_for_allowed_variables(c(to_test, "foobar")),
             pattern = "Not allowed: 'foobar'",
             info = "Last value of input is not an allowed type")
expect_error(annex:::check_for_allowed_variables(c("foo", "bar")),
             pattern = "Not allowed: 'foo', 'bar'",
             info = "Input is none of the allowed variable labels")
expect_silent(x <- annex:::check_for_allowed_variables(to_test))
expect_identical(c("CO2", "PM1", "Radon", "SolRad"),
                 annex:::check_for_allowed_variables(to_test),
                 info = "Check if we get the corrected return if all variables are valid")







# -------------------------------------------------------
# Testing config objects / config object validation
# -------------------------------------------------------

# Reading the config file with standard tools!
expect_silent(f_config <- system.file("demos/demo_UIBK_config.csv", package = "annex"))
expect_silent(f_data   <- system.file("demos/demo_UIBK.xlsx", package = "annex"))

expect_true(file.exists(f_config))
expect_true(file.exists(f_data))

if (file.exists(f_config) && file.exists(f_data)) {
    expect_silent(config <- read.csv(f_config, na.string = c("NA", "")))
    expect_error(annex_check_config(subset(config, select = -study)),
                 info = "variable study is missing")
    expect_error(annex_check_config(config[-1, ]),
                 info = "definition for variable dateime is missing")
    expect_error(annex_check_config(rbind(head(config[-1, ]), config)),
                 info = "duplicated definitions")
    expect_error(annex_check_config(config[1, ]),
                 info = "no variable definition(s)")

    tmp <- config; tmp$study[3] <- NA;
    expect_error(annex_check_config(tmp),
                 info = "missing values in definition")
    tmp <- config; tmp$study[3] <- "a:b"
    expect_error(annex_check_config(tmp[1:3, ]),
                 info = "colon (':') in study, home, or room not allowed")
}

#
