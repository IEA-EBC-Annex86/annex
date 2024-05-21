# -------------------------------------------------------
# Checking unit conversions.
# -------------------------------------------------------

if (interactive()) {
    library("tinytest")
    library("annex")
}


# -------------------------------------------------------
# Getting variable definition from XLSX template
# -------------------------------------------------------
expect_silent(vdef <- annex_variable_definition(),
              info = "Loading annex variable definition")

# For each variable where `allowed_units` are defined,
# a conversion function MUST exist. Even if it is only
# one fixed unit.
vdef <- subset(vdef, !is.na(allowed_units))


# -------------------------------------------------------
# Check whether or not the function(s) exist
# -------------------------------------------------------
for (var in vdef$name) {
    fnname <- sprintf("annex:::convert_unit_%s", var)
    fn <- tryCatch(eval(parse(text = fnname)),
                   error = function(e) return(NULL))
    expect_inherits(fn, "function",
                    info = paste("Checking if function", fnname, "exists"))
}


