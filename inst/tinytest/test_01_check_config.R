# -------------------------------------------------------
# Checking function 'check_config'
# -------------------------------------------------------

if (interactive()) library("tinytest")

# Reading the config file with standard tools!
expect_silent(f_config <- system.file("data/demo_UIBK_config.csv", package = "Annex"))
expect_silent(f_data   <- system.file("data/demo_UIBK.xlsx", package = "Annex"))

expect_true(file.exists(f_config))
expect_true(file.exists(f_data))

if (file.exists(f_config) && file.exists(f_data)) {
    expect_silent(config <- read.csv(f_config, na.string = c("NA", "")))
    expect_error(check_config(subset(config, select = -study)),
                 info = "variable study is missing")
    expect_error(check_config(config[-1, ]),
                 info = "definition for variable dateime is missing")
    expect_error(check_config(rbind(head(config[-1, ]), config)),
                 info = "duplicated definitions")
    expect_error(check_config(config[1, ]),
                 info = "no variable definition(s)")

    tmp <- config; tmp$study[3] <- NA;
    expect_error(check_config(tmp),
                 info = "missing values in definition")
    tmp <- config; tmp$study[3] <- "a:b"
    expect_error(check_config(tmp[1:3, ]),
                 info = "colon (':') in study, home, or room not allowed")
}

