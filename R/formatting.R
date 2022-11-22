#' Formats a data frame in specific format
#'
#' @description Generates a new data frame according to the formula provided.
#'
#' @param data_frame a data frame
#' @param formula a formula describing the specific layout of the data frame
#' @param tz a time zone like "UTC", "MET", ...
#' @param format the format in which the time stamps are given in the data frame
#' @param user abbreviation for the name of the respective scientist
#' @param study a natural number (entered as a character)
#' @param home a natural number (entered as a character)
#' @param room abbreviation for the name of the respective room
#'
#' @details  The function adds columns referring to the specific season and
#' time of day (tod) for each time stamp. Pollutants will be on the left side,
#' the date terms on the right including season and tod as well as other
#' mandatory variables (home, room, etc.). If additional variables were given,
#' these can also be found in the output.
#'
#' If a mandatory variable is not present in the output, a message is issued.
#' Furthermore, the evaluate function of ADA cannot be used unless all the required
#' variables are available. If the formula passed contains more than just the
#' mandatory variables, the evaluate function of ADA can be used without any
#' problems.
#'
#' The output contains at least seven columns in addition to the columns for
#' the respective pollutants.
#'
#' @return Returns a data frame that has a specific format according to the
#' formula provided
#'
#' @import Formula
#'
#' @seealso [base::data.frame()], [stats::formula()]
#' @export
#'
formatting <- function(data_frame,
                       formula,
                       tz = "UTC",
                       format = "%Y-%m-%d %H:%M:%S",
                       user = "",
                       study = "",
                       home = "",
                       room = ""
                       ){

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.character(tz), length(tz) == 1L)
  stopifnot(is.character(user), length(user) == 1L)
  stopifnot(is.character(study), length(study) == 1L)
  stopifnot(is.character(home), length(home) == 1L)
  stopifnot(is.character(room), length(room) == 1L)
  stopifnot(is.character(format), length(format) == 1L)

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  formula <- Formula(formula)
  col_names <- c("user", "study", "home", "room")
  col_ref <- c(user, study, home, room)

  #create new data frame in standardized form; lhs(poll) ~ rhs1(param) + rhs2(date)
  mf <- model.frame(formula = formula, data = data_frame, na.action = na.pass)

  #rename column and standardize time to object of class POSIXt if necessaryhead
  if (!colnames(mf)[ncol(mf)] == "date") {
    colnames(mf)[ncol(mf)] <- "date"
  }

  if (!inherits(mf$date, "POSIXt")) {
    mf$date <- as.POSIXct(mf$date, tz = tz, format = format)
  }

  #categorize time to specific season and time of day (tod)
  mf$season <- as.integer(format(mf$date, "%m%d"))
  mf$season <- cut(x = mf$season, breaks = c(-Inf, 329, 620, 922, 1220, Inf),
                   labels = c("12-02", "03-05", "06-08", "09-11", "12-02"))

  mf$tod <- as.integer(format(mf$date, "%H"))
  mf$tod <- cut(x = mf$tod, breaks = c(-Inf, 6, 22, Inf),
                labels = c("23-07", "07-23", "23-07"))

  #add user/study/home/room columns if necessary
  for (i in 1:length(col_names)) {
    if (!col_ref[i] == "") {
      mf[col_names[i]] = col_ref[i]
    }
  }

  #check if all mandatory variables are in the formatted output
  man_var = c("date", "season", "tod", "user", "study", "home", "room")

  for (i in man_var) {
    if (!(i %in% colnames(mf))) {
      print(paste("mandatory variable", i, "is missing!"))
    }
  }

  return(mf)

}
