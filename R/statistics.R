#' Generates compact statistical summary for data frame
#'
#' @description Creates the statistical summary used by [annex::evaluate()]
#'
#' @param data_frame a data frame in specific form [annex::formatting()]
#' @param season a character referring to the specific season
#' @param tod a character referring to the specific time of day
#'
#' @details Using [annex::evaluate()], the data set is suitably divided and then
#' combined with this function. Grouping is based on user, study, home, room,
#' season, and tod.
#'
#' @return Return statistical summary of the data set
#'
#' @import Formula
#'
statistics <- function(data_frame, season = "ALL", tod = "ALL") {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(is.character(season), length(season) == 1L)
  stopifnot(is.character(tod), length(tod) == 1L)

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  #variables
  x = data_frame[,1]
  time = data_frame$date

  #constants
  user = unique(data_frame$user)
  study = unique(data_frame$study)
  home = unique(data_frame$home)
  room = unique(data_frame$room)
  s = unique(data_frame$season)
  t = unique(data_frame$tod)

  if (length(s) == 1) {
    season = s
  }

  if (length(t) == 1) {
    tod = t
  }

  na_sum = sum(is.na(x))
  x = na.omit(x)

  shape1 = mean(x)^2*((1-mean(x))/var(x) - 1/mean(x))
  shape2 = shape1*(1/mean(x) - 1)

  val = data.frame("User.ID" = user,
                   "Study.ID"= study,
                   "Home.ID" = home,
                   "Room.ID" = room,
                   "Pollutant ID" = toupper(names(data_frame)[1]),
                   "Period ID" = season,
                   "Time ID" = tod,
                   "Mean" = mean(x),
                   "Std" = sd(x),
                   "P2.5" = quantile(x, 0.025)[[1]],
                   "P25" = quantile(x, 0.25)[[1]],
                   "P50" = quantile(x, 0.5)[[1]],
                   "P75" = quantile(x, 0.75)[[1]],
                   "P90" = quantile(x, 0.975)[[1]],
                   "shape1" = shape1,
                   "shape2" = shape2,
                   "exp_param" = 1/mean(x),
                   "Start" = min(time),
                   "End" = max(time),
                   "Typ.SI" = (as.numeric(max(time)) - as.numeric(min(time)))/length(time),
                   "Samples" = length(x),
                   "NAs" = na_sum,
                   "Percentage.excl" = "",
                   "Percentage.Mincut" = "",
                   "Percentage.Maxcut" = "")

  return(val)
}
