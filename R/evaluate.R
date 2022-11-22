#' Generates evaluation statistic for data frame
#'
#' @description Creates a statistic containing \code{mean}, \code{sd}, etc. of the
#' data frame provided
#'
#' @param data_frame a data frame in specific form [ADA::formatting()]
#' @param formula a formula describing the specific layout of the data frame
#' @param path a path with the file name for the generated .csv file
#'
#' @details The data frame must be formatted with [ADA::formatting()]
#' beforehand. Depending on the number of levels the size of the resulting
#' statistic varies.
#'
#' The data set is successively broken down into user, study, home, room, and
#' pollutant id and then summarized using statistical variables.
#' The statistical parameters used are contained in [ADA::statistics()].
#'
#' The output is saved in .csv format.
#'
#' @return Returns summary statistics in a convenient form
#'
#' @import Formula
#'
#' @seealso [ADA::formatting()], [base::data.frame()]
#' @export
#'
evaluate <- function(data_frame, formula, path) {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.character(path), length(path) == 1L)

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  #split model frame in all three main sections (poll, param, date) and extent
  #previous formula with season and time of day (tod) parameter
  formula = Formula(formula)
  formula = update(formula, . ~ . + season + tod | date)

  mfl = model.part(object = formula, data = data_frame, lhs=1)

  #get all the different pollutant names according to the formula provided
  pollutant_id = names(mfl)

  #store statistics in data frame and rbind everything together
  result = data.frame()

  for (p in pollutant_id) {

    columns = c(p, "date", "season", "tod", "user", "study", "home", "room")

    l = split(data_frame[columns],
              list(data_frame$user,
                   data_frame$study,
                   data_frame$home,
                   data_frame$room,
                   data_frame$season,
                   data_frame$tod), drop = TRUE)

    l1 = split(data_frame[columns],
              list(data_frame$user,
                   data_frame$study,
                   data_frame$home,
                   data_frame$room,
                   data_frame$season), drop = TRUE)

    l2 = split(data_frame[columns],
              list(data_frame$user,
                   data_frame$study,
                   data_frame$home,
                   data_frame$room,
                   data_frame$tod), drop = TRUE)

    l3 = split(data_frame[columns],
               list(data_frame$user,
                    data_frame$study,
                    data_frame$home,
                    data_frame$room), drop = TRUE)

    l = c(l, l1, l2, l3)

    res = lapply(X = l, FUN = statistics)
    res = do.call("rbind", res)
    rownames(res) <- NULL

    result = rbind(result, res)

  }

  # for (p in pollutant_id) {
  #
  #   tmp4 = subset(data_frame, select = c(p, "date"))
  #
  #   result = rbind(result, statistics(tmp4))
  #
  #   for (s in unique(data_frame$season)) {
  #
  #     tmp3 = subset(data_frame, subset = season == s, select = c(p, "date"))
  #
  #     result = rbind(result, statistics(tmp3, season = s))
  #
  #     for (t in unique(data_frame$tod)) {
  #
  #       tmp = subset(data_frame, subset = season == s & tod == t,
  #                    select = c(p, "date"))
  #
  #       result = rbind(result, statistics(tmp, season = s, tod = t))
  #     }
  #   }
  #
  #   for (d in unique(data_frame$tod)) {
  #
  #     tmp2 = subset(data_frame, subset = tod == d,
  #                  select = c(p, "date"))
  #
  #     result = rbind(result, statistics(tmp2, tod = d))
  #   }
  # }

  #round numerical values to three digits
  numeric_columns = sapply(result, class) == "numeric"
  result[numeric_columns] = round(result[numeric_columns], 3)

  #generate .csv file
  write.csv2(result, path)

  return(result)

}
