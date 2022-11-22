evaluate2 <- function(data_frame, formula) {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(inherits(formula, "formula"))

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  names = c("Period ID", "Time ID", "Avg", "Std", "P2.5", "P.25", "P50",
            "P75", "P97.5", "TotalSamples", "NANs")

  # names = c("Period ID", "Time ID", "Avg", "Std", "P2.5", "P.25", "P50",
  #           "P75", "P97.5", "TotalSamples", "NANs", "Start", "End", "Typ. SI")

  time_loc = ncol(data_frame) - 2

  #split model frame in all three main sections (poll, param, date) and extent
  #previous formula with season and time of day (tod) parameter
  formula = Formula(formula)
  formula = update(formula, . ~ . + season + tod | date)

  mfl <- model.part(object = formula, data = data_frame, lhs=1)
  mfr <- model.part(object = formula, data = data_frame, rhs=1)
  mft <- model.part(object = formula, data = data_frame, rhs=2)

  #generate list with data frames for each pollutant in data_frame and general
  #statistic via statistics function
  frames <- list()
  result <- list()

  for (i in seq_along(mfl)) {

    #generate data_frame for each pollutant
    frames[[i]] <- cbind(mfl[,i, drop =FALSE], mfr, mft)

    #aggregate each pollutant by season & tod and rename
    result[[i]] <- aggregate(frames[[i]][,1],
                             by = list(frames[[i]]$season, frames[[i]]$tod),
                             FUN = statistics)

    result[[i]] <- setNames(data.frame(result[[i]][,1:2], result[[i]][,3]),
                            names)

    #aggregate each pollutant by ONLY tod and rename
    result_allseasons <- aggregate(frames[[i]][,1],
                                  by = list(frames[[i]]$tod),
                                  FUN = statistics)

    result_allseasons <- setNames(data.frame(tmp = "ALL", result_allseasons[,1], result_allseasons[,2]),
                                  names)

    #aggregate each pollutant by ONLY season and rename

    result_alltods <- aggregate(frames[[i]][,1],
                                   by = list(frames[[i]]$season),
                                   FUN = statistics)

    result_alltods <- setNames(data.frame(result_alltods[,1], tmp = "ALL", result_alltods[,2]),
                                  names)

    #aggregate each pollutant by NOTHING and rename
    result_allall <- setNames(data.frame("tmp1" = "ALL", "tmp2" = "ALL",
                                         matrix(statistics(frames[[i]][,1]), nrow = 1)),
                              names)

    #Fuse everything together and add POLLUTANT ID
    result[[i]] <- rbind(result_allseasons, result[[i]], make.row.names = TRUE)
    result[[i]] <- rbind(result_alltods, result[[i]], make.row.names = TRUE)
    result[[i]] <- rbind(rbind(result_allall), result[[i]], make.row.names = TRUE)
    result[[i]] <- cbind("Pollutant ID" = toupper(colnames(frames[[i]][1])), result[[i]])


  }

  return(result)
}
