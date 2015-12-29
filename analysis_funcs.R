
# Univariate survival analysis

survival.univ <- function(survobj, vars, covardf, subtitle = ""){

  for(av in vars){
    lvls <- sort(unique(covardf[[av]]))
    mis <- sum(lvls == "", na.rm = TRUE) + sum(is.na(lvls))
    lvls <- lvls[lvls != ""]

    colpal <- suppressWarnings(brewer.pal(name = "YlGnBu",
                                          n = length(lvls) + 1)[-1])
    covardf[["thisvar"]] <- covardf[[av]]
    fit <- coxph(survobj ~ thisvar, data = covardf)
    datavals <- data.frame(thisvar = lvls)
    plot(survfit(fit, newdata = datavals), col = colpal,
         main = av, xlab = "Days in ART program",
         ylab = "Proportion retained in treatment",
         sub = paste("Missing:", mis))
    lvlsn <- c()
    for(lv in lvls){
      lvlsn <- append(lvlsn, sum(covars[[av]] == lv, na.rm = TRUE))
    }
    legend("topright", legend = paste0(lvls, " (",lvlsn,")"),
           fill = colpal, cex = 0.75)
    mtext(subtitle, side = 3, line = 0)
  }

}

