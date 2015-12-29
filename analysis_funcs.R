
# Univariate survival analysis

survival.univ <- function(survobj, vars, covardf, subtitle = ""){

  for(av in vars){
    lvls <- sort(unique(covardf[[av]]))
    mis <- sum(lvls == "", na.rm = TRUE) + sum(is.na(lvls))
    lvls <- lvls[lvls != ""]
    nlvls <- length(lvls)

    colpal <- suppressWarnings(brewer.pal(name = "YlGnBu",
                                          n = nlvls + 1)[-1])
    if(nlvls > 8){
      colpal <- append(colpal, rev(brewer.pal(name = "OrRd", n = 9)[-1]))
    }
    covardf[["thisvar"]] <- covardf[[av]]
    fit <- coxph(survobj ~ thisvar, data = covardf)
    datavals <- data.frame(thisvar = lvls)
    plot(survfit(fit, newdata = datavals), col = colpal,
         main = av, xlab = "Days in ART program",
         ylab = "Proportion retained in treatment",
         sub = paste("Missing:", mis))
    lvln <- c()
    for(lv in lvls){
      lvln <- append(lvln, sum(covars[[av]] == lv, na.rm = TRUE))
    }
    legend("topright", legend = paste0(lvls, " (",lvln,")"),
           fill = colpal, cex = 0.75)
    mtext(subtitle, side = 3, line = 0)
  }

}

