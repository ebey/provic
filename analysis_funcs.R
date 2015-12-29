
# Univariate survival analysis

survival.univ <- function(survobj, vars, covardf, subtitle = ""){

  for(av in vars){
    lvls <- unique(covardf[[av]])
    lvls <- lvls[lvls != ""]
    colpal <- suppressWarnings(brewer.pal(name = "YlGnBu",
                                          n = length(lvls) + 1)[-1])
    covardf[["thisvar"]] <- covardf[[av]]
    fit <- coxph(survobj ~ thisvar, data = covardf)
    datavals <- data.frame(thisvar = lvls)
    plot(survfit(fit, newdata = datavals), col = colpal,
         main = av, xlab = "Days in ART program",
         ylab = "Proportion retained in treatment")
    legend("topright", legend = lvls, fill = colpal)
    mtext(subtitle, side = 3, line = 0)
  }

}

