

# Days to next visit

dtnv.plots <- function(dtnv, sorteddata, prov, start.date, end.date){
  dtnvlist <- list()
  dtnvintervals <- list()
  visitno <- c()
  ccodes <- unique(sorteddata$Client.Code)
  for(cc in ccodes){
    dtnvlist[[cc]] <- dtnv[which(sorteddata$Client.Code == cc)]
    visitno <- append(visitno, seq(length(dtnvlist[[cc]])))
  }
  plot(dtnvlist[[1]], y = seq(length(dtnvlist[[1]])),
       col = adjustcolor(col = "#08519c", alpha.f = 0.3),
       xlim = c(0, 365), ylim = c(0, 25), pch = 16,
       xlab = "Days to Next Visit", ylab = "Visits Already Completed",
       main = paste(prov, start.date, "to", end.date))
  mtext(text = "Cut off after 1 year between visits or 25 visits")
  for(i in 1:length(ccodes)){
    l <- length(dtnvlist[[i]])
    points(x = dtnvlist[[i]], y = seq(l), pch = 16,
           col = adjustcolor(col = "#08519c", alpha.f = 0.3))
  }

  dtnvheight <- matrix(0, nrow = max(visitno), ncol = max(dtnv))
  for(vn in unique(visitno)){
    dtnvtemp <- dtnv[which(visitno == vn)]
    for(dt in dtnvtemp){
      dtnvheight[vn, dt] <- dtnvheight[vn, dt] + 1
    }
  }

  persp(x = seq(max(visitno)), y = seq(max(dtnv)), z = dtnvheight,
        theta = 35, phi = 15, col = "blue",
        xlab = "Visits completed", ylab = "Days to next visit")
}

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

