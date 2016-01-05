

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

survival.univ <- function(sorteddata, vars, subtitle = ""){

  ccodes <- unique(sorteddata$Client.Code)

  # find drop events
  drop.event <- rep(0, length(ccodes))
  reentered <- rep(0, length(ccodes))
  # drop-out is defined as being off ARVs for 90 days, in this case having
  # an interval between appointment dates greater than the prescription length
  # plus 90. See Unge et al 2010, Plos One Vol 5, Issue 10.
  cc.dropped <- unique(sorteddata$Client.Code[which(dtnv > prescr.length + drop.window)])
  drop.event[which(ccodes %in% cc.dropped)] <- 1
  ind <- 1
  cum.surv <- c()
  # cumulative time without dropping out
  for(cc in ccodes){
    temp.dtnv <- dtnv[which(sorteddata$Client.Code == cc)]
    if(cc %in% cc.dropped){
      # if someone drops out we add the prescription length to their last visit
      # in order to count their cumulative survival in the program
      if(length(temp.dtnv) == 1){
        cum.surv[ind] <- prescr.length
      } else if(temp.dtnv[1] > prescr.length + drop.window){
        cum.surv[ind] <- prescr.length
        reentered[ind] <- 1
      } else {
        drop.ind <- which(temp.dtnv > prescr.length + drop.window)
        if(length(drop.ind > 1)) {
          drop.ind <- drop.ind[1]
        }
        if(length(temp.dtnv) > drop.ind){
          reentered[ind] <- 1
        }
        cum.surv[ind] <- sum(temp.dtnv[1:(drop.ind - 1)]) + prescr.length
      }

    } else {
      # these people make it to the end of the study without dropping out so
      # we don't add the prescription length to their time in the program
      # (they are counted as censored)
      cum.surv[ind] <- sum(temp.dtnv)
    }
    ind <- ind + 1
  }

  # Survival analysis
  survobj <- Surv(time = cum.surv, event = drop.event)
  covardf <- as.data.frame(sorteddata[!duplicated(sorteddata$Client.Code),
                                      c("Client.Code", vars)])

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

