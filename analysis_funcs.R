

# Days to next visit

dtnv.plots <- function(dtnv, sorteddata, prov, start.date, end.date){
  library(plotly)

  # days to next visit
  dtnv <- diff(sorteddata$Date.of.Touchpoint)
  cens <- rep(0, length(dtnv))
  for(i in seq(nrow(sorteddata) - 1)){
    if(sorteddata[i, 1] != sorteddata[i+1, 1]){
      # use end of study period if there was no next visit
      dtnv[i] <- end.date - sorteddata$Date.of.Touchpoint[i]
      cens[i] <- 1
    }
  }
  dtnv <- append(dtnv, end.date - tail(sorteddata$Date.of.Touchpoint, 1))
  cens <- append(cens, 1)

  dtnvlist <- list()
  dtnvintervals <- list()
  visitno <- c()
  ccodes <- unique(sorteddata$Client.Code)
  for(cc in ccodes){
    dtnvlist[[cc]] <- dtnv[which(sorteddata$Client.Code == cc)]
    visitno <- append(visitno, seq(length(dtnvlist[[cc]])))
  }
  for(j in seq(max(visitno))){
    dtnvintervals[[j]] <- dtnv[which(visitno == j)]
  }

  # boxplot
  boxplot(dtnvintervals, xlab = "Visits completed", ylab = "Days to next visit",
          main = paste(prov, start.date, "to", end.date),
          col = adjustcolor(col = "#08519c", alpha.f = 0.3), outline = FALSE)

  # scatter plot
  plot(dtnvlist[[1]], y = seq(length(dtnvlist[[1]])),
       col = adjustcolor(col = "#238b45", alpha.f = 0.3),
       xlim = c(0, 365), ylim = c(0, 35), pch = 16,
       xlab = "Days to Next Visit", ylab = "Visits Already Completed",
       main = paste(prov, start.date, "to", end.date))
  mtext(text = "Cut off after 1 year between visits")
  count <- 1
  for(i in 1:length(ccodes)){
    plotcol <- ifelse(cens[i] == 0,
                      adjustcolor(col = "#08519c", alpha.f = 0.3),
                      adjustcolor(col = "#238b45", alpha.f = 0.3))
    l <- length(dtnvlist[[i]])
    points(x = dtnvlist[[i]], y = seq(l), pch = 16,
           col = plotcol)
    count <- count + 1
  }
  legend("topright", legend = c("Not censored obs", "Censored obs"),
         pch = 16, col = c(adjustcolor(col = "#08519c", alpha.f = 0.3),
                           adjustcolor(col = "#238b45", alpha.f = 0.3)))

  # 3d plot
  dtnvheight <- matrix(0, nrow = max(visitno), ncol = max(dtnv))
  for(vn in unique(visitno)){
    dtnvtemp <- dtnv[which(visitno == vn)]
    for(dt in dtnvtemp){
      dtnvheight[vn, dt] <- dtnvheight[vn, dt] + 1
    }
  }
  plot_ly(z = dtnvheight, type = "surface")
#   persp(x = seq(max(visitno)), y = seq(max(dtnv)), z = dtnvheight,
#         theta = -30, phi = 35, col = "blue",
#         xlab = "Visits completed", ylab = "Days to next visit")

}

# Univariate survival analysis

survival.univ <- function(sorteddata, dtnv, vars, subtitle = ""){

  ccodes <- unique(sorteddata$Client.Code)

  # days to next visit
  dtnv <- diff(sorteddata$Date.of.Touchpoint)
  for(i in seq(nrow(sorteddata) - 1)){
    if(sorteddata[i, 1] != sorteddata[i+1, 1]){
      # use end of study period if there was no next visit
      dtnv[i] <- end.date - sorteddata$Date.of.Touchpoint[i]
    }
  }
  dtnv <- append(dtnv, end.date - tail(sorteddata$Date.of.Touchpoint, 1))

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

