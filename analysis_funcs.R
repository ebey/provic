

# Days to next visit

dtnv.plots <- function(sorteddata, title = "",
                       type = "scatterplot", var = NULL){
  library(plotly)

  # days to next visit
  dtnv <- sorteddata$dtnv
  cens <- sorteddata$censored

  colpal <- c("#00A0DC", "#8C68CB", "#EC4339", "#F47B16", "#00AEB3",
              "#EFB920", "#ED4795", "#7CB82F", "#86898C")

  if(is.null(var)){
    plotcols <- rep(colpal[1], length(dtnv))
  } else {
    if(var == "censored"){
      plotcols <- cens
      lvls <- unique(cens)
    } else {
      plotcols <- sorteddata[[var]]
      lvls <- unique(sorteddata[[var]])
    }

    if(length(lvls) > 9 & type == "scatterplot"){
      stop("Not enough colors in palette for unique levels in covariate")
    }
    for(i in seq(length(lvls))){
      plotcols <- replace(plotcols, list = plotcols == lvls[i],
                          values = colpal[i])
    }
  }
  plotcols <- adjustcolor(plotcols, alpha.f = 0.3)

  dtnvlist <- list()
  dtnvintervals <- list()
  plotcollist <- list()
  visitno <- c()
  ccodes <- unique(sorteddata$Client.Code)
  for(cc in ccodes){
    dtnvlist[[cc]] <- dtnv[which(sorteddata$Client.Code == cc)]
    plotcollist[[cc]] <- plotcols[which(sorteddata$Client.Code == cc)]
    visitno <- append(visitno, seq(length(dtnvlist[[cc]])))
  }
  for(j in seq(max(visitno))){
    dtnvintervals[[j]] <- dtnv[which(visitno == j)]
  }

  if(type == "boxplot"){

    # boxplot
    boxplot(dtnvintervals, xlab = "Visits completed", ylab = "Days to next visit",
            main = title,
            col = adjustcolor(col = "#08519c", alpha.f = 0.3), outline = FALSE)

  } else if(type == "scatterplot"){

    # scatter plot
    plot(dtnvlist[[1]], y = seq(length(dtnvlist[[1]])),
         col = plotcollist[[1]],
         xlim = c(0, 700), ylim = c(0, 35), pch = 16,
         xlab = "Days to Next Visit", ylab = "Visits Already Completed",
         main = paste(prov, start.date, "to", end.date))
    mtext(text = var)
    count <- 1
    for(i in 1:length(ccodes)){
      l <- length(dtnvlist[[i]])
      points(x = dtnvlist[[i]], y = seq(l), pch = 16,
             col = plotcollist[[i]])
      count <- count + 1
    }
    legend("topright", legend = lvls,
           pch = 16, col = colpal[seq(length(lvls))])

  } else if(type == "3d"){

    # 3d plot
    dtnvheight <- matrix(0, nrow = max(visitno), ncol = max(dtnv))
    for(vn in unique(visitno)){
      dtnvtemp <- dtnv[which(visitno == vn)]
      for(dt in dtnvtemp){
        dtnvheight[vn, dt] <- dtnvheight[vn, dt] + 1
      }
    }
    plot_ly(z = dtnvheight, type = "surface")
  }

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
  # this only takes the covariate values from the first row with
  # a given client code - will need to change this for time dependent coefs!
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

