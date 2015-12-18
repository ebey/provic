## ----------------------------------------------
## Emily Beylerian
## 12/10/2015
## Based on STATA code written by David Phillips
## ----------------------------------------------

load("Kinshasa_prepped.rds")

analysis.vars <- c("ART.Situation", "ARV.TAR.Received.", "Beneficiary.Age",
                   "Beneficiary.Health.Zone", "Beneficiary.Syphilis.Result",
                   "CD4.Count", "Creatinine", "Cotrimaxazole.Prophylaxis",
                   "Education.Level", "Marital.Status", "Partner.s.Status",
                   "Profession", "Religion", "Sex", "Support.Group",
                   "Target.Group")
end.date <- as.Date("09/30/2015", format = "%m/%d/%Y")
prescr.length <- 30
drop.window <- 90

# days to next visit
sorteddata <- workingdata[order(factor(workingdata$Client.Code),
                                factor(workingdata$Date.of.Touchpoint)), ]
cc <- unique(sorteddata$Client.Code)
dtnv <- diff(sorteddata$Date.of.Touchpoint)
for(i in seq(nrow(sorteddata) - 1)){
  if(sorteddata[i, 1] != sorteddata[i+1, 1]){
    # use end of study period if there was no next visit
    dtnv[i] <- end.date - sorteddata$Date.of.Touchpoint[i]
  }
}

drop.event <- rep(0, length(cc))
reentered <- rep(0, length(cc))
# drop-out is defined as being off ARVs for 90 days, in this case having
# an interval between appointment dates greater than the prescription length
# plus 90. See Unge et al 2010, Plos One Vol 5, Issue 10.
cc.dropped <- unique(sorteddata$Client.Code[which(dtnv > prescr.length + drop.window)])
drop.event[which(cc %in% cc.dropped)] <- 1
ind <- 1
cum.surv <- c()
# cumulative time without dropping out
for(ccode in cc){
  temp.dtnv <- dtnv[which(sorteddata$Client.Code == ccode)]
  if(ccode %in% cc.dropped){
    # if someone drops out we add the prescription length to their last visit
    # in order to count their cumulative survival in the program
    if(length(temp.dtnv) == 1){
      cum.surv[ind] <- prescr.length
    } else if(temp.dtnv[1] > prescr.length + drop.window){
      cum.surv[ind] <- prescr.length
    } else {
      drop.ind <- which(temp.dtnv > prescr.length + drop.window)
      if(length(drop.ind > 1)) {
        drop.ind <- drop.ind[1]
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

survobj <- Surv(time = cum.surv, event = drop.event)

