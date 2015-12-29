## ----------------------------------------------
## Emily Beylerian
## 12/10/2015
## Based on STATA code written by David Phillips
## ----------------------------------------------

library(RColorBrewer)
library(survival)

# load functions that we need
source("analysis_funcs.R")
source("prep_data.R")

prov <- "Katanga"
# prov <- "Kinshasa"
start.date <- as.Date("10/01/2013", format = "%m/%d/%Y")
end.date <- as.Date("09/30/2015", format = "%m/%d/%Y")
prescr.length <- 90
drop.window <- 90

# load, subset and clean data
# prep.data(province = prov, start.date = start.date, end.date = end.date)

workingdata <- readRDS(paste0(prov, "_prepped.rds"))

# group numeric variables
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age < 11] <- "0-10"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 10 &
                                    workingdata$Beneficiary.Age < 21] <- "11-20"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 20 &
                                    workingdata$Beneficiary.Age < 31] <- "21-30"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 30 &
                                    workingdata$Beneficiary.Age < 41] <- "31-40"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 40 &
                                    workingdata$Beneficiary.Age < 51] <- "41-50"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 50 &
                                    workingdata$Beneficiary.Age < 61] <- "51-60"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 60] <- "61+"

workingdata$CD4.Count.Group[workingdata$CD4.Count < 201] <- "0-200"
workingdata$CD4.Count.Group[workingdata$CD4.Count > 200 &
                              workingdata$CD4.Count <= 350] <- "201-350"
workingdata$CD4.Count.Group[workingdata$CD4.Count > 350 &
                              workingdata$CD4.Count <= 500] <- "351-500"
workingdata$CD4.Count.Group[workingdata$CD4.Count > 500] <- "501+"


analysis.vars <- c("ART.Situation", "ARV.TAR.Received.",
                   "Beneficiary.Health.Zone", "Beneficiary.Syphilis.Result",
                   "Cotrimaxazole.Prophylaxis", "Education.Level",
                   "Marital.Status", "Partner.s.Status", "Profession",
                   "Religion", "Sex", "Support.Group", "Target.Group",
                   "Beneficiary.Age.Group", "CD4.Count.Group")




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
reentered <- rep(0, length(cc)) # not used yet
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

survobj <- Surv(time = cum.surv, event = drop.event)
covars <- as.data.frame(sorteddata[!duplicated(sorteddata$Client.Code),
                                   c("Client.Code", analysis.vars)])

survival.univ(survobj, vars = analysis.vars, covardf = covars,
              subtitle = paste(prov, start.date, "-", end.date))

