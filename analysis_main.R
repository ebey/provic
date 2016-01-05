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

# or load already prepared data
workingdata <- readRDS(paste0(prov, "_prepped.rds"))

# group numeric variables
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age <= 14] <- "0-14"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 14 &
                                    workingdata$Beneficiary.Age < 25] <- "15-24"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 24 &
                                    workingdata$Beneficiary.Age < 35] <- "25-34"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 34 &
                                    workingdata$Beneficiary.Age < 45] <- "35-44"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age > 44 &
                                    workingdata$Beneficiary.Age < 55] <- "45-54"
workingdata$Beneficiary.Age.Group[workingdata$Beneficiary.Age >= 55] <- "55+"

workingdata$CD4.Count.Group[workingdata$CD4.Count < 200] <- "0-199"
workingdata$CD4.Count.Group[workingdata$CD4.Count >= 200 &
                              workingdata$CD4.Count < 350] <- "200-349"
workingdata$CD4.Count.Group[workingdata$CD4.Count >= 350 &
                              workingdata$CD4.Count < 500] <- "350-499"
workingdata$CD4.Count.Group[workingdata$CD4.Count >= 500] <- "500+"

workingdata$Support.Group.YesNo[workingdata$Support.Group == ""] <- "No"
workingdata$Support.Group.YesNo[workingdata$Support.Group == "N/A"] <- "No"
workingdata$Support.Group.YesNo[workingdata$Support.Group != "" &
                                  workingdata$Support.Group != "N/A"] <- "Yes"


analysis.vars <- c("ART.Situation", "ARV.TAR.Received.",
                   "Beneficiary.Health.Zone", "Beneficiary.Syphilis.Result",
                   "Cotrimaxazole.Prophylaxis", "Education.Level",
                   "Marital.Status", "Partner.s.Status", "Profession",
                   "Religion", "Sex", "Support.Group.YesNo", "Target.Group",
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
dtnv <- append(dtnv, end.date - tail(sorteddata$Date.of.Touchpoint, 1))




# Days to next visit
dtnv.plots(dtnv, sorteddata, prov, start.date, end.date)


# Univariate survival analysis
survival.univ(sorteddata, vars = analysis.vars,
              subtitle = paste(prov, start.date, "-", end.date))

