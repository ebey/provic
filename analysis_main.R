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

analysis.vars <- c("ART.Situation", "ARV.TAR.Received.",
                   "Beneficiary.Health.Zone", "Beneficiary.Syphilis.Result",
                   "Cotrimaxazole.Prophylaxis", "Education.Level",
                   "Marital.Status", "Partner.s.Status", "Profession",
                   "Religion", "Sex", "Support.Group.YesNo", "Target.Group",
                   "Beneficiary.Age.Group", "CD4.Count.Group")


# sort by client code and date
sorteddata <- workingdata[order(factor(workingdata$Client.Code),
                                factor(workingdata$Date.of.Touchpoint)), ]


# Days to next visit
dtnv.plots(sorteddata, title = paste(prov, start.date, "to", end.date),
           type = "scatterplot", var = "Sex")


# Univariate survival analysis
survival.univ(sorteddata, vars = analysis.vars,
              subtitle = paste(prov, start.date, "-", end.date))

