## ----------------------------------------------
## Emily Beylerian
## 12/09/2015
## Based on STATA code written by David Phillips
## ----------------------------------------------


# Load data and subset observations ---------------------------------------

# total observations = 431340
rawdata <- read.csv("C:/Users/ebeylerian/Dropbox/ProVIC Retention analysis/data/Ben_TP_HIVpos_alltime_allprov_2015.10.22.csv",
                    header = TRUE, stringsAsFactors = FALSE)
doT <- as.Date(rawdata$Date.of.Touchpoint, format = "%m/%d/%Y")


# only use observations after March 2014, in Kinshasa, where ART was received
workingdata <- rawdata[doT >= "2014-04-01", ]
workingdata <- workingdata[workingdata$Beneficiary.Region.Province == "Kinshasa", ]
## EDIT THIS DEPENDING ON EXPLORATORTY RESULTS
workingdata <- workingdata[workingdata$ARV.TAR.Received. != "", ]
workingdata$Date.of.Touchpoint <- as.Date(workingdata$Date.of.Touchpoint,
                                          format = "%m/%d/%Y")
workingdata$Beginning.Date.of.Treatment <- as.Date(workingdata$Beginning.Date.of.Treatment,
                                                   format = "%m/%d/%Y")

## new total obs = 7083


# Clean variable values ---------------------------------------------------

# Remove NA client codes
workingdata <- workingdata[!is.na(workingdata$Client.Code), ]

# Beneficiary health zone
workingdata$Beneficiary.Health.Zone[workingdata$Beneficiary.Health.Zone == "ZS"] <- "Unlabeled"
workingdata$Beneficiary.Health.Zone[workingdata$Beneficiary.Health.Zone == "HZ"] <- "Unlabeled"

# Beneficiary health area
workingdata$Beneficiary.Health.Area[workingdata$Beneficiary.Health.Area == "AS"] <- "Unlabeled"

# Partner status
workingdata$Partner.s.Status[grep("gatif", workingdata$Partner.s.Status, value = FALSE)] <- "N"
workingdata$Partner.s.Status[workingdata$Partner.s.Status == "Inconnue"] <- "Unknown"

# Sex
workingdata$Sex[workingdata$Sex == "Masculin"] <- "Male"
workingdata$Sex[workingdata$Sex == "Feminin"] <- "Female"

# Martial status
workingdata$Marital.Status[workingdata$Marital.Status == "Mariage monogamique"] <- "Monogamous marriage"

# ART situation
workingdata$ART.Situation[workingdata$ART.Situation == "Ancien cas TARV/ Site"] <- "Existing ART case"
workingdata$ART.Situation[workingdata$ART.Situation == "Ancien cas TARV/ Non Site"] <- "Existing ART case non-site"

# Deal with duplicates ----------------------------------------------------

# drop complete dups (new total = 2708)
workingdata <- unique(workingdata)

dupids <- which(duplicated(workingdata[, 1:2]))

# find the repeated observations and check for blank values in all variables,
# if so replace with the value from the duplicate obs
for(i in dupids){

  dupuid <- workingdata[i, 1]
  dupdate <- workingdata[i, 2]
  k <- which(workingdata$Client.Code == dupuid & workingdata$Date.of.Touchpoint == dupdate)

  for(j in 4:length(workingdata)){

    if((workingdata[k[1], j] == "" | is.na(workingdata[k[1], j]))
       & (workingdata[k[2], j] != "" & !is.na(workingdata[k[2], j]))){
      workingdata[k[1], j] <- workingdata[k[2], j]
    } else if((workingdata[k[2], j] == "" | is.na(workingdata[k[2], j]))
              & (workingdata[k[1], j] != "" & !is.na(workingdata[k[1], j]))){
      workingdata[k[2], j] <- workingdata[k[1], j]
    }

  }
}

# remove complete dups if we have created new ones
workingdata <- unique(workingdata)

# new total observations = 2703


# Save prepped dataframe --------------------------------------------------

saveRDS(workingdata, file = "Kinshasa_prepped.rds")
