## ----------------------------------------------
## Emily Beylerian
## 12/09/2015
## Based on STATA code written by David Phillips
## ----------------------------------------------


# Load data ---------------------------------------------------------------

cat("**Loading Data**")

rawdata <- read.csv("C:/Users/ebeylerian/Dropbox/ProVIC Retention analysis/data/Ben_TP_HIVpos_alltime_allprov_2015.10.22.csv",
                    header = TRUE, stringsAsFactors = FALSE)

# Clean variable values ---------------------------------------------------

cat("**Cleaning Data**")

# Remove NA client codes
workingdata <- rawdata[!is.na(rawdata$Client.Code), ]

# Beneficiary health zone
workingdata$Beneficiary.Health.Zone[workingdata$Beneficiary.Health.Zone == "ZS" |
                                      workingdata$Beneficiary.Health.Zone == "HZ" |
                                      workingdata$Beneficiary.Health.Zone == "HZS"] <- "Health Zone"
workingdata$Beneficiary.Health.Zone[workingdata$Beneficiary.Health.Zone == "Masina 1"] <- "Masina I"
workingdata$Beneficiary.Health.Zone[workingdata$Beneficiary.Health.Zone == "Masina 2"] <- "Masina II"
workingdata$Health.Zone.of.the.Touchpoint[workingdata$Health.Zone.of.the.Touchpoint == "Masina 1"] <- "Masina I"
workingdata$Health.Zone.of.the.Touchpoint[workingdata$Health.Zone.of.the.Touchpoint == "Masina 2"] <- "Masina II"
workingdata$Health.Zone.of.the.Touchpoint[grep("Zone de", workingdata$Health.Zone.of.the.Touchpoint, value = FALSE)] <- "Health Zone"
workingdata$Health.Zone.of.the.Touchpoint[grep("Autre", workingdata$Health.Zone.of.the.Touchpoint, value = FALSE)] <- "Other (specify)"

# Beneficiary health area
workingdata$Beneficiary.Health.Area[workingdata$Beneficiary.Health.Area == "AS" |
                                      workingdata$Beneficiary.Health.Area == "HAS"] <- "Health Area"
workingdata$Health.Area.of.the.Touchpoint[workingdata$Health.Area.of.the.Touchpoint == "AS" |
                                      workingdata$Health.Area.of.the.Touchpoint == "HAS"] <- "Health Area"

# Partner status
workingdata$Partner.s.Status[grep("gatif", workingdata$Partner.s.Status, value = FALSE)] <- "Negative"
workingdata$Partner.s.Status[workingdata$Partner.s.Status == "Inconnue"] <- "Unknown"

# Sex
workingdata$Sex[workingdata$Sex == "Masculin"] <- "Male"
workingdata$Sex[workingdata$Sex == "Feminin"] <- "Female"

# Martial status
workingdata$Marital.Status[workingdata$Marital.Status == "Mariage monogamique"] <- "Monogamous marriage"
workingdata$Marital.Status[grep("gatif", workingdata$Marital.Status, value = FALSE)] <- "Negative"

# TB
workingdata$TB.Screening.Result[grep("gatif", workingdata$TB.Screening.Result, value = FALSE)] <- "Negative"
workingdata$TB.Screening.Result[workingdata$TB.Screening.Result == "Non"] <- "Negative"
workingdata$TB.Screening.Result[workingdata$TB.Screening.Result == "Oui"] <- "Positive"
workingdata$TB.Treatment.Received[grep("gatif", workingdata$TB.Treatment.Received, value = FALSE)] <- "Negative"
workingdata$TB.Treatment.Received[workingdata$TB.Treatment.Received == "Pendant la grossesse"] <- "During pregnancy"

# Type of service
workingdata$Type.of.Service[workingdata$Type.of.Service == "Autre service"] <- "Other service"
workingdata$Type.of.Service[workingdata$Type.of.Service == "Service de clinique"] <- "Clinical service"

# ART situation
workingdata$ART.Situation[workingdata$ART.Situation == "Ancien cas TARV/ Site"] <- "Existing ART case"
workingdata$ART.Situation[workingdata$ART.Situation == "Ancien cas TARV/ Non Site"] <- "Existing ART case non-site"
workingdata$ART.Situation[workingdata$ART.Situation == "Consultation prénatale"] <- "Antenatal consultation"
workingdata$ART.Situation[workingdata$ART.Situation == "Salle d'accouchement"] <- "Delivery room"

# Deal with duplicates ----------------------------------------------------

cat("**Removing Duplicates**")

# drop complete dups (new total = 3474)
workingdata <- unique(workingdata)
# dups within client code/date of touchpoint
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

# Add columns for groups, dtnv, censored ------------------------------------

cat("**Adding New Columns**")

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


workingdata$Date.of.Touchpoint <- as.Date(workingdata$Date.of.Touchpoint,
                                          format = "%m/%d/%Y")

# sort by client code and date
sorteddata <- workingdata[order(factor(workingdata$Client.Code),
                                factor(workingdata$Date.of.Touchpoint)), ]

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

sorteddata$dtnv <- dtnv
sorteddata$censored <- cens

# Save cleaned dataframe --------------------------------------------------

cat("**Saving to csv**")

write.csv(sorteddata, file = "ProVIC_salesforce_cleaned.csv", row.names = FALSE)
