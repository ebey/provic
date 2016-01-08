
prep.data <- function(province, start.date, end.date){

  # Load cleaned data -------------------------------------------------------

  cleandata <- readRDS(paste0(prov, "_cleaned.rds"))

  # Subset observations -----------------------------------------------------

  doT <- as.Date(cleandata$Date.of.Touchpoint, format = "%m/%d/%Y")
  prov <- province
  start.date <- start.date
  end.date <- end.date

  # subset observations to those from correct province, timespan,
  # and where ART was received
  workingdata <- cleandata[doT >= start.date & doT <= end.date, ]
  workingdata <- workingdata[workingdata$Beneficiary.Region.Province == prov, ]
  ## find ART visits
  workingdata <- workingdata[(workingdata$ARV.TAR.Received. != "" |
                                workingdata$ARV.Type != "" |
                                workingdata$ART.Start != "" |
                                workingdata$ART.Situation != "" |
                                workingdata$ARV.Received. != "" |
                                workingdata$ARV.Prophylaxis.Received. != ""), ]
  workingdata$Date.of.Touchpoint <- as.Date(workingdata$Date.of.Touchpoint,
                                            format = "%m/%d/%Y")
  workingdata$Beginning.Date.of.Treatment <- as.Date(workingdata$Beginning.Date.of.Treatment,
                                                     format = "%m/%d/%Y")

# Save prepped data -------------------------------------------------------

  saveRDS(workingdata, file = paste0(prov, "_prepped.rds"))
}