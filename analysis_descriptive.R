

# Load and explore data ---------------------------------------------------


# total observations = 431340
rawdata <- read.csv("C:/Users/ebeylerian/Dropbox/ProVIC Retention analysis/data/Ben_TP_HIVpos_alltime_allprov_2015.10.22.csv",
                    header = TRUE, stringsAsFactors = FALSE)
doT <- as.Date(rawdata$Date.of.Touchpoint, format = "%m/%d/%Y")
yrmo <- substr(doT, 1, 7)

# start with beginning of 2011 (there are some dates before this)
plotnames <- c()
for(i in 2011:2015){
  plotnames <- append(plotnames, paste(i, c("01", "02", "03", "04", "05", "06",
                                            "07", "08", "09", 10, 11, 12),
                                       sep = "-"))
}


datefactor <- factor(yrmo, levels = plotnames)

# indices and plots for whole dataset
barplot(tabulate(datefactor), main = "All Touchpoints in Dataset",
        xlab = "year-month", names.arg = plotnames[1:57])


## rows with at least some ARV info
hasARVinfo <- which(rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                      rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                      rawdata$ARV.Prophylaxis.Received. != "" |
                      rawdata$ARV.TAR.Received. != "")

barplot(rbind(tabulate(datefactor[hasARVinfo]),
              tabulate(datefactor[-hasARVinfo])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "All Touchpoints in Dataset",
        col = c("#005D6E", "gray"))
legend("topright", legend = c("No ART info recorded", "At least some ART info"),
       fill = c("gray", "#005D6E"))


# totals_arvtype <- rbind(tabulate(datefactor[rawdata$ARV.Type != ""]),
#                         tabulate(datefactor[rawdata$ARV.Type == ""]))
#
# barplot(totals_arvtype, names.arg = plotnames[1:57],
#         main = "All Touchpoints by ARV Type (blank/not blank)")

# indices and plots for Kinshasa only
hasARVinfo_k <- which(rawdata$Beneficiary.Region.Province == "Kinshasa" &
                       (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                        rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                        rawdata$ARV.Prophylaxis.Received. != "" |
                       rawdata$ARV.TAR.Received. != ""))
noARVinfo_k <- which(rawdata$Beneficiary.Region.Province == "Kinshasa" &
                        (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                         rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                         rawdata$ARV.Prophylaxis.Received. == "" &
                         rawdata$ARV.TAR.Received. == ""))

barplot(rbind(tabulate(datefactor[hasARVinfo_k]),
              tabulate(datefactor[noARVinfo_k])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "Kinshasa Touchpoints Only",
        col = c("#005D6E", "gray"))
legend("topright", legend = c("No ART info recorded", "At least some ART info"),
       fill = c("gray", "#005D6E"))

# totals_arvtype_k <- rbind(tabulate(datefactor[(rawdata$ARV.Type != "") &
#                           (rawdata$Beneficiary.Region.Province == "Kinshasa")]),
#                           tabulate(datefactor[(rawdata$ARV.Type == "") &
#                           (rawdata$Beneficiary.Region.Province == "Kinshasa")]))
# barplot(totals_arvtype_k,
#         main = "All Kinshasa Touchpoints by ARV Type (blank/not blank)",
#         names.arg = plotnames[1:57])



# indices and plots for Katanga only
hasARVinfo_kat <- which(rawdata$Beneficiary.Region.Province == "Katanga" &
                        (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                           rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                           rawdata$ARV.Prophylaxis.Received. != "" |
                           rawdata$ARV.TAR.Received. != ""))
noARVinfo_kat <- which(rawdata$Beneficiary.Region.Province == "Katanga" &
                       (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                          rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                          rawdata$ARV.Prophylaxis.Received. == "" &
                          rawdata$ARV.TAR.Received. == ""))

barplot(rbind(tabulate(datefactor[hasARVinfo_kat]),
              tabulate(datefactor[noARVinfo_kat])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "Katanga Touchpoints Only",
        col = c("#005D6E", "gray"))
legend("topright", legend = c("No ART info recorded", "At least some ART info"),
       fill = c("gray", "#005D6E"))


# observations without a start date
# 41648 obs have no start date and no ARV info
nostart_someARV <- which(rawdata$Beginning.Date.of.Treatment == "" &
                           (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                              rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                              rawdata$ARV.Prophylaxis.Received. != "" |
                              rawdata$ARV.TAR.Received. != ""))
nostart_noARV <- which(rawdata$Beginning.Date.of.Treatment == "" &
                           (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                              rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                              rawdata$ARV.Prophylaxis.Received. == "" &
                              rawdata$ARV.TAR.Received. == ""))

barplot(rbind(tabulate(datefactor[nostart_someARV]),
              tabulate(datefactor[nostart_noARV])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "All Touchpoints without 'Beginning Date of Treatment'",
        col = c("#005D6E", "gray"))
legend("topright", legend = c("No ART info recorded", "At least some ART info"),
       fill = c("gray", "#005D6E"))


# observations with a start date
# 216325 obs have a start date and no arv info
withstart_someARV <- which(rawdata$Beginning.Date.of.Treatment != "" &
                           (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                              rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                              rawdata$ARV.Prophylaxis.Received. != "" |
                              rawdata$ARV.TAR.Received. != ""))
withstart_noARV <- which(rawdata$Beginning.Date.of.Treatment != "" &
                         (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                            rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                            rawdata$ARV.Prophylaxis.Received. == "" &
                            rawdata$ARV.TAR.Received. == ""))

barplot(rbind(tabulate(datefactor[withstart_someARV]),
              tabulate(datefactor[withstart_noARV])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "All Touchpoints with 'Beginning Date of Treatment'",
        col = c("#005D6E", "gray"))
legend("topright", legend = c("No ART info recorded", "At least some ART info"),
       fill = c("gray", "#005D6E"))

# of those with ART info, what kind of info do we have?
arvtype <- which(rawdata$ARV.Type != "")
artstart <- which(rawdata$ART.Start != "")
artsit <- which(rawdata$ART.Situation != "")
arvrec <- which(rawdata$ARV.Received. != "")
arvtarrec <- which(rawdata$ARV.TAR.Received. != "")
arvprrec <- which(rawdata$ARV.Prophylaxis.Received. != "")

barplot(rbind(tabulate(datefactor[arvtype]),
              tabulate(datefactor[artstart]),
              tabulate(datefactor[artsit]),
              tabulate(datefactor[arvrec]),
              tabulate(datefactor[arvtarrec]),
              tabulate(datefactor[arvprrec])),
        xlab = "year-month", ylab = "Number of Touchpoints",
        names.arg = plotnames[1:57],
        main = "Types of ART Information Recorded",
        col = c("#9A3E25", "#B37055", "#708259",
                "#95A17E", "#005D6E", "#6BBBA1"))
legend("topleft", legend = c("ARV Type", "ART Start", "ART Situation",
                             "ARV Received?", "ARV/TAR Received?",
                             "ARV Prophylaxis Received?"),
       fill = c("#9A3E25", "#B37055", "#708259",
                "#95A17E", "#005D6E", "#6BBBA1"))


artvars <- c("ARV.Type", "ART.Start", "ART.Situation",
             "ARV.Received.", "ARV.TAR.Received.",
             "ARV.Prophylaxis.Received.")
ndatapoints <- c()
for(i in hasARVinfo){
  n <- 0
  for(av in artvars){
    if(rawdata[i,][[av]] != "") n <- n + 1
  }
  ndatapoints <- append(ndatapoints, n)
}
barplot(table(ndatapoints), main = "ART Data Points per Touchpoint (out of six)",
        ylab = "Number of touchpoints",
        col = "#005D6E")
