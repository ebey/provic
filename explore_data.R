

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
someARVinfo <- which(rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                       rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                       rawdata$ARV.Prophylaxis.Received. != "")

totals_arvtype <- rbind(tabulate(datefactor[rawdata$ARV.Type != ""]),
                        tabulate(datefactor[rawdata$ARV.Type == ""]))

barplot(tabulate(datefactor), main = "All Touchpoints in Dataset",
        xlab = "year-month", names.arg = plotnames[1:57])

barplot(rbind(tabulate(datefactor[someARVinfo]),
              tabulate(datefactor[-someARVinfo])),
        xlab = "year-month", names.arg = plotnames[1:57],
        main = "All Touchpoints in Dataset without/with ARV/ART info")

barplot(totals_arvtype, names.arg = plotnames[1:57],
        main = "All Touchpoints by ARV Type (blank/not blank)")

# indices and plots for Kinshasa only
someARVinfo_k <- which(rawdata$Beneficiary.Region.Province == "Kinshasa" &
                       (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                        rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                        rawdata$ARV.Prophylaxis.Received. != ""))
noARVinfo_k <- which(rawdata$Beneficiary.Region.Province == "Kinshasa" &
                        (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                         rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                         rawdata$ARV.Prophylaxis.Received. == ""))

totals_arvtype_k <- rbind(tabulate(datefactor[(rawdata$ARV.Type != "") &
                          (rawdata$Beneficiary.Region.Province == "Kinshasa")]),
                          tabulate(datefactor[(rawdata$ARV.Type == "") &
                          (rawdata$Beneficiary.Region.Province == "Kinshasa")]))

barplot(rbind(tabulate(datefactor[someARVinfo_k]),
              tabulate(datefactor[noARVinfo_k])),
        xlab = "year-month", names.arg = plotnames[1:57],
        main = "All Kinshasa Touchpoints without/with ARV/ART info")
barplot(totals_arvtype_k,
        main = "All Kinshasa Touchpoints by ARV Type (blank/not blank)",
        names.arg = plotnames[1:57])


# observations without a start date
# 41648 obs have no start date and no ARV info
nostart_someARV <- which(rawdata$Beginning.Date.of.Treatment == "" &
                           (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                              rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                              rawdata$ARV.Prophylaxis.Received. != ""))
nostart_noARV <- which(rawdata$Beginning.Date.of.Treatment == "" &
                           (rawdata$ARV.Type == "" & rawdata$ART.Start == "" &
                              rawdata$ART.Situation == "" & rawdata$ARV.Received. == "" &
                              rawdata$ARV.Prophylaxis.Received. == ""))

barplot(rbind(tabulate(datefactor[nostart_someARV]),
              tabulate(datefactor[nostart_noARV])),
        names.arg = plotnames[1:57],
        main = "Touchpoints without start date, colored by without/with ARV info")


# observations with a start date
# 0 obs have a start date and no arv info
withstart_someARV <- which(rawdata$Beginning.Date.of.Treatment != "" &
                           (rawdata$ARV.Type != "" | rawdata$ART.Start != "" |
                              rawdata$ART.Situation != "" | rawdata$ARV.Received. != "" |
                              rawdata$ARV.Prophylaxis.Received. != ""))
withstart_noARV <- which(rawdata$Beginning.Date.of.Treatment != "" &
                         (rawdata$ARV.Type == "" & rawdata$ART.Start != "" &
                            rawdata$ART.Situation != "" & rawdata$ARV.Received. != "" &
                            rawdata$ARV.Prophylaxis.Received. != ""))
