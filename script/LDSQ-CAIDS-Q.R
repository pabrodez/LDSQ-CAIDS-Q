if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./data/raw")) dir.create("./data/raw")

# 1. Read data ------------------------------------------------------------
list.files("./data/raw", pattern = ".xlsx")

library(xlsx)

adult_jun_nov <- read.xlsx("./data/raw/ANON_EDashboard_Jun-Nov2017.xlsx", sheetIndex = 1, startRow = 2, header = TRUE)
child_jun_nov <- read.xlsx("./data/raw/ANON_EDashboard_Jun-Nov2017.xlsx", sheetIndex = 2, startRow = 2, header = TRUE)
adult_dec_may <- read.xlsx("./data/raw/ANON_LDSQ DATA Dec16-May17.xlsx", sheetIndex = 1, header = TRUE)
child_dec_may <- read.xlsx("./data/raw/ANON_LDSQ DATA Dec16-May17.xlsx", sheetIndex = 2, header = TRUE)

# 2. Compare variables ----------------------------------------------------
diff_jun_dec <- setdiff(names(adult_jun_nov), names(adult_dec_may))
diff_dec_jun <- setdiff(names(adult_dec_may), names(adult_jun_nov))  
diff_jun_dec_c <- setdiff(names(child_jun_nov), names(child_dec_may))  
diff_dec_jun_c <- setdiff(names(child_dec_may), names(child_jun_nov))  
# gives a simple view of what names of variables are present in the 1st and not in the second
# criteria: If, after renaming variables, one name shows in one but strictly no in the other, discard; 
# if one name shows in one, not in the other and there is one named in other way, then include and change names

# 2.2. Subset -------------------------------------------------------------
#  For the sake of consistency present vars in one DF and absent in the other should be discarded 
# Exception: "FME.start.time" and "FME.end.time" in jun_nov DF, which can be used to compute a new var of the mins in FME (present in dec_may DF);
# "Learning disability", "Physical.disability", "Mental.health" can be kept in a separate DF. 
# "Learning disability" can be used to match against the LDSQ score.
# Create var FME mins in jun-nov datasets from FME.start.time and FME.end.time
library(lubridate)
adult_jun_nov$FME.mins <- difftime(adult_jun_nov$FME.end.time, adult_jun_nov$FME.start.time, units = "mins")
child_jun_nov$FME.mins <- difftime(child_jun_nov$FME.end.time, child_jun_nov$FME.start.time, units = "mins")

library(tidyverse)

ad_dec_sub <- adult_dec_may[!names(adult_dec_may) %in% diff_dec_jun]
ad_jun_sub <- adult_jun_nov[!names(adult_jun_nov) %in% diff_jun_dec]
ch_dec_sub <- child_dec_may[!names(child_dec_may) %in% diff_dec_jun_c]
ch_jun_sub <- child_jun_nov[!names(child_jun_nov) %in% diff_jun_dec_c]

# Discard rows with empty values in all columns

ad_dec_sub <- ad_dec_sub[rowSums(is.na(ad_dec_sub)) != ncol(ad_dec_sub), ]
ad_jun_sub <- ad_jun_sub[rowSums(is.na(ad_jun_sub)) != ncol(ad_jun_sub), ]
ch_dec_sub <- ch_dec_sub[rowSums(is.na(ch_dec_sub)) != ncol(ch_dec_sub), ]
ch_jun_sub <- ch_jun_sub[rowSums(is.na(ch_jun_sub)) != ncol(ch_jun_sub), ]

# rowdis <- function(x) {
#   x[rowSums(is.na(x)) != ncol(x), ]
# }
# lapply(df_list, rowdis)

# Discard columns with empty values in all columns
ad_dec_sub <- ad_dec_sub[, colSums(is.na(ad_dec_sub)) != nrow(ad_dec_sub)]
ad_jun_sub <- ad_jun_sub[, colSums(is.na(ad_jun_sub)) != nrow(ad_jun_sub)]
ch_dec_sub <- ch_dec_sub[, colSums(is.na(ch_dec_sub)) != nrow(ch_dec_sub)]
ch_jun_sub <- ch_jun_sub[, colSums(is.na(ch_jun_sub)) != nrow(ch_jun_sub)]

compareCols <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  data.frame(Column = commonNames,
             df1 = sapply(df1[,commonNames], class),
             df2 = sapply(df2[,commonNames], class)) 
}
compareCols(ad_dec_sub, ad_jun_sub)
compareCols(ch_dec_sub, ch_jun_sub)


# Remove invalid values and substitue with NA for consistency. Change other values when necessary
library(plyr)
uniqueVals <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  list(df1 = sapply(df1[,commonNames], unique),
             df2 = sapply(df2[,commonNames], unique)) 
}

# 3. Substitue invalid values for NAs. Change other values when necessary--------


uniqueVals(ad_dec_sub, ad_jun_sub) #check

# adults starting on dec
ad_dec_sub$Area.of.residence <- as.factor(gsub(pattern = "Not provided", replacement = NA, as.character(ad_dec_sub$Area.of.residence)))
ad_dec_sub$Relationship.to.alleged.perp. <- as.factor(gsub(pattern = "Other (see comments)", replacement = "Other", as.character(ad_dec_sub$Relationship.to.alleged.perp.)))
ad_dec_sub$Assault.type.1 <- as.factor(gsub(pattern = "Penile anal", replacement = "Penile Anal", as.character(ad_dec_sub$Assault.type.1)))
ad_dec_sub$Assault.type.2 <- as.factor(gsub(pattern = "Penile oral", replacement = "Penile Oral", as.character(ad_dec_sub$Assault.type.2)))
ad_dec_sub$Assault.type.2 <- as.factor(gsub(pattern = "Penile anal", replacement = "Penile Anal", as.character(ad_dec_sub$Assault.type.2)))
ad_dec_sub$Assault.type.4 <- as.factor(gsub(pattern = "digital vaginal", replacement = "Digital Vaginal", as.character(ad_dec_sub$Assault.type.2)))
ad_dec_sub$Referrer <- as.factor(gsub(pattern = "Police Referrals", replacement = "Police", as.character(ad_dec_sub$Referrer)))
ad_dec_sub$Referrer <- as.factor(gsub(pattern = "Self Referrals", replacement = "Self", as.character(ad_dec_sub$Referrer)))
ad_dec_sub$DV.history <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ad_dec_sub$DV.history)))
ad_dec_sub$DV.history <- as.factor(gsub(pattern = "Dash score", replacement = "Yes", as.character(ad_dec_sub$DV.history)))
ad_dec_sub$DASH.Score <- as.factor(gsub(pattern = "Declined", replacement = NA, as.character(ad_dec_sub$DASH.Score)))
ad_dec_sub$DASH.Score <- as.factor(gsub(pattern = "police", replacement = NA, as.character(ad_dec_sub$DASH.Score)))
ad_dec_sub$DASH.Score <- as.factor(gsub(pattern = "Medium", replacement = NA, as.character(ad_dec_sub$DASH.Score)))
ad_dec_sub$Met.on.internet <- as.factor(gsub(pattern = "NO", replacement = "No", as.character(ad_dec_sub$Met.on.internet)))
ad_dec_sub$Met.on.internet <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ad_dec_sub$Met.on.internet)))
ad_dec_sub$Met.on.internet <- as.factor(gsub(pattern = "YES", replacement = "Yes", as.character(ad_dec_sub$Met.on.internet)))
ad_dec_sub$Met.on.internet <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ad_dec_sub$Met.on.internet)))

ad_dec_sub$No..of.perps. <- ifelse(ad_dec_sub$No..of.perps. > 1, c("Multiple perps"), c("Single perp")) 
ad_dec_sub$No..of.perps. <- as.factor(ad_dec_sub$No..of.perps.)

ad_dec_sub$Emergency.contraception <- as.factor(gsub(ad_dec_sub$Emergency.contraception, pattern = "N/A", replacement = NA))
ad_dec_sub$HIV.PEP <- as.factor(gsub(ad_dec_sub$HIV.PEP, pattern = "N/A", replacement = NA))
ad_dec_sub$Hep.B <- as.factor(gsub(ad_dec_sub$Hep.B, pattern = "N/A", replacement = NA))

# adults starting on jun
ad_jun_sub$Gender <- as.factor(gsub(pattern = "female", replacement = "Female", as.character(ad_jun_sub$Gender)))
ad_jun_sub$Area.of.residence <- as.factor(gsub(pattern = "No fixed abode", replacement = "No fixed above", as.character(ad_jun_sub$Area.of.residence)))
# ad_jun_sub$DASH.Score is dicotomical: >14 or <14, or "Police DASH", which means Police hold the score, not known.
# change ad_dec_sub$DASH.Score to >14 or <14 categories for consistency
# Issue: there are values of 14 in ad_dec_sub$DASH.Score. Because d_jun_sub$DASH.Score is >14 or <14 there's no inclusion of value 14 in any of both categories, leaving the value outside.
# Check presence of value 14 in ad_dec_sub$DASH.Score
sort(summary(ad_dec_sub$DASH.Score), decreasing = TRUE) # there are 8 values of 14 and 328 NA's
ad_jun_sub$DASH.Score <- as.factor(gsub(pattern = "Police DASH", replacement = NA, as.character(ad_jun_sub$DASH.Score)))
ad_dec_sub$DASH.Score <- as.factor(ifelse(as.numeric(as.character(ad_dec_sub$DASH.Score)) >= 14, c(">=14"), c("<14")))
# Create empty levels on both factors for compability:
ad_jun_sub$DASH.Score <- factor(ad_jun_sub$DASH.Score, levels = c(levels(ad_jun_sub$DASH.Score), ">=14"))
ad_dec_sub$DASH.Score <- factor(ad_dec_sub$DASH.Score, levels = c(levels(ad_dec_sub$DASH.Score), ">14"))

ad_jun_sub$Ethnicity <- as.factor(gsub(pattern = "white - British", replacement = "White - British", as.character(ad_jun_sub$Ethnicity)))
ad_jun_sub$Religion <- as.factor(gsub(pattern = "none", replacement = "None", as.character(ad_jun_sub$Religion)))
ad_jun_sub$Religion <- as.factor(gsub(pattern = "none", replacement = "None", as.character(ad_jun_sub$Religion)))
ad_jun_sub$Met.on.internet <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ad_jun_sub$Met.on.internet)))
ad_jun_sub$Met.on.internet <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ad_jun_sub$Met.on.internet)))
# Typo in data entry: "Vaginal rape" in ad_jun_sub$Met.on.internet
table(ad_jun_sub$Met.on.internet)
ad_jun_sub$Met.on.internet <- as.factor(gsub(pattern = "Vaginal rape", replacement = NA, as.character(ad_jun_sub$Met.on.internet)))
ad_jun_sub$Assault.type.2 <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ad_jun_sub$Assault.type.2)))
ad_jun_sub$Strangulation <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ad_jun_sub$Strangulation)))
ad_jun_sub$Strangulation <- as.factor(gsub(pattern = "Notrecorded in Notes", replacement = NA, as.character(ad_jun_sub$Strangulation)))
ad_jun_sub$Strangulation <- as.factor(gsub(pattern = "Not kNown by client", replacement = "Unkown", as.character(ad_jun_sub$Strangulation)))
ad_jun_sub$Sex.Worker <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ad_jun_sub$Sex.Worker)))
ad_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ad_jun_sub$Emergency.contraception)))
ad_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "not required", replacement = "Not required", as.character(ad_jun_sub$Emergency.contraception)))
ad_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "Not documented", replacement = NA, as.character(ad_jun_sub$Emergency.contraception)))
ad_jun_sub$HIV.PEP <- as.factor(gsub(pattern = "Not documented", replacement = NA, as.character(ad_jun_sub$HIV.PEP)))
ad_jun_sub$Hep.B <- as.factor(gsub(pattern = "Not documented", replacement = NA, as.character(ad_jun_sub$Hep.B)))
ad_jun_sub$Hep.B <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ad_jun_sub$Hep.B)))
ad_jun_sub$Referrer <- as.factor(gsub(pattern = "self", replacement = "Self", as.character(ad_jun_sub$Referrer)))
ad_jun_sub$Referrer <- as.factor(gsub(pattern = "police", replacement = "Police", as.character(ad_jun_sub$Referrer)))
ad_jun_sub$LDSQ.. <- as.factor(gsub(pattern = "declined", replacement = "Not done", as.character(ad_jun_sub$LDSQ..)))  # declined is a reason why it wasn't done, but not done doesn't imply it was declined

# Test this.
# Could use this to minimize code using gsub:
# gsubFactor <- function(x, pattern, replacement) {
# for(i in 1:length(pattern))
# y <- as.factor(gsub(
#     pattern[i] ,
#     replacement[i],
#     as.character(x)))
#   y
# }
# gsubFactorMult <- function(x, pattern, replacement) {
#   for(i in 1:length(pattern))
#     x <- gsub(pattern[i], replacement[i], x)
#   x
# }
# In 1st by using lexical scope would only need to gsubFactor(x, pattern, replacement), but in 2nd should be x$y <- gsubFactorMult(x, pattern, replacement)
# Using sapply(x, tolower) would do most of the substitution job
uniqueVals(ch_dec_sub, ch_jun_sub)  # check

# children starting on dec
ch_dec_sub$Referrer <- as.factor(gsub(pattern = "Police Referrals", replacement = "Police", as.character(ch_dec_sub$Referrer)))
ch_dec_sub$Referrer <- as.factor(gsub(pattern = "Self Referrals", replacement = "Self", as.character(ch_dec_sub$Referrer)))

ch_dec_sub$Assault.type.1 <- as.factor(gsub(pattern = "Penile anal", replacement = "Penile Anal", as.character(ch_dec_sub$Assault.type.1)))
ch_dec_sub$Assault.type.1 <- as.factor(gsub(pattern = "Penile vaginal", replacement = "Penile Vaginal", as.character(ch_dec_sub$Assault.type.1)))

ch_dec_sub$Assault.type.2 <- as.factor(gsub(pattern = "Penile anal", replacement = "Penile Anal", as.character(ch_dec_sub$Assault.type.2)))
ch_dec_sub$Assault.type.2 <- as.factor(gsub(pattern = "Digital anal", replacement = "Digital Anal", as.character(ch_dec_sub$Assault.type.2)))

ch_dec_sub$No..of.perps. <- ifelse(as.numeric(as.character(ch_dec_sub$No..of.perps.)) > 1, c("Multiple perps"), c("Single perp"))
ch_dec_sub$No..of.perps. <- as.factor(ch_dec_sub$No..of.perps.)

ch_dec_sub$Relationship.to.alleged.perp. <- as.factor(gsub(pattern = "Acquaitnace > 24 hours", replacement = "Acquaintance > 24 hours", as.character(ch_dec_sub$Relationship.to.alleged.perp.)))

ch_dec_sub$Met.on.internet <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_dec_sub$Met.on.internet)))

ch_dec_sub$Emergency.contraception <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_dec_sub$Emergency.contraception)))
ch_dec_sub$Emergency.contraception <- as.factor(gsub(pattern = "Unknown", replacement = NA, as.character(ch_dec_sub$Emergency.contraception)))

ch_dec_sub$HIV.PEP <- as.factor(gsub(pattern = "N/a", replacement = NA, as.character(ch_dec_sub$HIV.PEP)))
ch_dec_sub$HIV.PEP <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_dec_sub$HIV.PEP)))
ch_dec_sub$HIV.PEP <- as.factor(gsub(pattern = "Unknown", replacement = NA, as.character(ch_dec_sub$HIV.PEP)))

ch_dec_sub$Hep.B <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_dec_sub$Hep.B)))
ch_dec_sub$Hep.B <- as.factor(gsub(pattern = "Unknown", replacement = NA, as.character(ch_dec_sub$Hep.B)))

ch_dec_sub$U.16.DVD <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_dec_sub$U.16.DVD)))
ch_dec_sub$U.16.DVD <- as.factor(gsub(pattern = "Unknown", replacement = NA, as.character(ch_dec_sub$U.16.DVD)))

ch_dec_sub$CSE.CSE.risk <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_dec_sub$CSE.CSE.risk)))

# children starting on jun
ch_jun_sub$FME.context <- as.factor(gsub(pattern = "acute", replacement = "Acute", as.character(ch_jun_sub$FME.context)))

ch_jun_sub$Gender <- as.factor(gsub(pattern = "female", replacement = "Female", as.character(ch_jun_sub$Gender)))
ch_jun_sub$Gender <- as.factor(gsub(pattern = "male", replacement = "Male", as.character(ch_jun_sub$Gender)))

ch_jun_sub$Ethnicity <- as.factor(gsub(pattern = "white - British", replacement = "White - British", as.character(ch_jun_sub$Ethnicity)))
ch_jun_sub$Ethnicity <- as.factor(gsub(pattern = "Not known", replacement = NA, as.character(ch_jun_sub$Ethnicity)))

ch_jun_sub$Religion <- as.factor(gsub(pattern = "none", replacement = "None", as.character(ch_jun_sub$Religion)))

ch_jun_sub$DV.history <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ch_jun_sub$DV.history)))
ch_jun_sub$DV.history <- as.factor(gsub(pattern = "Unknown", replacement = NA, as.character(ch_jun_sub$DV.history)))
ch_jun_sub$DV.history <- as.factor(gsub(pattern = "UnkNown", replacement = NA, as.character(ch_jun_sub$DV.history)))

ch_jun_sub$CSE.CSE.risk <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ch_jun_sub$CSE.CSE.risk)))
ch_jun_sub$CSE.CSE.risk <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_jun_sub$CSE.CSE.risk)))
ch_jun_sub$CSE.CSE.risk <- as.factor(gsub(pattern = "UnkNown", replacement = NA, as.character(ch_jun_sub$CSE.CSE.risk)))

ch_jun_sub$Relationship.to.alleged.perp. <- as.factor(gsub(pattern = "brother", replacement = "Brother", as.character(ch_jun_sub$Relationship.to.alleged.perp.)))
ch_jun_sub$Relationship.to.alleged.perp. <- as.factor(gsub(pattern = "father", replacement = "Father", as.character(ch_jun_sub$Relationship.to.alleged.perp.)))
ch_jun_sub$Relationship.to.alleged.perp. <- as.factor(gsub(pattern = "stranger", replacement = "Stranger", as.character(ch_jun_sub$Relationship.to.alleged.perp.)))

ch_jun_sub$Met.on.internet <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ch_jun_sub$Met.on.internet)))
ch_jun_sub$Met.on.internet <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_jun_sub$Met.on.internet)))

ch_jun_sub$Assault.type.2 <- as.factor(gsub(pattern = "oral rape", replacement = "Oral rape", as.character(ch_jun_sub$Assault.type.2)))
ch_jun_sub$Assault.type.2 <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_jun_sub$Assault.type.2)))

ch_jun_sub$Assault.type.3 <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_jun_sub$Assault.type.3)))
ch_jun_sub$Assault.type.3 <- as.factor(gsub(pattern = "anal rape", replacement = "Anal rape", as.character(ch_jun_sub$Assault.type.3)))

ch_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "anal rape", replacement = "Anal rape", as.character(ch_jun_sub$Emergency.contraception)))
ch_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "already had", replacement = "Yes", as.character(ch_jun_sub$Emergency.contraception)))
ch_jun_sub$Emergency.contraception <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_jun_sub$Emergency.contraception)))

ch_jun_sub$HIV.PEP <- as.factor(gsub(pattern = "not required", replacement = "Not required", as.character(ch_jun_sub$HIV.PEP)))
ch_jun_sub$HIV.PEP <- as.factor(gsub(pattern = "Not documented", replacement = NA, as.character(ch_jun_sub$HIV.PEP)))

ch_jun_sub$Hep.B <- as.factor(gsub(pattern = " Declined", replacement = "Declined", as.character(ch_jun_sub$Hep.B)))
ch_jun_sub$Hep.B <- as.factor(gsub(pattern = "Not", replacement = "No", as.character(ch_jun_sub$Hep.B)))
ch_jun_sub$Hep.B <- as.factor(gsub(pattern = "Not documented", replacement = NA, as.character(ch_jun_sub$Hep.B)))
ch_jun_sub$Hep.B <- as.factor(gsub(pattern = "No documented", replacement = NA, as.character(ch_jun_sub$Hep.B)))

ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "dvd", replacement = NA, as.character(ch_jun_sub$U.16.DVD)))
ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "n/a", replacement = NA, as.character(ch_jun_sub$U.16.DVD)))
ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_jun_sub$U.16.DVD)))
ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "yes", replacement = "Yes", as.character(ch_jun_sub$U.16.DVD)))
ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "yES", replacement = "Yes", as.character(ch_jun_sub$U.16.DVD)))
ch_jun_sub$U.16.DVD <- as.factor(gsub(pattern = "no", replacement = "No", as.character(ch_jun_sub$U.16.DVD)))

ch_jun_sub$Referrer <- as.factor(gsub(pattern = "POLICE", replacement = "Police", as.character(ch_jun_sub$Referrer)))
ch_jun_sub$Referrer <- as.factor(gsub(pattern = "police", replacement = "Police", as.character(ch_jun_sub$Referrer)))

ch_jun_sub$CAIDSQ.. <- as.factor(gsub(pattern = "N/A", replacement = NA, as.character(ch_jun_sub$CAIDSQ..)))
ch_jun_sub$CAIDSQ.. <- as.factor(gsub(pattern = "not done", replacement = "Not done", as.character(ch_jun_sub$CAIDSQ..)))

# Function to change to lowcase all characters in DF
# lapply(df, function(x) {
#   if (is.character(x)) return(tolower(x))
#   else return(v)
# })

# 4. Rbind DFs ------------------------------------------------------------
# fix mistake:
setdiff(names(ad_dec_sub), names(ad_jun_sub))
setdiff(names(ad_jun_sub), names(ad_dec_sub)) 
ad_dec_sub$FME.mins <- NULL
ad_jun_sub$FME.mins <- NULL
setdiff(names(ch_dec_sub), names(ch_jun_sub))  
setdiff(names(ch_jun_sub), names(ch_dec_sub)) 
ch_dec_sub$Assault.type.4 <- NA
ch_jun_sub$FME.mins <- NULL

adultDF <- rbind(ad_dec_sub, ad_jun_sub)
childDF <- rbind(ch_dec_sub, ch_jun_sub)

sapply(adultDF[, names(adultDF)], class)
sapply(childDF[, names(childDF)], class)
childDF$Assault.type.4 <- factor(childDF$Assault.type.4)

sapply(adultDF[, names(adultDF)], unique)
sapply(childDF[, names(childDF)], unique)

# correction:
childDF$Gender <- as.factor(gsub(pattern = "FeMale", replacement = "Female", as.character(childDF$Gender)))

# 5. Explore and map missing values ---------------------------------------
library(data.table)
library(tidyverse)
# divide between chategorial and numeric vars. Keep date aside
catVarAd <- names(adultDF)[which(sapply(adultDF, is.factor))]
numVarAd <- names(adultDF)[which(sapply(adultDF, is.numeric))]

catVarCh <- names(childDF)[which(sapply(childDF, is.factor))]
numVarCh <- names(childDF)[which(sapply(childDF, is.numeric))]

# check for NAs
colSums(sapply(adultDF, is.na))  # Assult type 2, 3 and 4 are not needed to be answered. While the 1st is usually answered the others can be if there are multiple types  
colSums(sapply(childDF, is.na))  # Same as above

# Using the vector for cat and num vars we could be specific
colSums(sapply(as.data.table(adultDF)[,.SD, .SDcols = catVarAd], is.na))
colSums(sapply(as.data.table(adultDF)[,.SD, .SDcols = numVarAd], is.na))

colSums(sapply(as.data.table(childDF)[,.SD, .SDcols = catVarCh], is.na))
colSums(sapply(as.data.table(childDF)[,.SD, .SDcols = numVarCh], is.na))

# Visualize map of NAs
plotNa <- function(dataFrame, title = NULL) {
  tempDf <- as.data.frame(ifelse(is.na(dataFrame), 0, 1))
  tempDf <- tempDf[, order(colSums(tempDf))]
  tempData <- expand.grid(list(x = 1:nrow(tempDf), y = colnames(tempDf)))
  tempData$v <- as.vector(as.matrix(tempDf))
  tempData <- data.frame(x = unlist(tempData$x), y = unlist(tempData$y), v = unlist(tempData$v))
  ggplot(tempData) + geom_tile(aes(x=x, y=y, fill=factor(v))) +
    scale_fill_manual(values=c("white", "black"), name="Missing value\n1=No, 0=Yes") +
    theme_light() + ylab("") + xlab("Rows of data set") + ggtitle(title)
  
}

plotNa(adultDF)
ggsave("NAs adult data-set.png", plot = last_plot(), device = "png", path = "./plots")
plotNa(childDF)
ggsave("NAs children data-set.png", plot = last_plot(), device = "png", path = "./plots")

# percentage of missing values in each variable
sapply(adultDF, function(x) sum(is.na(x)) / nrow(adultDF))
sapply(childDF, function(x) sum(is.na(x)) / nrow(childDF))

# check duplicated rows
sum(duplicated(adultDF))
sum(duplicated(childDF))
childDF[duplicated(childDF), ]  # they don't seem as strict duplicates: Different dates.


# 6. Summary statistics ---------------------------------------------------
# rmarkdown::render("./reports/Week 5-11 Feb/Report 5-11 Feb.Rmd", "pdf_document")

# Summarize categorical data
summary(as.data.table(adultDF)[, .SD, .SDcols = catVarAd])
summary(as.data.table(childDF)[, .SD, .SDcols = catVarCh])

# Summarize numeric data
summary(as.data.table(adultDF)[, .SD, .SDcols = numVarAd])
summary(as.data.table(childDF)[, .SD, .SDcols = numVarCh])

# Assign categ and num vars to DFs
adult_cat <- as.data.table(adultDF)[,.SD, .SDcols = catVarAd]
adult_num <- as.data.table(adultDF)[,.SD, .SDcols = numVarAd]
child_cat <- as.data.table(childDF)[,.SD, .SDcols = catVarCh]
child_num <- as.data.table(childDF)[,.SD, .SDcols = numVarCh]

# Functions to plot
library(gridExtra)
library(e1071)  

plotHist <- function(input, i) {
  data <- data.frame(x=input[[i]])
  his <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(input)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (his)
}


density_glot <- function(input, i){
  data <- data.frame(x=input[[i]])
  plot <- ggplot(data = data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(input)[i]), '\n', 'Skewness: ',round(skewness(input[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(suppressWarnings(plot))
  
}

grid_plot <- function(input, fun, ii, ncol=2) {
  plot_list <- list()
  for (i in ii) {
    plot <- fun(input=input, i=i)
    plot_list <- c(plot_list, list(plot))
  }
  do.call("grid.arrange", c(plot_list, ncol=ncol))
}
# Cat vars adults
grid_plot(adult_cat, plotHist, c(1, 3, 4))
grid_plot(adult_cat, plotHist, 5:8, ncol = 1)
grid_plot(adult_cat, plotHist, 9:12)
grid_plot(adult_cat, plotHist, 13:16)
grid_plot(adult_cat, plotHist, 16:20)

# Cat vars children
grid_plot(child_cat, plotHist, c(1, 2, 4))
grid_plot(child_cat, plotHist, 5:8)
grid_plot(child_cat, plotHist, 9:13)
grid_plot(child_cat, plotHist, 14:17)
grid_plot(child_cat, plotHist, 18:19)

# Num vars adults
grid_plot(adult_num, density_glot, ii = 1)

# Num vars chil
grid_plot(child_num, density_glot, ii= 1)














