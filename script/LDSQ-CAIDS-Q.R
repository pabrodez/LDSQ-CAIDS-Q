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

# Remove invalid values and substitue with NA for consistency (i.e: "not answered" or "N/A")
library(plyr)
uniqueVals <- function(df1, df2) {
  commonNames <- names(df1)[names(df1) %in% names(df2)]
  list(df1 = sapply(df1[,commonNames], unique),
             df2 = sapply(df2[,commonNames], unique)) 
}
uniqueVals(ad_jun_sub, ad_dec_sub) #check
uniqueVals(ad_dec_sub, ad_jun_sub)

# adults
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



