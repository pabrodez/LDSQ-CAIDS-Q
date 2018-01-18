if (!dir.exists("./data")) dir.create("./data")
library(tidyverse)
library(readxl)
list.files("./data/raw")
adult_jun_nov <- read_xlsx("./data/raw/ANON_EDashboard_Jun-Nov2017.xlsx", sheet = 1)
child_jun_nov <- read_xlsx("./data/raw/ANON_EDashboard_Jun-Nov2017.xlsx", sheet = 2)
adult_dec_feb <- read_xlsx("./data/raw/ANON_LDSQ DATA Dec16-Feb 17.xlsx", sheet = 1)
child_dec_feb <- read_xlsx("./data/raw/ANON_LDSQ DATA Dec16-Feb 17.xlsx", sheet = 2)

# compare vars
names_dec_feb <- names(adult_dec_feb)
names_jun_nov <- names(adult_jun_nov)
names <- cbind("dec_feb" = sort(names_dec_feb), "jun_nov" = sort(names_jun_nov))
