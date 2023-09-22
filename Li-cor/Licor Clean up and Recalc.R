#cut and spliced from other sources
#complied/created by Kit swift
#parts of code taken from Isobel Mifsud, Laura Bogar, and Ronja Keeley; (thank you :))

library(tidyverse)
library(readxl)
library(lubridate)
library(lme4)
library(lmerTest)
library(stargazer)
library(cowplot)

#create a vector listing all excel files in your directory
#can be changed to match your file name
files <- "Raw Licor Data.xlsx"

#read the first file to create a vector listing the column names
a <- read_excel(path = paste("data/", files[1], sep = ""), skip = 8, col_names = T)

colnames <- colnames(a)

#reading Leaf area + creating vectore to assign name to data
leafareas = read_excel(path = "data/Leaf Area.xlsx")
refplant = leafareas$ID
refdoc = paste(refplant, collapse=", ")


#create an empty dataframe to put the data in
licor <- data.frame(matrix(ncol = length(colnames), nrow = 0))
date = numeric()
entries = numeric()
#loop through all files in directory and bind to new dataframe
for (i in 1:length(files)) {
  data_to_add = read_excel(paste("data/", files[i], sep = ""), skip = 8)
  date = rbind(date, 
               rep(names(read_excel(paste("data/", files[i], sep = ""), skip = 1))[1]))
  entries = rbind(entries, nrow(data_to_add))
  licor <- rbind(licor, data_to_add)
}
colnames(licor) <- colnames
datecolumn = numeric()

for (i in 1:length(date)) {
  datecolumn = c(datecolumn, rep(date[i], entries[i]))
}

licor$date = datecolumn


#create notes column, fix time and obs columns, remove in/out rows
licor <- subset(licor, HHMMSS != "in")
licor <- licor %>%
  mutate(Notes = ifelse(Obs == "Remark=", 
                        substr(HHMMSS, start = 11, stop = nchar(HHMMSS)-3), NA),
         HHMMSS = ifelse(Obs == "Remark=", substr(HHMMSS, 2, 9), HHMMSS),
         Obs = 1:nrow(licor))

simpledate = substr(licor$date, start = 5, stop = 10) # extract just month and day
licor$date = simpledate

#This assigns different plant IDs to data points;it checks for multiple concurrent rows with NA in them in the 
#photo column (indicating notes inbetween tests), and assigns different values to the data inbetween blocks of NA
licor$plantID <- NA*length(licor$Obs)
plantID <- 0
for (i in 1:entries[1,1]){
  if (is.na(licor$Photo[i])) {
    for (j in 1:1){
      if (is.na(licor$Photo[i+1])){
        if (is.na(licor$Photo[i+2])){
        }
        else {
          plantID <- plantID + 1
        }
      }
    }
  }
  else {
    licor$plantID[i] <- plantID
  }
}


#removing the notes rows for an even cleaner data set
licor2 <- licor; licor2$Notes <- NULL
licor2 <- na.omit(licor2)
obs <- seq(from = 1, to = length(licor2$Obs)); licor2$Obs <- obs
docname1 = sprintf("cleaned_data/%s; Cleaned Raw.csv", refdoc)
write.csv(licor2, file = docname1)


#### Read in data ####
rawdata = read_csv(file = docname1)

#### Functions to recompute LICOR values ####\
# start with PHOTO

fda = function(flow, area) {
  output = (flow * 0.000001)/(area*0.0001)
  return(output)
}

photo = function(CO2R, CO2S, H2OR, H2OS, fda){
  output = (CO2R-(CO2S*(1000-H2OR))/(1000-H2OS)) * fda
  return(output)
}

# format dates for merging: raw data
rawdata$date_full = paste(rawdata$date, "2022")
rawdata$date_full = mdy(rawdata$date_full)

# format dates for merging: leaf areas
for (i in 1:nrow(leafareas)) {
  if (nchar(leafareas$Date[i]) < 5) {
    leafareas$Date[i] = paste(leafareas$Date[i], "22", sep = "/")
  }
}

leafareas$date_full = leafareas$Date

uniqueID <- unique(rawdata$plantID)
plantID <- seq(1, length(uniqueID))
leafareas$plantID <- plantID

# Merging raw data with leaf areas

full_data_set = left_join(leafareas, rawdata, by = c("plantID", "date_full"))


#### Recomputing photosynthesis based on leaf area ####

full_data_set = mutate(full_data_set, fda_recomp = fda(Flow, Area.leaf))
full_data_set = mutate(full_data_set, Photo_recomp = photo(CO2R, CO2S, H2OR, H2OS, fda_recomp))

docname2 = sprintf("cleaned_data/%s; Recalculated Full.csv", refdoc)
write.csv(full_data_set, file = docname2)

#outputting only what we need
fpull <- as.vector(c("ID","PARi", "Photo_recomp", "Ci")) 
finalraw <- full_data_set[,fpull]

finalraw <- finalraw %>% 
  rename(
    PAR = PARi,
    Photo = Photo_recomp,
    CI_out = Ci
  )

docname3 = sprintf("cleaned_data/%s; Photo_Par Final.csv", refdoc)
write.csv(finalraw, file = docname3)
