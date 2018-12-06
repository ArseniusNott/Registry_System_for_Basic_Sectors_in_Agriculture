# Registry System for Basic Sectors in Agriculture
# Farm Laborers by province, sex, kind of work

# Data Source:
# https://data.gov.ph/dataset/registry-system-basic-sectors-agriculture/resource/5ce92de6-63b0-46ea-8bc6-ccdc704e1b36
# 

# Goals:
# 1. To create an interactive map that shows number of farm workers conditioned 
#    on province, sex, and kind of work

# libraries
library(dplyr)

# data download
setwd("~/Projects/Courses/Coursera/Developing Data Products/Project_3/Registry_System_for_Basic_Sectors_in_Agriculture/")
download.file(url = "https://data.gov.ph/node/1037/download", 
              destfile = "farmlaborerssexkindofworkasfarmlaborerprov.csv", method = "w")


farmer.data <- read.csv("./farmlaborerssexkindofworkasfarmlaborerprov.csv")
head(farmer.data)
str(farmer.data)

# change column names
colnames(farmer.data)
new.col.names <- c("province", "kind_of_work", "total_farm_laborers", 
                   "male_farm_laborers", "female_farm_laborers", 
                   "total_farm_laborers_only", "male_farm_laborers_only", 
                   "female_farm_laborers_only", 
                   "total_farm_laborers_and_fishermen",
                   "male_farm_laborers_and_fishermen", 
                   "female_farm_laborers_and_fishermen")
names(farmer.data) <- new.col.names
head(farmer.data)
str(farmer.data)

# add lat long columns for each province
# manual search and input
unique(farmer.data$province)

return.latlong <- function(type = "lat", province) {
  lat <- NA
  long <- NA
  if(province == "Abra") {
    lat <- 17.5693
    long <- 120.8039
  } else if(province == "Agusan del Sur") {
    lat <- 8.5055
    long <- 125.7407
  } else if(province == "Camarines Sur") {
    lat <- 13.525
    long <- 123.3486
  } else if(province == "Davao Oriental") {
    lat <- 7.0806
    long <- 126.1763
  } else if(province == "Eastern Samar") {
    lat <- 12.1282
    long <- 125.3027
  } else if(province == "Ifugao") {
    lat <- 16.8331
    long <- 121.171
  } else if(province == "Kalinga") {
    lat <- 17.1261
    long <- 121.7088
  } else if(province == "Masbate") {
    lat <- 12.306
    long <- 123.5589
  } else if(province == "Mountain Province") {
    lat <- 17.0663
    long <- 121.0335
  } else if(province == "North Cotabato") {
    lat <- 7.2167
    long <- 124.25
  } else if(province == "Northern Samar") {
    lat <- 12.3613
    long <- 124.7741
  } else if(province == "Romblon") {
    lat <- 12.5294
    long <- 122.2881
  } else if(province == "Western Samar") {
    lat <- 11.9325
    long <- 125.0388
  } else if(province == "Siquijor") {
    lat <- 9.2133
    long <- 123.5157
  } else if(province == "Sultan Kudarat") {
    lat <- 6.5069
    long <- 124.4198
  } else if(province == "Surigao del Norte") {
    lat <- 9.5148
    long <- 125.697
  } else if(province == "Surigao del Sur") {
    lat <- 8.995
    long <- 126.0023
  } else if(province == "Zamboanga del Norte") {
    lat <- 7.0243
    long <- 122.1889
  } else if(province == "Sarangani") {
    lat <- 5.9267
    long <- 124.9948
  } else if(province == "Apayao") {
    lat <- 18.012
    long <- 121.171
  }
  
  if(type == "lat") {
    return(lat)
  } else {
    return(long)
  }
}

for(i in 1:dim(farmer.data)[1]) {
  province.name <- as.character(farmer.data$province[i])
  
  lat <- return.latlong(type = "lat", province = province.name)
  long <- return.latlong(type = "long", province = province.name)
  
  farmer.data[i, "lat"] <- lat
  farmer.data[i, "lng"] <- long
}

# save dataset to new csv
write.csv(x = farmer.data, file = "farm_laborers.csv")
