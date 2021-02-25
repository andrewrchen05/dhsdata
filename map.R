library(tidyverse)
library(ggmap)
library(leaflet)
library(dplyr)
library(stringr)
library(tidyr) # for replace_na
require(fuzzyjoin)
library(janitor)

# filter entries of only asian countries
aapi_countries <- aapicountries$asian_country

index <- 0
year = 2007

# import dataframes

repeat {
  
  
  name = paste("lprusatop200_2007-2019/LPR_USA_Counties_", toString(year), "_Top_200_D.txt", sep = "")
  
  temp_year_df = read.delim(name, header = TRUE, stringsAsFactors = FALSE, quote = "", sep = "\t")
  
  aapi_only_temp_df <- temp_year_df[grepl(paste(aapi_countries, collapse="|"), temp_year_df$Country.of.Birth), ]
  
  # create data frame of county and state
  aapicountries_residence <- aapi_only_temp_df[, c("County.of.Residence", "State.of.Residence")]
  # data frame of country of origin
  aapicountries_origin <- data.frame(aapi_only_temp_df[, c("Country.of.Birth")])
  
  # find unique counties of residence
  unique_aapi_residence <- unique(aapicountries_residence[,c("County.of.Residence", "State.of.Residence")])
  # find unique nations of origin
  unique_aapi_origin <- data.frame( unique( aapi_only_temp_df [, c ("Country.of.Birth") ] ) )
  
  # create data frame with county appended to 
  unique_aapi_counties <- data.frame(paste(unique_aapi_residence$County.of.Residence, "County, ", unique_aapi_residence$State.of.Residence ))
  
  # register_google(key = "")
  
  # use Google API to geolocate
  county_plus_coordinates <- mutate_geocode(unique_aapi_counties, Counties)
  birth_plus_coordinates <- mutate_geocode(unique_aapi_origin, Country.of.Birth) 
  
  index <- index + 1
  year <- year + 1
  
  if (index == 1) {
    break
  }
}

# register_google(key = "")


# create new column in main aapi lpr200 file
aapi_only1$County.State <- paste(aapi_only1$County.of.Residence, "County, ", aapi_only1$State.of.Residence )

# merge counties and main aapi file
temp_finalsheet <- subset ( data.frame ( merge( city_coordinates, aapi_only1, by = c( 'County.State') ) ), select = -c(State.of.Residence, County.of.Residence) )

# create dataset with geolocations for county.state and country.of.birth for aapi lprtop200 counties
# col_order <- c("County.State", "lon", "lat", "Country.of.Birth", "origin.lon", "origin.lat", "Major.Class.of.Admission", "Admissions")
finalsheet <- ( merge( temp_finalsheet, birth_plus_coordinates, by = c( 'Country.of.Birth') ) )
finalsheet <- finalsheet %>% select(County.State, lon, lat, Major.Class.of.Admission, Admissions, Country.of.Birth, origin.lon, origin.lat)
colnames(finalsheet) <- c("CountyState", "lon", "lat", "MajorClassAdmission", "Admissions", "CountryofBirth", "originlon", "originlat")

# formatting whitespace
finalsheet$MajorClassAdmission <- gsub(' ', '', finalsheet$MajorClassAdmission)
finalsheet$MajorClassAdmission <- gsub('-', '', finalsheet$MajorClassAdmission)

finalsheet$CountyState <- gsub(",\\s", ",", finalsheet$CountyState)
  
finalsheet$CountyState <- gsub('Saint', 'St.', finalsheet$CountyState)

finalsheet <- left_join(finalsheet, census_GEOID_reference., by=c("CountyState"="NAME"))
# finalsheet <- regex_left_join(finalsheet, census_GEOID_reference., by=c("CountyState"="NAME"), ignore_case=TRUE)

write.csv(finalsheet, "finalsheet.csv", row.names = FALSE)

# exportframe <- newvar[!is.na(newvar$GEOID), ]
