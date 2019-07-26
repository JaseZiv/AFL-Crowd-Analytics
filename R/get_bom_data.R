library(tidyverse)
library(rvest)
library(lubridate)



# Function to download zip folders ----------------------------------------

download_obs_file <- function(station, obs_code) {
  tmppath <- paste0(obs_code, "_", station, ".zip")
  if (!file.exists(tmppath)) {
    Sys.sleep(0.1)
    
    
    landing_page_url <- paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=", obs_code, "&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", station)
    landing_page <- read_html(landing_page_url)
    
    zip_link <- html_node(landing_page, "#content-block li+ li a") %>% html_attr("href")
    
    base_url <- "http://www.bom.gov.au"
    
    ziplink <- paste0(base_url, zip_link)
    
    if (ziplink != base_url) {
      download.file(ziplink, tmppath, quiet = TRUE, mode = "wb")
    }
    
    print(station)
    return(tmppath)
    
  }
  
}



# Function to process each weather station --------------------------------

process_file <- function(filename, years) {
  temp_dir <- tempdir()
  unzip(filename, exdir = temp_dir)
  
  datafilename <- dir(temp_dir, pattern = ".csv", full.names = TRUE)
  
  if (length(datafilename) == 1) {
    # read csv
    df <- read.csv(datafilename)
    df <- df[df$Year %in% years, 2:8]
    
    
    if(grepl("136_([0-9]{4,5}).zip", filename)) {
    # Clean up missing rainfall data ------------------------------------------
      vector <- df$Rainfall.amount..millimetres. %>% rev # the following look works in reverse, hence reversing the current rainfall vector
      for (i in 1:length(vector)) {
        
        if(i == 1 & is.na(vector[i])) {
          vector[i] <- mean(vector, na.rm=TRUE) # if the first observation extracted is missing, impute the overall average rainfall for the perid extracted. Should be rare that that's the case
        } else { if(vector[i] %>% is.na(.)) {
          total_rainfall <- vector[i-1] # get the rainfall value for the previous day that wasn't missing 
          
          
          is_it_na_vector <- c()
          for(j in 1:(length(vector)-i+1)) {
            is_it_na_vector <- c(is_it_na_vector, is.na(vector[j+i - 1]))
            if(!is_it_na_vector[j]) {break} # look for all of the remaining values until a non-NA is found
          }
          
          consec_nas <- sum(is_it_na_vector) # count how many missing days there are in the sequence
          
          vector[(i-1):(i-1+consec_nas)] <- total_rainfall/(consec_nas + 1) # then divide the next recorded rainfall amount and disperse over this day and all missing days before it in the sequence
          
        } # End if
        } # End else 
        
      }
      df$rainfall_clean <- vector %>% rev # reverse it back again so it's back to the correct order
    }
    
  } else {
    df <- 0
  }
  # delete csv
  file.remove(dir(temp_dir, full.names = TRUE))
  
  return(df)
}


# Rain Extractions --------------------------------------------------------
# stations <- c(86038, 40764)
years <- c(1999:2019)

stations <- c(023090, 087014, 040913, 031011, 070247, 014015, 040846, 087113, 086232, 009225, 066062, 094029, 091237)

tmp_paths <- lapply(stations, download_obs_file, obs_code = 136)
df <- lapply(dir(pattern = "136_([0-9]{4,5}).zip"), process_file, years = years)
rain_data <- do.call("rbind", df)
rain_data <- rain_data[rain_data$Year != 0, ]
rm(df)
unlink(tmp_paths)



# Preprocessing -----------------------------------------------------------

# because the date the rainfall is recorded on is the day after the actual rainfall, a variable is needed that assigns the
# weather reading to the previous day's record. Use dplyr::lead() for this
rain_data <- rain_data %>% 
  arrange(Bureau.of.Meteorology.station.number, Year, Month, Day) %>% 
  group_by(Bureau.of.Meteorology.station.number) %>% ungroup()


# combine the individual fields into one date field, convert to date field using lubridate::ymd()
rain_data <- rain_data %>% unite("weather_date", c("Year", "Month", "Day"), sep = "-")
rain_data$weather_date <- ymd(rain_data$weather_date)



# put city names against each weather station
stations <- c(023090, 087014, 040913, 031011, 070247, 014015, 040846, 087113, 086232, 009225, 066062, 094029, 091237)
cities <- c("Adelaide", "Ballarat", "Brisbane", "Cairns", "Canberra", "Darwin", "Gold Coast", "Geelong", "Melbourne", "Perth", "Sydney", "Hobart", "Launceston")

station_cities <- cbind(stations, cities) %>% data.frame() %>% mutate(stations = as.numeric(as.character(stations)), cities = as.character(cities))

# join it to the main rain DF
rain_data <- rain_data %>% left_join(station_cities, by = c("Bureau.of.Meteorology.station.number" = "stations"))

rain_data <- rain_data %>% select(-Bureau.of.Meteorology.station.number)

# write file
write.csv(rain_data, "data/cleaned_data/preprocessed_rain_data.csv", row.names = F)



# Max Temp Extractions -----------------------------------------------------
# stations <- c(86038, 40764)
years <- c(1999:2019)

temp_stations <- c(023090, 089002, 040913, 031011, 070351, 014015, 040764, 087113, 086068, 009225, 066062, 094029, 091237)
temp_cities <- c("Adelaide", "Ballarat", "Brisbane", "Cairns", "Canberra", "Darwin", "Gold Coast", "Geelong", "Melbourne", "Perth", "Sydney", "Hobart", "Launceston")

tmp_paths <- lapply(temp_stations, download_obs_file, obs_code = 122)
df <- lapply(dir(pattern = "122_([0-9]{4,5}).zip"), process_file, years = years)
max_temp_data <- do.call("rbind", df)
max_temp_data <- max_temp_data[max_temp_data$Year != 0, ]
rm(df)
unlink(tmp_paths)

# Impute any missing maximum temperatues by taking the mean for that weatherstation and month.
max_temp_data <- max_temp_data %>% 
  group_by(Bureau.of.Meteorology.station.number, Month) %>% 
  mutate(Maximum.temperature..Degree.C. = ifelse(is.na(Maximum.temperature..Degree.C.), mean(Maximum.temperature..Degree.C., na.rm = T), Maximum.temperature..Degree.C.)) %>% 
  ungroup()


# Min Temp Extractions -----------------------------------------------------
# stations <- c(86038, 40764)
years <- c(1999:2019)

tmp_paths <- lapply(temp_stations, download_obs_file, obs_code = 123)
df <- lapply(dir(pattern = "123_([0-9]{4,5}).zip"), process_file, years = years)
min_temp_data <- do.call("rbind", df)
min_temp_data <- min_temp_data[min_temp_data$Year != 0, ]
rm(df)
unlink(tmp_paths)

min_temp_data <- min_temp_data %>% 
  group_by(Bureau.of.Meteorology.station.number, Month) %>% 
  mutate(Minimum.temperature..Degree.C. = ifelse(is.na(Minimum.temperature..Degree.C.), mean(Minimum.temperature..Degree.C., na.rm = T), Minimum.temperature..Degree.C.)) %>% 
  ungroup()


temp_station_cities <- cbind(temp_stations, temp_cities) %>% data.frame() %>% mutate(temp_stations = as.numeric(as.character(temp_stations)), temp_cities = as.character(temp_cities))

temperature_data <- min_temp_data %>% 
  full_join(max_temp_data, by = c("Bureau.of.Meteorology.station.number", "Year", "Month", "Day")) %>% 
  select(Bureau.of.Meteorology.station.number, Year, Month, Day, min_temp = Minimum.temperature..Degree.C., max_temp = Maximum.temperature..Degree.C.)




# combine the individual fields into one date field, convert to date field using lubridate::ymd()
temperature_data <- temperature_data %>% unite("weather_date", c("Year", "Month", "Day"), sep = "-")
temperature_data$weather_date <- ymd(temperature_data$weather_date)



# join it to the main rain DF
temperature_data <- temperature_data %>% left_join(temp_station_cities, by = c("Bureau.of.Meteorology.station.number" = "temp_stations"))

temperature_data <- temperature_data %>% select(-Bureau.of.Meteorology.station.number)

# join the temperature and rain datasets together
weather_data <- rain_data %>% 
  full_join(temperature_data, by = c("weather_date", "cities" = "temp_cities"))

# write file
write.csv(weather_data, "data/cleaned_data/preprocessed_rain_temp_data.csv", row.names = F)

