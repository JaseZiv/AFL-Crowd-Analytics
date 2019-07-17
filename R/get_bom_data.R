library(dplyr)
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


# Extractions -------------------------------------------------------------
# stations <- c(86038, 40764)
years <- c(1999:2019)

stations <- c(023090, 040913, 040849, 087184, 086232, 009225, 066062, 096046)

tmp_paths <- lapply(stations, download_obs_file, obs_code = 136)
df <- lapply(dir(pattern = "136_([0-9]{4,5}).zip"), process_file, years = years)
rain_data <- do.call("rbind", df)
rain_data <- data_rain[data_rain$Year != 0, ]
rm(df)
unlink(tmp_paths)



# Preprocessing -----------------------------------------------------------

# because the date the rainfall is recorded on is the day after the actual rainfall, a variable is needed that assigns the
# weather reading to the previous day's record. Use dplyr::lead() for this
rain_data <- rain_data %>% 
  arrange(Bureau.of.Meteorology.station.number, Year, Month, Day) %>% 
  group_by(Bureau.of.Meteorology.station.number) %>% 
  mutate(actual_days_rain = lead(rainfall_clean)) %>% ungroup()


# combine the individual fields into one date field, convert to date field using lubridate::ymd()
rain_data <- rain_data %>% unite("weather_date", c("Year", "Month", "Day"), sep = "-")
rain_data$weather_date <- ymd(rain_data$weather_date)

# filter out only required data
rain_data <- rain_data %>% filter(weather_date >= '2000-01-01')

write.csv(rain_data, "data/cleaned_data/preprocessed_rain_data.csv", row.names = F)






