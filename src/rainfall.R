
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------- Weather Data -----------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in data ------------------------------------------------------------

# data collected from here: http://www.bom.gov.au/climate/data/

files <- list.files("data")[str_detect(list.files("data"),"_rain_Data")]

rain_data <- data.frame()

for(i in files) {
  each_file <- read_csv(paste0("data/", i))
  each_file$location <- gsub("_.*", "", i)
  rain_data <- bind_rows(rain_data, each_file)
}

# want to keep everything from 1999 to add the lead() weather reading below. Then I will filter out only up to the 2000 season
rain_data <- rain_data %>% filter(Year >= 1999)


# because the date the rainfall is recorded on is the day after the actual rainfall, a variable is needed that assigns the
# weather reading to the previous day's record. Use dplyr::lead() for this
rain_data <- rain_data %>% 
  arrange(location, Year, Month, Day) %>% 
  group_by(location) %>% 
  mutate(actual_days_rain = lead(`Rainfall amount (millimetres)`)) %>% ungroup()


# combine the individual fields into one date field, convert to date field using lubridate::ymd()
rain_data <- rain_data %>% unite("weather_date", c("Year", "Month", "Day"), sep = "-")
rain_data$weather_date <- ymd(rain_data$weather_date)

# filter out only required data
rain_data <- rain_data %>% filter(weather_date >= '2000-01-01')

write.csv(rain_data, "data/cleaned_data/preprocessed_rain_data.csv", row.names = F)


#--------- NOTES: ----------#

# 1. there are NAs in the rainfal reading - that's because in some instances rainfall isn't collected every day, but then
# in subsequent days it is, and how many days a reading wasn't collected for is noted in the 'Period over which rainfall was measured (days)' variable.
# This needs to be fixed for accuracy


