library(dplyr)
library(rvest)

# obs_code <- 136
# station <- 86038


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



process_file <- function(filename, years) {
  temp_dir <- tempdir()
  unzip(filename, exdir = temp_dir)
  
  datafilename <- dir(temp_dir, pattern = ".csv", full.names = TRUE)
  
  if (length(datafilename) == 1) {
    # read csv
    df <- read.csv(datafilename)
    df <- df[df$Year %in% years, 2:8]
  } else {
    df <- 0
  }
  # delete csv
  file.remove(dir(temp_dir, full.names = TRUE))
  
  return(df)
}


# Extractions -------------------------------------------------------------
stations <- c(86038, 40764)
years <- c(2017:2019)

stations <- 086232

tmp_paths <- lapply(stations, download_obs_file, obs_code = 136)
df <- lapply(dir(pattern = "136_([0-9]{4,5}).zip"), process_file, years = years)
data_rain <- do.call("rbind", df)
data_rain <- data_rain[data_rain$Year != 0, ]
rm(df)
unlink(tmp_paths)

