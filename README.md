# AFL-Crowd-Analytics

This project was created to build a machine learning model to predict Australian Football League (AFL) attendances using historical data.

Two articles have been posted to Medium:

* [Using Data To Determine Which AFL Fans Jump On The Bandwagon](https://medium.com/@jaseziv83/using-data-to-determine-which-afl-fans-jump-on-the-bandwagon-8adbcc4160b0?sk=b4c0788b1f54aca1cfd50e2766eadd5c)
* [Building a Linear Regression Model in R to Predict AFL Crowds](https://medium.com/@jaseziv83/building-an-linear-regression-model-in-r-to-predict-afl-crowds-735b16a1f7c6)

## The Data
Three sources of data were used in the project:

1. [AFL Tables](https://afltables.com) was used as the primary source of information. This is no doubt the go to for anyone looking at doing AFL analysis
2. [Australian Sports Betting](http://www.aussportsbetting.com) was used to get betting data for matches from the 2013 season
3. [Australian Bureau of Meteorology](http://www.bom.gov.au/climate/data/) was used for climat data (rain and temperature). Part of the scraper for BOM data was taken from James Day's [fitzRoy](https://github.com/jimmyday12/fitzRoy) R package. Big thanks to him.


## Packages Used

* rvest
* httr
* dplyr
* stringr
* tidyr
* lubridate
* tidyverse
* car
* MASS
* readxl


# To Reproduce The Analysis

Functions have been created to colect both sources of data:

* [afl_scraper.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/R/afl_scraper.R) goes and scrapes eihter a single season, or a range of seasons.
* [return_betting_data.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/R/return_betting_data.R) downloads the betting data xlsx file and does some preprocessing steps
* [get_bom_data.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/R/get_bom_data.R) extracts rain and temperature data and fills in missing values. Missing rain figures are averaged out with the next available reading, while missing temperatures are given that months averag min and max temp.


Then to scrape and save the files to csv, run [src/get_afl_data.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/src/get_afl_data.R).

If existing data in this repo is enough, simply run [src/preprocessing.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/src/preprocessing.R) to go thorough the preprocessing steps to get the data ready for the analysis in the Medium posts.
