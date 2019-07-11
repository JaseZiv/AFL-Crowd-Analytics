# AFL-Crowd-Analytics

This project was created to build a machine learning model to predict Australian Football League (AFL) attendances using historical data.

Two articles have been posted to Medium:

* Article 1
* Article 2

Data used was scraped from [afltables.com](https://afltables.com/) and bettng data files were downloaded from the [Austrailan Sports Betting](http://www.aussportsbetting.com/) website.

# To Reproduce The Analysis

Functions have been created to colect both sources of data:

* [afl_scraper.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/R/afl_scraper.R) goes and scrapes eihter a single season, or a range of seasons.
* [return_betting_data.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/R/return_betting_data.R) downloads the betting data xlsx file and does some preprocessing steps


Then to scrape and save the files to csv, run [src/get_afl_data.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/src/get_afl_data.R).

If existing data in this repo is enough, simply run [src/preprocessing.R](https://github.com/JaseZiv/AFL-Crowd-Analytics/blob/master/src/preprocessing.R) to go thorough the preprocessing steps to get the data ready for the analysis in the Medium posts.
