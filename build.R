# ./build.r
# This is the master file for ETL of data needed for marketing/sales reports
# Blake Abbenante
# 10/1/19

if (!require(tidyverse)) {
  install.packages('tidyverse') # load the base tidyverse libraries (dplyr, tidyr, etc.)
  require(tidyverse)
}
if (!require(janitor)) {
  install.packages('janitor') # functions for augmenting dataframes
  require(janitor)
}
if (!require(readr)) {
  install.packages('readr') # enhanced functions for loading data
  require(readr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # enhanced functions for loading data
  require(lubridate)
}
if (!require(here)) {
  install.packages('here') # file referencing
  require(here)
}
if (!require(httr)) {
  install.packages('httr') # http posts
  require(httr)
}
if (!require(config)) {
  install.packages('config') # read a config file
  require(config)
}
if (!require(RiHana)) {
  install.packages('RiHana') # Hana stuff
  require(RiHana)
}


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

release_notes<-''


hana_dates<-RiHana::get_relevant_date()

config<-config::get(file="~/OneDrive - CBRE, Inc/data/config/r_config.yaml")


###Build the reports ####

#check if our cached data is less than a day old, otherwise run the ETL script
file_date<-file.info('inc/funnel_data.RData')$mtime

 if(difftime(now(),file_date,units="hours")>24)
 {
  source("funnel_and_trend_etl.R")
}

trend_pub<-rsconnect::deployDoc("coworking_marketing_funnel.Rmd",forceUpdate=TRUE,launch.browser=FALSE)

if (trend_pub){
  POST(url=config$insights_webhook,body=get_slack_payload("Funnel and Trend Dashboard","https://blake-abbenante-hana.shinyapps.io/hana_funnel_and_trend/",release_notes),encode="json")
}

