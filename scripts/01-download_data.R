#### Preamble ####
# Purpose: Downloads and saves the data from ACS API
# Author: Yiliu Cao
# Date: 1 October 2023
# Contact: yiliu.cao@mail.utoronto.ca
# License: MIT
# Pre-requisites: None


#### Workspace setup ####


#### Download data ####
url02 <- "https://api.census.gov/data/2020/acs/acs5/profile?get=group(DP02)&for=county:*"
DP02 <- read.csv(url(url02), stringsAsFactors = FALSE)
url03 <- "https://api.census.gov/data/2020/acs/acs5/profile?get=group(DP03)&for=county:*"
DP03 <- read.csv(url(url03), stringsAsFactors = FALSE)
url05 <- "https://api.census.gov/data/2020/acs/acs5/profile?get=group(DP05)&for=county:*"
DP05 <- read.csv(url(url05), stringsAsFactors = FALSE)

url_covid <- paste0(
  "https://raw.githubusercontent.com/CSSEGISandData/",
  "COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
  "01-01-2021.csv")
covid_data <- read.csv(url_covid, header = TRUE, sep = ",")

#### Save data ####
write.csv(DP02, "inputs/data/DP_02.csv", row.names = FALSE)
write.csv(DP03, "inputs/data/DP_03.csv", row.names = FALSE)
write.csv(DP05, "inputs/data/DP_05.csv", row.names = FALSE)
write.csv(covid_data, "inputs/data/covid_data.csv", row.names = FALSE)
         
