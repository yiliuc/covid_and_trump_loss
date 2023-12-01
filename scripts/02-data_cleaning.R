#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(dplyr)
library(stringr)
library(tidyverse)
library(janitor)
library(tools)

#### Clean data ####
# Import all the raw data
DP02 <- read.csv("inputs/data/DP_02.csv")
DP03 <- read.csv("inputs/data/DP_03.csv")
DP05 <- read.csv("inputs/data/DP_05.csv")
covid_data <- read.csv("inputs/data/covid_data.csv")
covid_data2 <- read.csv("inputs/data/covid_data2.csv")
election_data <- read.csv("inputs/data/countypres_2000-2020.csv")

get_state_abbreviation <- function(state_names) {
  sapply(state_names, function(state_name) {
    state_data <- state.abb[match(state_name, state.name)]
    if (!is.na(state_data)) {
      return(state_data)
    } else {
      return(NA)  # changed from NULL to NA for consistency in a vector
    }
  })
}

# Extract the county name from a 'County, State' formatted string.
extract_county <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(str_replace(str_split(name, ",")[[1]][1], " County", ""))
  } 
  else if (!is.na(name)) {
    return(str_replace(str_split(name, " ")[[1]][1], " County", ""))
  }
  else {
    return(NA)
  }
}

# Extract the state abbreviation from a 'County, State' formatted string.
extract_state <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(get_state_abbreviation(str_trim(str_split(name, ",")[[1]][2])))
  } else {
    return(NA)
  }
}

extract_fips <- function(code) {
  extracted <- sub(".*US", "", code)
  extracted <- as.numeric(extracted)
  return(extracted)
}

# Clean and rename columns in a DataFrame.
data_cleaning <- function(df, rename_dict) {
  df$county <- sapply(df$NAME, extract_county)
  df$state <- sapply(df$NAME, extract_state)
  df$fips <- extract_fips(df$GEO_ID)
  columns_to_select <- c("county", "state", "fips", names(rename_dict))
  selected_df <- df[, columns_to_select, drop = FALSE]
  colnames(selected_df) <- c("county", "state", "fips", rename_dict)
  selected_df <- na.omit(selected_df)
  # selected_df$state <- unlist(selected_df$state)
  return(selected_df %>% arrange(fips))
}

dict_02 <- c("DP02_0068PE" = "prop_higher_education")
dict_03 <- c("DP03_0063E" = "mean_household_income",
             "DP03_0097PE" = "private_insurance",
             "DP03_0099PE" = "no_insurance")
dict_05 <- c("DP05_0001PE" = "total_population",
             "DP05_0002PE" = "males",
             "DP05_0017PE" = "age_85",
             "DP05_0037PE" = "white_pct",
             "DP05_0038PE" = "black_pct")

# Clean all the ACS raw data
DP02_clean <- data_cleaning(DP02, dict_02)
DP03_clean <- data_cleaning(DP03, dict_03)
DP05_clean <- data_cleaning(DP05, dict_05)

# Clean the covid data
covid_data_clean <- covid_data %>% 
  filter(Country_Region == "US") %>% 
  dplyr::select(FIPS, Admin2, Province_State, Confirmed, Deaths) %>% 
  rename(`county` = Admin2,
         `state` = Province_State,
         `cases` = Confirmed) %>%
  mutate(state = get_state_abbreviation(state)) %>% 
  clean_names() 
covid_data_clean <- na.omit(covid_data_clean)

covid_data2_clean <- covid_data2 %>% 
  filter(Country_Region == "US") %>% 
  dplyr::select(FIPS, Admin2, Province_State, Confirmed, Deaths) %>% 
  rename(`county` = Admin2,
         `state` = Province_State,
         `cases` = Confirmed) %>%
  mutate(state = get_state_abbreviation(state)) %>% 
  clean_names() 
covid_data2_clean <- na.omit(covid_data2_clean)

# Clean the election data
election_data_clean <- election_data %>% 
  filter(year == 2020) %>% 
  mutate(county = str_to_title(county_name),
         party = str_to_title(party),
         state = state_po,
         votes = candidatevotes,
         total_votes = totalvotes, 
         fips = county_fips,
         pct_vote = votes/total_votes) %>%
  dplyr::select(state_po, county, fips, candidate, party, votes, total_votes, pct_vote)

winning_party <- election_data_clean %>%
  group_by(fips) %>%
  summarize(winning_party = party[which.max(votes)])

election_data_clean <- election_data_clean %>%
  left_join(winning_party, by = "fips")

# Merge the data
merged_acs <- DP02_clean %>%
  inner_join(DP03_clean, by = c("state", "county", "fips")) %>%
  inner_join(DP05_clean, by = c("state", "county", "fips")) %>% 
  mutate(pctile = ntile(mean_household_income, 100),
         prop_higher_education = as.numeric(prop_higher_education))

merged_covid_election <- covid_data_clean %>% 
  inner_join(election_data_clean, by = "fips") %>% 
  mutate(county = county.x) %>% 
  dplyr::select(state, county, fips, cases, deaths, party, votes, total_votes, pct_vote, winning_party)

merged_data <- merged_acs %>% 
  inner_join(merged_covid_election, by = c("fips", "state")) %>% 
  mutate(infrate = cases/total_population * 100000,
         mortrate = deaths/total_population * 100000)

mean_infrate <- mean(merged_data$infrate)
mean_mortrate <- mean(merged_data$mortrate)

merged_data <- merged_data %>% 
  mutate(high_infrate = ifelse(infrate > mean(infrate), 1, 0),
         high_mortrate = ifelse(infrate > mean(mortrate), 1, 0),
         county = county.x) %>% 
  select(-county.x, county.y)

# Write CSV
write.csv(election_data_clean, "outputs/data/election_data_clean.csv", row.names = FALSE)
write.csv(covid_data_clean, "outputs/data/covid_data_clean.csv", row.names = FALSE)
write.csv(merged_covid_election, "outputs/data/covid_election.csv", row.names = FALSE)
write.csv(merged_acs, "outputs/data/acs_data_clean.csv", row.names = FALSE)
write.csv(merged_data, "outputs/data/merged_data.csv", row.names = FALSE)