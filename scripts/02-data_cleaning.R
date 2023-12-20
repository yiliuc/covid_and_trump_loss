#### Preamble ####
# Purpose: Data cleaning and export the cleaned data
# Author: Yiliu Cao
# Date: Dec 20 2023
# Contact: yiliu.cao@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(dplyr)
library(stringr)
library(tidyverse)
library(janitor)
library(tools)
library(tidyr)

#### Clean data ####
# Import all the raw data
DP02 <- read.csv("inputs/data/DP_02.csv")
DP03 <- read.csv("inputs/data/DP_03.csv")
DP05 <- read.csv("inputs/data/DP_05.csv")
covid_data <- read.csv("inputs/data/covid_data.csv")
election_data <- read.csv("inputs/data/countypres_2000-2020.csv")

get_state_abbreviation <- function(state_names) {
  sapply(state_names, function(state_name) {
    if (state_name == "District of Columbia") {
      return("D.C.")
    } else {
      state_data <- state.abb[match(state_name, state.name)]
      if (!is.na(state_data)) {
        return(state_data)
      } else {
        return(NA)  # Keep NA for consistency in vector
      }
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
  mutate(state = get_state_abbreviation(state),
         state = ifelse(FIPS == 11001, "DC", state)) %>% 
  clean_names() 

na_rows <- apply(covid_data_clean, 1, function(x) any(is.na(x)))
covid_data_clean[na_rows, ]

# Clean the election data
election_data_2020 <- election_data %>% 
  filter(year == 2020) %>% 
  mutate(county = str_to_title(county_name),
         party = str_to_title(party),
         state = state_po,
         votes = candidatevotes,
         total_votes = totalvotes, 
         fips = ifelse(state == "DC", 11001, county_fips)) %>% 
  dplyr::select(state, county, fips, candidate, party, votes, total_votes, mode)

total_data <- election_data_2020 %>% 
  filter(mode == "TOTAL") %>% 
  dplyr::select(-mode)
non_total_data <- election_data_2020 %>% 
  filter(mode != "TOTAL" & state != "UT") %>% 
  dplyr::select(-mode)

aggregated_data <- non_total_data %>%
  group_by(state, county, fips, candidate, party, total_votes) %>% 
  summarize(votes = sum(votes, na.rm = TRUE), .groups = 'drop')

election_data_2020 <- bind_rows(total_data, aggregated_data)

election_data_2020_clean <- election_data_2020 %>%
  group_by(state, county, fips, party, total_votes) %>%
  summarise(votes = sum(votes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(vote_demo = Democrat,
         vote_rep = Republican,
         vote_green = Green,
         vote_lib = Libertarian,
         vote_other = Other,
         across(c(vote_demo, vote_rep, vote_green, vote_lib, vote_other), 
                ~replace(., is.na(.), 0)),
         # winning_party = ifelse(vote_demo > vote_rep, "Democrat", "Republican"),
         pct_vote_demo = vote_demo/total_votes,
         pct_vote_rep = as.numeric(vote_rep/total_votes),
         rep_won20 = ifelse(pct_vote_rep > pct_vote_demo, 1, 0)) %>% 
  dplyr::select(-Democrat, -Green, -Libertarian, -Other, -Republican)

mean_values <- round(colMeans(election_data_2020_clean[, -c(1, 2, 3)], na.rm = TRUE), 1)
new_row_base <- data.frame(state = "UT", county = "Salt Lake", fips = 49035)
new_row <- cbind(new_row_base, t(data.frame(mean_values)))
election_data_2020_clean <- rbind(election_data_2020_clean, new_row) %>% 
  mutate(winning_party = ifelse(vote_demo > vote_rep, "Democrat", "Republican"),
         fips = as.numeric(fips))
# The only NA value for election_data_2020_clean is fips for RI, Federal Precint

election_data_2016 <- election_data %>% 
  filter(year == 2016) %>% 
  mutate(county = str_to_title(county_name),
         party = str_to_title(party),
         state = state_po,
         votes = candidatevotes,
         total_votes = totalvotes, 
         fips = county_fips) %>% 
  dplyr::select(state, county, fips, candidate, party, votes, total_votes)

election_data_2016_clean <- election_data_2016 %>%
  group_by(state, county, fips, party, total_votes) %>%
  summarise(votes = sum(votes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(vote_demo16 = Democrat,
         vote_rep16 = Republican,
         vote_other = Other,
         pct_vote_demo16 = Democrat/total_votes,
         pct_vote_rep16 = Republican/total_votes,
         across(c(pct_vote_demo16, pct_vote_rep16), 
                ~replace(., is.na(.), 0)),
         rep_won16 = ifelse(pct_vote_rep16 > pct_vote_demo16, 1, 0),
         demo_won16 = ifelse(pct_vote_rep16 < pct_vote_demo16, 1, 0)) %>% 
  dplyr::select(state, county, fips, pct_vote_demo16, pct_vote_rep16, vote_demo16,
                vote_rep16, rep_won16, demo_won16)

## elction_data_clean combines the election data for 2016 and 2020
election_data_clean <- election_data_2020_clean %>% 
  full_join(election_data_2016_clean, by = "fips") %>% 
  filter(! is.na(state.x),
         state.x == state.y) %>% 
  # The only row with NA for election_data_clean is fips for RI, Federal Precint
  mutate(state = state.x,
         county = county.x) %>% 
  dplyr::select(state, county, everything(), -county.x, -county.y, -state.x, -state.y)

# Merge the data
merged_acs <- DP02_clean %>%
  inner_join(DP03_clean, by = c("state", "county", "fips")) %>%
  inner_join(DP05_clean, by = c("state", "county", "fips")) %>% 
  mutate(pctile = ntile(mean_household_income, 100),
         prop_higher_education = as.numeric(prop_higher_education))

merged_covid_election <- election_data_clean %>% 
  left_join(covid_data_clean, by = "fips") %>% 
  filter(! is.na(fips)) %>% 
         # ! is.na(state.x) & is.na(state.y)) %>% 
  mutate(county = county.x,
         state = state.x,
         across(c(cases, deaths), 
                ~replace(., is.na(.), 0))) %>% 
  dplyr::select(state, county, everything(), -county.x, -county.y, -state.x, -state.y)

merged_data <- merged_acs %>% 
  full_join(merged_covid_election, by = "fips") %>% 
  na.omit() %>% 
  # The data for Hawaii is full but not for Alaska. There are "disctricts" in Election data 
  # but "counties" or "borough" in covid or acs data
  # filter(state.x != "AK" & state.x != "HI") %>% 
  mutate(infrate = cases/total_population * 100000,
         mortrate = deaths/total_population * 100000,
         dpc = deaths/cases*100000,
         across(dpc, 
                ~replace(., is.na(.), 0)),
         county = county.x,
         state = state.x,
         county = ifelse(state == "LA", gsub(" Parish", "", county), county),
         change_vote_rep = as.numeric(pct_vote_rep) - pct_vote_rep16) %>% 
  dplyr::select(state, county, everything(), -county.x, -county.y, -state.x, -state.y)

# Electoral Votes for each staet
states <- c("AL", "KY", "ND", "AK", "LA", "OH", "AZ", "ME", "OK", "AR", 
            "MD", "OR", "CA", "MA", "PA", "CO", "MI", "RI", "CT", "MN", 
            "SC", "DE", "MS", "SD", "DC", "MO", "TN", "FL", "MT", "TX", 
            "GA", "NE", "UT", "HI", "NV", "VT", "ID", "NH", "VA", "IL", 
            "NJ", "WA", "IN", "NM", "WV", "IA", "NY", "WI", "KS", "NC", "WY")

electoral_votes <- c(9, 8, 3, 3, 8, 17, 11, 4, 7, 6, 
                     10, 8, 54, 11, 19, 10, 15, 4, 7, 10, 
                     9, 3, 6, 3, 3, 10, 11, 30, 4, 40, 
                     16, 5, 6, 4, 6, 3, 4, 4, 13, 19, 
                     14, 12, 11, 5, 4, 6, 28, 10, 6, 16, 3)
electoral_votes_state <- data.frame(state = states, electoral_votes = electoral_votes)

# Write CSV
write.csv(election_data_clean, "outputs/data/election_data_clean.csv", row.names = FALSE)
write.csv(election_data_2016, "outputs/data/election_data_2016.csv", row.names = FALSE)
write.csv(covid_data_clean, "outputs/data/covid_data_clean.csv", row.names = FALSE)
write.csv(merged_covid_election, "outputs/data/covid_election.csv", row.names = FALSE)
write.csv(merged_acs, "outputs/data/acs_data_clean.csv", row.names = FALSE)
write.csv(merged_data, "outputs/data/merged_data.csv", row.names = FALSE)
write.csv(electoral_votes_state, "outputs/data/electoral_votes_state.csv", row.names = FALSE)

