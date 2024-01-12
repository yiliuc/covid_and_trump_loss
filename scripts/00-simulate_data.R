#### Preamble ####
# Purpose: Simulate the data
# Author: Yiliu Cao
# Date: Dec 20 2023
# Contact: yiliu.cao@mail.utoronto.ca
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
# This is a simple simulation of the analysis data
# I used number to represent the counties

data <- tibble(county = sample(c(1:3000)),
               vote_demo = runif(3000, min = 0, max = 1),
               vote_rep = 1 - vote_demo,
               dpc = runif(3000, min = 0, max = 1),
               insurance = runif(3000, min = 0, max = 1),
               higher_education = runif(3000, min = 0, max = 0.5),
               winning_party = ifelse(vote_demo > vote_rep, "Democrat",
                                      "Republican"))



