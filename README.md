# COVID-19 and the 2020 U.S. Presidential Election: Counties with Extra COVID-19 Deaths Showed Less Support For Trump

## Overview

This paper uses the 2020 U.S. Election data from MIT Election Data Science Club, COVID-19 data from Center for Systems Science and Engineering at Johns Hopkins University and U.S. socio-economic data from American Community Survey (ACS), to find the relationship between extra COVID-19 deaths and voting for Trump in 2020. Specifically, we perform exploratory analysis to define the treatment and use the method of propensity score matching to find the treatment effects.

Except the election data was downloaded directly from their website, all the data are extracted using API and stored at `inputs/data`. The output paper is in `outputs/paper/paper.pdf`.

## File Structure

The repo is structured as:

-   `input/data` contains all the raw data in this paper.
-   `outputs/data` contains the cleaned datasets
-   `outputs/paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.