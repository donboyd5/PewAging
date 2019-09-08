# Clear the workspace and
rm(list = ls())

################################################################################
#     Program Name: R Sample Code for Computing a Calendar Year Estimate       #
#                   For a State Using the State Weights (California)           #
#                                                                              #
#     Written by: Arcenis Rojas - 24 April 2018                                #
#     Version: R 3.4.4                                                         #
#                                                                              #
#     This program is intended to outline the procedures for computing PUMD    #
#     weighted estimates. Any errors are mine alone.                           #
################################################################################

# Load required packages and install them if necessary
pkgs <- c("readxl", "readr", "magrittr", "stringr", "tidyr", "dplyr")
lapply(pkgs, function(p) if (!p %in% installed.packages()) install.packages(p))
lapply(pkgs, function(p) library(p, character.only = TRUE))

# Store the path to the directory containing the five 2016 quarterly FMLI files
fmli_dir <- "c:/ajr-projects/pumd/2016"

# Store the path to the file containing the CA state weights
ca_wt_file <- "c:/ajr-projects/state-weights/xlsx-ca/Interview/cawgt16.xlsx"

# Read in the FMLI files in an "lapply"
fmli <- lapply(

  # List the CSV file in the directory containing the fmli files that start
  # with "fmli"
  list.files(fmli_dir, pattern = "fmli.*[.]csv", full.names = TRUE),

  # Declare a function to read each file
  function(x) {

    # Read in the file
    read_csv(x) %>%

      # Change all column names to lower case
      set_names(names(.) %>% tolower) %>%

      # Keep only the necessary variables
      select(
        newid, state, psu, qintrvyr, qintrvmo, totexpcq, totexppq, foodcq,
        foodpq, transcq, transpq, houscq, houspq, healthcq, healthpq, entertcq,
        entertpq
      ) %>%

      # Ensure that variables are the correct data type or character length
      mutate(
        newid = str_pad(newid, width = 8, side = "left", pad = 0),
        qintrvyr = as.integer(qintrvyr),
        qintrvmo = as.integer(qintrvmo),
        state = str_pad(state, width = 2, side = "left", pad = 0)
      )
  }
) %>%

  # Concatenate, a.k.a. append, the dataframes in the above list
  bind_rows %>%

  # Store variables that keep the calendar year expenditure values for each
  # CU depending on the time of the interview
  mutate(
    "total expenditures" = ifelse(
      qintrvyr %in% "2017", totexppq,
      ifelse(qintrvmo %in% 1:3, totexpcq, (totexpcq + totexppq))
    ),
    food = ifelse(
      qintrvyr %in% "2017", foodpq,
      ifelse(qintrvmo %in% 1:3, foodcq, (foodcq + foodpq))
    ),
    transportation = ifelse(
      qintrvyr %in% "2017", transpq,
      ifelse(qintrvmo %in% 1:3, transcq, (transcq + transpq))
    ),
    housing = ifelse(
      qintrvyr %in% "2017", houspq,
      ifelse(qintrvmo %in% 1:3, houscq, (houscq + houspq))
    ),
    healthcare = ifelse(
      qintrvyr %in% "2017", healthpq,
      ifelse(qintrvmo %in% 1:3, healthcq, (healthcq + healthpq))
    ),
    entertainment = ifelse(
      qintrvyr %in% "2017", entertpq,
      ifelse(qintrvmo %in% 1:3, entertcq, (entertcq + entertpq))
    )
  ) %>%

  # Drop the "cq" and "pq" variables
  select(-ends_with("cq"), -ends_with("pq"))

# Read in the file with the CA weights
ca_wgt <- read_excel(ca_wt_file) %>%

  # Change all column names to lower case
  set_names(names(.) %>% tolower) %>%

  # Ensure that variables are the correct data type or character length and
  # create variables with the number of months in scope and a population weight
  # based on the months in scope for a given CU
  mutate(
    newid = str_pad(newid, width = 8, side = "left", pad = 0),
    qintrvyr = as.integer(qintrvyr),
    qintrvmo = as.integer(qintrvmo),
    state = str_pad(state, width = 2, side = "left", pad = 0),
    mo_scope = ifelse(
      qintrvyr %in% "2017", 4 - qintrvmo,
      ifelse(qintrvmo %in% 1:3, qintrvmo - 1, 3)
    ),
    population = (cawgt / 4) * (mo_scope / 3)
  )

# Generate a table of calendar year means for each expenditure category and keep
# the sum of the population weight
ca_means <- ca_wgt %>%

  # Merge the fmli data to the CA weight data (this will drop all CU's in the
  # fmli dataframe with no corresponding record in the CA weight dataframe)
  left_join(fmli, by = c("newid", "state", "psu", "qintrvyr", "qintrvmo")) %>%

  # Multiply each expenditure variable by the CA weight to get a weighted
  # expenditure for each CU
  mutate_at(
    vars(
      one_of("total expenditures"), food, transportation, housing, healthcare,
      entertainment
    ),
    funs(. * cawgt)
  ) %>%

  # Take the sum of each expenditure variable and the CA population weight
  summarise_at(
    vars(
      one_of("total expenditures"), food, transportation, housing, healthcare,
      entertainment, population
    ),
    funs(sum)
  ) %>%

  # Divide the sum of each expenditure by the CA population to get mean annual
  # expenditures per CU
  mutate_at(
    vars(
      one_of("total expenditures"), food, transportation, housing, healthcare,
      entertainment
    ),
    funs((. / population))
  ) %>%

  # Format the numeric values for better printing
  mutate_at(
    vars(
      one_of("total expenditures"), food, transportation, housing, healthcare,
      entertainment, population
    ),
    funs(. %>% round(2) %>% prettyNum(big.mark = ",", small.mark = "."))
  ) %>%

  # Convert the dataframe from wide to long format
  gather(Expenditure, 'Estimated CA Calendar Year Mean') %>%

  # Change the "Expenditure" variable format to title case for better printing
  mutate(Expenditure = str_to_title(Expenditure))
