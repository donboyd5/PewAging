


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")

# https://www.kff.org/medicaid/state-indicator/medicaid-spending-per-enrollee/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D#

library(rvest)


#****************************************************************************************************
#                Medicaid spending per enrollee, 2014 ####
#****************************************************************************************************
df <- read_excel("./data_raw/KFFMedicaidPerEnrollee2014.xlsx", 
                 range="A3:F55", 
                 col_types = c("text", rep("numeric", 5)))

df %>% select(stname, aged) %>%
  filter(!is.na(aged)) %>%
  arrange(-aged) %>%
  ht(10)


