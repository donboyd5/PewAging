
 
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

# library("alfred")


#****************************************************************************************************
#                Census population estimates ####
#****************************************************************************************************

# https://www.census.gov/data/tables/time-series/demo/fertility/his-cps.html
# https://www2.census.gov/programs-surveys/demo/tables/fertility/time-series/his-cps/

# https://usa.ipums.org/usa/sampdesc.shtml
# https://opendata.stackexchange.com/questions/5442/historical-us-population-by-age-sex


#****************************************************************************************************
#                CDC Wonder Census population estimates ####
#****************************************************************************************************
# https://wonder.cdc.gov/wonder/help/bridged-race.html#About%201990-2017
# https://wonder.cdc.gov/Bridged-Race-v2017.HTML

cdcpd <- "D:/Data/PopData_misc/CDCWonder/"
fn1 <- "CDC Wonder age state year 1990-1999_nototals.txt"
fn2 <- "CDC Wonder age state year 2000-2009_nototals.txt"
fn3 <- "CDC Wonder age state year 2010-2017_nototals.txt"


df1 <- read_tsv(paste0(cdcpd, fn1))

glimpse(df1)
ht(df1)


#****************************************************************************************************
#                World Bank ####
#****************************************************************************************************

# https://datacatalog.worldbank.org/dataset/population-estimates-and-projections

#****************************************************************************************************
#                Cancer NCI population data ####
#****************************************************************************************************
# https://seer.cancer.gov/popdata/download.html
# population by county, single age, rate, origin, sex, 1990-2016
ncipd <- "D:/Data/PopData_misc/NCI/"
nciafn <- "us.1990_2016.singleages.adjusted.txt"

# year stabbr stfips
widths <- c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8)
cnames <- c("year", "stabbr", "stfips", "cntyfips", "registry",
            "race", "origin", "sex", "age", "pop")
ctypes <- "iciiiiiiid"


df <- read_fwf(paste0(ncipd, nciafn), fwf_widths(widths, cnames), col_types=ctypes)
glimpse(df)
ht(df)

df1 <- as.data.frame(df)
df1 <- as_tibble(df)
glimpse(df1)
count(df1, stabbr)
unique(df1$stabbr)
str(df)


#****************************************************************************************************
#                Download basic age data ####
#****************************************************************************************************
udir <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2017/state/asrh/"
ufn <- "sc-est2017-agesex-civ.csv"
download.file(paste0(udir, ufn), paste0("./data_raw/Census/", ufn), mode="wb")


#****************************************************************************************************
#                Parse and save basic age data ####
#****************************************************************************************************
df <- read_csv(paste0("./data_raw/Census/", ufn))
ht(df)
names(df)

df2 <- df %>%
  mutate(stabbr=stcodes$stabbr[match(STATE, as.integer(stcodes$stfips))]) %>%
  select(-SUMLEV, -REGION, -DIVISION, -STATE) %>%
  setNames(str_to_lower(names(.))) %>%
  gather(variable, value, -stabbr, -name, -sex, -age) %>%
  mutate(variable=str_remove(variable, "_civ"),
         year=str_sub(variable, -4, -1),
         vtype=str_remove(variable, year),
         year=as.integer(year)) %>%
  select(stabbr, name, sex, age, vtype, year, value)
ht(df2)
count(df2, stabbr, name)


