

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

library("alfred")


#****************************************************************************************************
#                Census population estimates ####
#****************************************************************************************************

# https://www.census.gov/data/tables/time-series/demo/fertility/his-cps.html
# https://www2.census.gov/programs-surveys/demo/tables/fertility/time-series/his-cps/



#****************************************************************************************************
#                Population projections ####
#****************************************************************************************************
# national
# https://www.census.gov/newsroom/press-releases/2018/cb18-41-population-projections.html
# https://www.census.gov/programs-surveys/popproj/data/tables.html




# comprehensive state


# individual state

#****************************************************************************************************
#                Census API info ####
#****************************************************************************************************
# api.census.gov/data.html
# https://www.census.gov/data/developers/data-sets.html




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


#****************************************************************************************************
#                Census population projections ####
#****************************************************************************************************
# Note that 2017-vintage projections were revised after Aug 1, 2018 and before Oct 2, 2018
# Best source:
# NP2017_D5: Projected Population by Single Year of Age, Sex, Race, Hispanic Originand Nativity for the United States: 2016 to 2060
# from
# https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
# https://www2.census.gov/programs-surveys/popproj/datasets/2017/2017-popproj/np2017_d5.csv

# api currently (10/3/2018) is available for older projections and for current estimates, but not
# for current projections
# Estimates
# api.census.gov/data/2017/pep/population
# api.census.gov/data/2017/pep/population.html 

# National tables 1-13
# https://www2.census.gov/programs-surveys/popproj/tables/2017/2017-summary-tables/
# all downloaded to ppdir
# np2017-t1.xlsx



ppdir <- "D:/Data/CensusPopulationProjections/"

#..csv files ----
# note that age 85 is really 85+
# The key for NATIVITY is as follows:
# 1 = Native born
# 2 = Foreign born

# race_hisp
# 0 = Total population
# 1 = White alone
# 2 = Black alone
# 3 = AIAN alone
# 4 = Asian alone
# 5 = NHPI alone
# 6 = Two or More Races
# 7 = Not Hispanic
# 8 = Hispanic
# 9 = Non-Hispanic White alone
rhf <- read_csv("lev, lab
0, Total population
1, White alone
2, Black alone
3, AIAN alone
4, Asian alone
5, NHPI alone
6, Two or More Races
7, Not Hispanic
8, Hispanic
9, Non-Hispanic White alone")


ppfn <- "np2017_d5.csv"
df <- read_csv(paste0(ppdir, ppfn))
glimpse(df)

ppdf <- df %>%
  setNames(str_to_lower(names(.))) %>%
  gather(variable, value, contains("pop")) %>%
  mutate(variable=ifelse(variable=="total_pop", "pop_-1", variable),
         age=str_remove(variable, "pop_") %>% as.integer(),
         value=as.numeric(value)) %>%
  select(year, age, sex, nativity, race_hisp) %>%
  mutate(natf=factor(nativity, levels=1:2, labels=c("native", "foreign")),
         rhispf=factor(race_hisp, levels=rhf$lev, labels=rhf$lab))
glimpse(ppdf)


# do some checks on the data
unique(ppdf$age)
count(ppdf, nativity, natf)
count(ppdf, race_hisp, rhispf)


# Census pop projections older approach ----
#..individual xlsx tables ----
fnames <- paste0("np2017-t", 1:13, ".xlsx")
fnames

dfnames <- paste0(ppdir, fnames)

#.... table 1 ----
t1 <- read_excel(dfnames[1], range = "A8:H52",
                 col_names = c("year", "pop", "change", "pchya", "births", "deaths", "natinc", "netintl"),
                 col_types=c("numeric"))
ht(t1)

# checks:
t1 %>% mutate(check=pop - lag(pop) - change) %>% select(year, pop, change, check)
t1 %>% mutate(check=births - deaths - natinc) %>% select(year, births, deaths, natinc, check)
t1 %>% mutate(check=change - (natinc +  netintl)) %>% select(year, change, natinc, netintl, check)

#.... table 8 ----
t8 <- read_excel(dfnames[8], range = "A8:j52",
                 col_names = c("year", 
                               "pop", "change", "pchya",
                               "pop_native", "change_native", "pchya_native",
                               "pop_foreign", "change_foreign", "pchya_foreign"),
                 col_types=c("numeric"))
ht(t8)








#****************************************************************************************************
#                FRED via alfred ####
#****************************************************************************************************
df <- get_fred_series("INDPRO", "indpro")
ht(df)

df2 <- get_alfred_series("INDPRO", "indpro")
df2



