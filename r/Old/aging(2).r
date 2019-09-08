

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

#..csv files ----
# note that age 85 is really 85+

# nativity
# 1 = Native born
# 2 = Foreign born

# race_hisp
rhf <- read_csv("lev, lab
0, all_rhisp
1, white_alone
2, black_alone
3, AIAN_alone
4, asian_alone
5, NHPI_alone
6, two_plus_races
7, not_hisp
8, hispanic
9, non-hisp_White_alone")

# Black = Black or African American
# AIAN = American Indian and Alaska Native
# NHPI = Native Hawaiian and Other Pacific Islander

# sex
# 0 = Both sexes
# 1 = Male
# 2 = Female

ppdir <- "D:/Data/CensusPopulationProjections/"
ppfn <- "np2017_d5.csv"
df <- read_csv(paste0(ppdir, ppfn))
glimpse(df)

# get the basic data
ppdf1 <- df %>%
  setNames(str_to_lower(names(.))) %>%
  gather(variable, value, contains("pop")) %>%
  mutate(variable=ifelse(variable=="total_pop", "pop_-1", variable),
         age=str_remove(variable, "pop_") %>% as.integer(),
         value=as.numeric(value)) %>%
  select(year, sex, race_hisp, nativity, age, value)
glimpse(ppdf1)

# create an all nativities group as, oddly, it is not in the data
allnative <- ppdf1 %>%
  group_by(year, sex, race_hisp, age) %>%
  summarise(value=sum(value)) %>%
  mutate(nativity=0)
glimpse(allnative)

ppdf2 <- bind_rows(ppdf1, allnative)
glimpse(ppdf2)
ht(ppdf2)

ppdf3 <- ppdf2 %>%
  mutate(natf=factor(nativity, levels=0:2, labels=c("all_nativities", "native", "foreign")),
         rhispf=factor(race_hisp, levels=rhf$lev, labels=rhf$lab),
         sexf=factor(sex, levels=0:2, labels=c("all_sexes", "male", "female"))) %>%
  mutate_at(vars(sex, nativity, race_hisp), as.factor) %>%
  select(year, age, sex, nativity, race_hisp, sexf, natf, rhispf, pop=value) %>%
  arrange(year, age, sex, nativity, race_hisp)
glimpse(ppdf3)

ppdf <- ppdf3
ht(ppdf)

# do some checks on the data
unique(ppdf$age)
count(ppdf, nativity, natf)
count(ppdf, race_hisp, rhispf)
count(ppdf, sex, sexf)

comment(ppdf) <- "Census NP2017_D5: Projected Population by Single Year of Age, Sex, Race, Hispanic Originand Nativity for the United States: 2016 to 2060"
comment(ppdf)
saveRDS(ppdf, "./data/uspopproj.rds")


#****************************************************************************************************
#                Examine Census population projections ####
#****************************************************************************************************
# Census Bureau dependency measures
# Age dependency ratio: divide combined under 18 years and 65 years and over by the 18-64 years population and multiplying by 100.
# (American Community Survey and Population Estimates Program)




ppdf <- readRDS("./data/uspopproj.rds")
glimpse(ppdf)
ht(ppdf)

ppdf %>%
  filter(age==-1, sex==0, nativity==0, race_hisp==0) %>%
  ggplot(aes(year, pop)) + geom_line() + geom_point()

ppdf %>%
  filter(age==-1, sex %in% 1:2, nativity==0, race_hisp==0) %>%
  ggplot(aes(year, pop, colour=sexf)) + geom_line() + geom_point()

count(ppdf, race_hisp, rhispf)
ppdf %>%
  filter(age==-1, sex==0, nativity==0, race_hisp!=0) %>%
  ggplot(aes(year, pop, colour=rhispf)) + geom_line() + geom_point()

ppdf %>%
  filter(age==-1, sex==0, nativity==0, race_hisp %in% 7:8) %>%
  group_by(year) %>%
  mutate(year_tot=sum(pop)) %>%
  ggplot(aes(year, pop / year_tot, colour=rhispf)) + geom_line() + geom_point()

ppdf %>%
  filter(age==-1, sex %in% 1:2, nativity==0, race_hisp==0) %>%
  group_by(year) %>%
  mutate(year_tot=sum(pop)) %>%
  ggplot(aes(year, pop / year_tot, colour=sexf)) + geom_line() + geom_point()


acuts <- c(-Inf, 24, 64, 84, Inf)
acuts <- c(-Inf, 24, 64, Inf)
ppdf %>%
  filter(age>=0, sex==0, nativity==0, race_hisp==0) %>%
  mutate(agegrp=cut(age, acuts)) %>%
  group_by(year, agegrp) %>%
  summarise(pop=sum(pop)) %>%
  group_by(year) %>%
  mutate(year_tot=sum(pop)) %>%
  ggplot(aes(year, pop / year_tot * 100, colour=agegrp)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(2010, 2080, 5)) +
  scale_y_continuous(breaks=seq(0, 100, 2))


ppdf %>%
  filter(age>=0, sex==0, nativity==0, race_hisp==0) %>%
  mutate(dpop=case_when(
    age >= 65 ~ "old",
    age < 24 ~ "young",
    age >= 24 & age < 65 ~ "working",
    TRUE ~ "error")) %>%
  group_by(year, dpop) %>%
  summarise(pop=sum(pop)) %>%
  spread(dpop, pop) %>%
  mutate(old_depratio=old / working) %>%
  ggplot(aes(year, old_depratio)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(2010, 2080, 5)) +
  scale_y_continuous(breaks=seq(0, 10, .02))



#****************************************************************************************************
#                EXPERIMENTAL OR OLD BELOW HERE ####
#****************************************************************************************************
# Census pop projections older approach ----
# api currently (10/3/2018) is available for older projections and for current estimates, but not
# for current projections
# Estimates
# api.census.gov/data/2017/pep/population
# api.census.gov/data/2017/pep/population.html 

# National tables 1-13
# https://www2.census.gov/programs-surveys/popproj/tables/2017/2017-summary-tables/
# all downloaded to ppdir
# np2017-t1.xlsx

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



