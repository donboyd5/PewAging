

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


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
totpop <- "https://demographics.coopercenter.org/sites/demographics/files/NationalProjections_ProjectedTotalPopulation_2020-2040_Updated06-2016.xls"
agepop <- "https://demographics.coopercenter.org/sites/demographics/files/NationalProjections_ProjectedAgeDistribution_2020-2040_Updated06-2016.xls"

wcdir <- "D:/Data/PopData_misc/WeldonCooper/"
wctot <- "NationalProjections_ProjectedTotalPopulation_2020-2040_Updated06-2016.xls"
wcage <- "NationalProjections_ProjectedAgeDistribution_2020-2040_Updated06-2016.xls"


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
# stfips <- df$stfips
getstabbr_from_fips <- function(stfips){
  # works if stfips is text or numeric but not factor
  if(is.factor(stfips)) print("ERROR - stfips is a factor!!")
  stfips <- stfips %>% as.integer %>% str_pad(width=2, side="left", pad="0")
  stabbr <- stcodes$stabbr[match(stfips, stcodes$stfips)]
  return(stabbr)
}

getregion_states <- function(region) {stcodes$stabbr[stcodes$beargn==region]}

getregion <- function(stabbr) {stcodes$beargn[match(stabbr, stcodes$stabbr)]}
getregion_name <- function(stabbr) {stcodes$beargn.name[match(stabbr, stcodes$stabbr)]}

# getregion_name(state.abb)


#****************************************************************************************************
#                ONE TIME: Download population projections data ####
#****************************************************************************************************
# download.file(totpop, paste0(wcdir, wctot), mode="wb")
# download.file(agepop, paste0(wcdir, wcage), mode="wb")


#****************************************************************************************************
#                ONE TIME: Read and clean population projections data ####
#****************************************************************************************************

year <- 2010

idvars <- c("stfips", "stname", "sex", "poptot")
starts <- seq(0, 80, 5) %>% str_pad(width=2, side="left", pad="0")
ends <- seq(4, 84, 5) %>% str_pad(width=2, side="left", pad="0")
popranges <- paste(starts, ends, sep="_")
vnames <- c(idvars, paste0("pop", popranges), "pop85plus")
vnames

ctypes <- c("text", "text", "text", rep("numeric", length(vnames) - 3))

# ONE-TIME DATA SETUP
getproj <- function(year){
  print(year)
  df <- read_excel(paste0(wcdir, wcage), sheet=as.character(year), range="A5:V160", col_names=vnames, col_types=ctypes)
  # glimpse(df)
  
  df2 <- df %>%
    mutate(checksum=rowSums(.[5:ncol(.)]) - poptot,
           year=as.integer(year),
           stabbr=getstabbr_from_fips(stfips))
  # glimpse(df2)
  if(abs(sum(df2$checksum, na.rm=TRUE)) > .001) print("Potential summation problem")
  # count(df2, stabbr, stname)
  # count(df2, sex)
  df3 <- df2 %>% select(-stfips, -stname, -checksum) %>%
    gather(popgroup, value, -stabbr, -year, -sex)
  return(df3)
}

wc_popproj <- ldply(c(2010, 2020, 2030, 2040), getproj, .progress="text")
glimpse(wc_popproj)
count(wc_popproj, stabbr)
count(wc_popproj, sex)
count(wc_popproj, year)
count(wc_popproj, popgroup)
saveRDS(wc_popproj, "./data/popproj/wc_popproj.rds")
# END ONE_TIME


#****************************************************************************************************
#                Examine data ####
#****************************************************************************************************
popproj <- readRDS("./data/popproj/wc_popproj.rds")
glimpse(popproj)
count(popproj, popgroup)
unique(popproj$popgroup)
