

source("./r/includes/libraries.r")
source("./r/includes/globals.r")
source("./r/includes/functions.r")


# CPS - create cps_person - person data frame ####


# March supplement as obtained from Census
# General info https://www.census.gov/programs-surveys/saipe/guidance/model-input-data/cpsasec.html
# ftp download

# State tax variables discussed at http://answers.popdata.org/Negative-values-fedtax-statetax-q1469058.aspx

cpsdir <- "D:/Data/CensusCPS/splitfiles/"

stabfips <- function(fips){
  stabbr <- stcodes$stabbr[match(fips, as.character(stcodes$stfips))] %>%
    as.character
  return(stabbr)
}

# households - good for 2015, 2016, 2017, 2018 ----
hvars <- read_csv("vname, start, end, type
hseq, 2, 6, i
gestfips, 42, 43, c")
hvars

gethh <- function(year, hvars) {
  hhfn <- paste0(cpsdir, "asec", year, "_hh.dat")
  hhdf <- read_fwf(hhfn, fwf_positions(hvars$start, hvars$end, col_names=hvars$vname),
                   col_types = paste0(hvars$type, collapse = ""),
                   n_max=-1)
  return(hhdf)
}


hh2017 <- gethh(2017, hvars) %>%
  mutate(year=2017,
         stabbr=stabfips(gestfips))
glimpse(hh2017)
count(hh2017, gestfips, stabbr)

hh2018 <- gethh(2018, hvars) %>%
  mutate(year=2018,
         stabbr=stabfips(gestfips))
glimpse(hh2018)
count(hh2018, gestfips, stabbr)


# persons  - also good for 2015, 2016, 2017 ----

# ptotval Total persons income
# D FILESTAT 1 733 (1:6)
# Tax Filer status
# V 1 .Joint, both <65
# V 2 .Joint, one <65 & one 65+
# V 3 .Joint, both 65+
# V 4 .Head of household
# V 5 .Single
# V 6 .Nonfiler

# D STATETAX_AC 6 790 (-9999:999999)
# State income tax liability, after all
# credits
# V 0 .None
# V -9999 - .Dollar amount
# V 999999 .

pvars <- read_csv("vname, start, end, type
hseq, 2, 6, i
age, 19, 20, i
sex, 24, 24, i
marsupwt, 155, 162, d
ptotval, 580, 587, d
filestat, 733, 733, i
statetax_ac, 790, 795, d
")
pvars

getp <- function(year, pvars){
  perfn <- paste0(cpsdir, "asec", year, "_person.dat")
  perdf <- read_fwf(perfn, fwf_positions(pvars$start, pvars$end, col_names=pvars$vname),
                    col_types = paste0(pvars$type, collapse = ""),
                    n_max=-1)
  return(perdf)
}

p2017 <- getp(2017, pvars) %>%
  mutate(year=2017,
         marsupwt=marsupwt / 100,
         stabbr=hh2017$stabbr[match(hseq, hh2017$hseq)])
glimpse(p2017)

p2018 <- getp(2018, pvars) %>%
  mutate(year=2018,
         marsupwt=marsupwt / 100,
         stabbr=hh2018$stabbr[match(hseq, hh2018$hseq)])
glimpse(p2018)


# create a combined file
# put real tax in 2017 dollars -- that is, the 2018 file has 2017 $ (tax in the prior year)
# so adjust the 2017-file amount forward from 2016$ to 2017$
# the CPI-U was up 2.1% from 2016 to 2017

cps_person <- bind_rows(p2017, p2018) %>%
  mutate(rincome=ifelse(year==2017, ptotval * 1.021, ptotval),
         rtax=ifelse(year==2017, statetax_ac * 1.021, statetax_ac))

glimpse(cps_person)

saveRDS(cps_person, "./data/cps_person.rds")


# old items below here ----

# hh2015 <- gethh(2015, hvars) %>%
#   mutate(year=2015,
#          stabbr=stabfips(gestfips))
# glimpse(hh2015)
# count(hh2015, gestfips, stabbr)
# 
# hh2016 <- gethh(2016, hvars) %>%
#   mutate(year=2016,
#          stabbr=stabfips(gestfips))
# glimpse(hh2016)
# count(hh2016, gestfips, stabbr)

# p2015 <- getp(2015, pvars) %>%
#   mutate(year=2015,
#          marsupwt=marsupwt / 100,
#          stabbr=hh2015$stabbr[match(hseq, hh2015$hseq)])
# glimpse(p2015)
# 
# p2016 <- getp(2016, pvars) %>%
#   mutate(year=2016,
#          marsupwt=marsupwt / 100,
#          stabbr=hh2016$stabbr[match(hseq, hh2016$hseq)])
# glimpse(p2016)

