

#****************************************************************************************************
#                Setup ####
#****************************************************************************************************

source("./r/libraries.r")


#****************************************************************************************************
#                California ####
#****************************************************************************************************
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/P3_Complete.csv.zip
# http://www.dof.ca.gov/Forecasting/Demographics/Projections/P3_Dictionary.txt

# historical: http://www.dof.ca.gov/Forecasting/Demographics/Estimates/


csdir <- "./data_raw/Case Study States/"
cadir <- paste0(csdir, "CA/")

df <- read_csv(paste0(cadir, "P3_Complete.csv"), col_types="iiiiid")
ht(df)
glimpse(df)
count(df, perwt)
count(df, fips)

stage <- df %>%
  group_by(year, agerc) %>%
  summarise(perwt=sum(perwt))
saveRDS(stage, paste0(cadir, "ca_stage.rds"))

stage <- readRDS(paste0(cadir, "ca_stage.rds"))
stage %>%
  group_by(year) %>%
  summarise(perwt=sum(perwt) / 1e6) %>%
  ggplot(aes(year, perwt)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(30, 90, 1))


#****************************************************************************************************
#                New Hampshire ####
#****************************************************************************************************
# https://www.nh.gov/osi/data-center/documents/2016-state-county-projections-final-report.pdf
cssd <- "./data_raw/Case Study States/"
nhp <- read_excel(paste0(cssd, "NH/", "NH_pop_projections_2016.xlsx"))
nhp

saveRDS(nhp, paste0(cssd, "NH/", "nh_stage.rds"))


#****************************************************************************************************
#                New York ####
#****************************************************************************************************
# https://pad.human.cornell.edu/counties/projections.cfm
# padprojections0.xls

nyp <- read_excel(paste0(cssd, "NY/", "padprojections0_savedasxlsx.xlsx")) # I had to fix a corrupted file
glimpse(nyp)

saveRDS(nyp, paste0(cssd, "NY/", "ny_stage.rds"))



#****************************************************************************************************
#                Ohio ####
#****************************************************************************************************
# https://www.development.ohio.gov/files/research/P6001.pdf
# P6001_djb.xlsx

ohp <- read_excel(paste0(cssd, "OH/", "P6001_djb.xlsx")) # I had to fix a corrupted file
glimpse(ohp)

saveRDS(ohp, paste0(cssd, "OH/", "oh_stage.rds"))



#****************************************************************************************************
#                Tennessee ####
#****************************************************************************************************
# 
# http://tndata.bus.utk.edu/data/TN_CoPopProj_2017_CoByAge.xlsx
tnp <- read_excel(paste0(cssd, "TN/", "TN_CoPopProj_2017_CoByAge_djb.xlsx")) # data only - original workbook is huge with charts
glimpse(tnp)
# no totals for any of the variables
count(tnp, FIPS, County) %>% ht # does not appear to be a state summary
count(tnp, Age) # 18 groups
count(tnp, Race) # 4 groups
count(tnp, Sex) # Female Male
count(tnp, Year) # 2016-2070 by year

tnp2 <- tnp %>%
  select(year=Year, agegroup=Age, pop=Pop) %>%
  group_by(year, agegroup) %>%
  summarise(pop=sum(pop))

saveRDS(tnp2, paste0(cssd, "TN/", "TN_stage.rds"))


#****************************************************************************************************
#                Texas ####
#****************************************************************************************************
# https://demographics.texas.gov/Data/TPEPP/Projections/
# https://demographics.texas.gov/Resources/TPEPP/Projections/2018/table2/indage/State_of_Texas.zip
# State_of_Texas.csv

txp <- read_csv(paste0(cssd, "TX/", "State_of_Texas.csv"))
glimpse(txp)
count(txp, area_name)
count(txp, age_in_yrs_num) %>% ht # -1 to 95 -- -1 is all years, 95 is 95+

saveRDS(txp, paste0(cssd, "TX/", "TX_stage.rds"))


                