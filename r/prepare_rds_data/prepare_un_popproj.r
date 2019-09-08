

source("./r/includes/libraries.r")
source("./r/includes/globals.r")
source("./r/includes/functions.r")


#****************************************************************************************************
#                United Nations population projections ####
#****************************************************************************************************
# Data underlying 2017 Revision of World Population Prospects, The United Nations, https://population.un.org/wpp/
# From: https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationBySingleAgeSex.csv
# https://population.un.org/wpp/Download/Other/Documentation/
# https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationBySingleAgeSex.csv
unpd <- "D:/Data/PopData_misc/UnitedNations/"
unfn <- "WPP2017_PopulationBySingleAgeSex.csv"
df <- read_csv(paste0(unpd, unfn))
glimpse(df)
d <- count(df, LocID, Location)
d %>% filter(str_detect(Location, "United"))

usppun <- df %>% filter(LocID==840) # 840 is USA
ht(usppun)
count(usppun, VarID, Variant) # varid 2 variant medium for all don't need these
count(usppun, Time, MidPeriod) %>% ht(20) # seems like we can just keep Time
unique(usppun$AgeGrp)
count(usppun, AgeGrpStart, AgeGrp, AgeGrpSpan) %>% ht(20)

usppun <- df %>% 
  filter(LocID==840) %>%
  select(year=Time, age=AgeGrpStart, PopMale, PopFemale, pop=PopTotal) %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(age=as.integer(age))
ht(usppun)
count(usppun, age) %>% ht
saveRDS(usppun, "./data_raw/UnitedNations/un_popproj.rds")

# look at the data ----
acuts <- c(-Inf, 17, 64, Inf)
usppun %>%
  mutate(agegrp=cut(age, acuts)) %>%
  group_by(year, agegrp) %>%
  summarise(pop=sum(pop)) %>%
  group_by(year) %>%
  mutate(year_tot=sum(pop)) %>%
  ggplot(aes(year, pop / year_tot * 100, colour=agegrp)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(1900, 2200, 5)) +
  scale_y_continuous(breaks=seq(0, 100, 2)) +
  ggtitle("UN projections for United States")
