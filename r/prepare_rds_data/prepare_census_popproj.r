
source("./r/includes/libraries.r")
source("./r/includes/globals.r")
source("./r/includes/functions.r")


#****************************************************************************************************
#                Prepare Census population projections ####
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
saveRDS(ppdf, "./data/popproj/census_popproj.rds")


#****************************************************************************************************
#                Examine Census population projections ####
#****************************************************************************************************
# Census Bureau dependency measures
# Age dependency ratio: divide combined under 18 years and 65 years and over by the 18-64 years population and multiplying by 100.
# (American Community Survey and Population Estimates Program)
# Old age dependency ratio: divide population 65 years and over by the 18 to 64 years population and multiplying by 100. (American Community Survey)


ppdf <- readRDS("./data/popproj/census_popproj.rds")
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
acuts <- c(-Inf, 17, 64, Inf)
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
    age < 18 ~ "young",
    age >= 18 & age < 65 ~ "working",
    TRUE ~ "error")) %>%
  group_by(year, dpop) %>%
  summarise(pop=sum(pop)) %>%
  spread(dpop, pop) %>%
  mutate(dep_ratio=(old + young) / working * 100,
         old_depratio=old / working * 100) %>%
  gather(variable, value, contains("ratio")) %>%
  filter(variable!="dep_ratio") %>%
  ggplot(aes(year, value, colour=variable)) + geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(2010, 2080, 5)) +
  scale_y_continuous(breaks=seq(0, 100, 2))

ppdf %>%
  filter(age>=0, sex==0, nativity==0, race_hisp==0) %>%
  mutate(dpop=case_when(age >= 65 ~ "old",
                        age < 18 ~ "young",
                        age >= 18 & age < 65 ~ "working",
                        TRUE ~ "error")) %>%
  group_by(year, dpop) %>%
  summarise(pop=sum(pop)) %>%
  spread(dpop, pop) %>%
  mutate(total=young + working + old) %>%
  mutate_at(vars(young, working, old), funs(. / total * 100)) %>%
  gather(age_group, share, -year, -total) %>%
  group_by(age_group) %>%
  mutate(change=share - share[year==2016]) %>%
  ggplot(aes(year, change, colour=age_group)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks=seq(2010, 2080, 5)) +
  scale_y_continuous(breaks=seq(-100, 100, 1)) +
  ggtitle("Change in share versus 2016, by age group (working=18-64)")


