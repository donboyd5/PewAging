

#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
# search()

source("./r/includes/globals.r")
source("./r/includes/functions.r")


#****************************************************************************************************
#                Weldon Cooper and Census data ####
#****************************************************************************************************
wpop <- readRDS("./data/popproj/wc_popproj.rds") # Weldon Cooper pop projections
glimpse(wpop)
count(wpop, popgroup)

cpop <- readRDS("./data/popproj/census_popproj.rds") # Census pop projections
glimpse(cpop)
count(cpop, age) # single year of age, national

# CDC Wonder access to state projections - these are Census projections
# https://wonder.cdc.gov/population-projections.html
# "Notes"	"Age Group"	"Age Group Code"	"State"	"State Code"	"Year"	"Year Code"	Projected Populations
df <- read_delim(paste0("./data_raw/CDC Wonder/", "age group state year.txt"),
                 delim="\t", skip=1, 
                 col_names=c("note", "agegroup", "agecode", "stname", "stfips", "year", "yearcode", "pop"))
ht(df)
count(df, stcode, stname)

df2 <- df %>%
  filter(!is.na(pop)) %>%
  select(-note, -yearcode) %>%
  mutate(stabbr=stcodes$stabbr[match(stfips, stcodes$stfips)],
         year=as.integer(year)) %>%
  select(stabbr, stname, stfips, year, agegroup, agecode, pop)
count(df2, stfips, stabbr, stname)
count(df2, agegroup, agecode)
ht(df2)

cdcpop <- df2 %>%
  select(-stfips, -stname, -agegroup)
ht(cdcpop)

#****************************************************************************************************
#                WC vs. CDC (Census) data ####
#****************************************************************************************************
# create uniform files, total pop by conforming age groups, state, and year
globals$wc_young
globals$wc_workage
globals$wc_older

#.. Weldon Cooper ----
glimpse(wpop)
wdf <- wpop %>%
  filter(sex=="Total", stabbr!="US") %>%
  mutate(agroup=case_when(popgroup %in% globals$wc_young ~ "young",
                         popgroup %in% globals$wc_workage ~ "workage",
                         popgroup %in% globals$wc_older ~ "older",
                         TRUE ~ "other")) %>%
  filter(agroup!="other") %>%
  group_by(stabbr, year, agroup) %>%
  summarise(pop=sum(value))
ht(wdf)

#.. CDC ----
glimpse(cdcpop)
clow <- seq(0, 85, 5)
chi <- seq(4, 89, 5)
(cgrps <- paste0(clow, "-", chi))
cgrps[length(cgrps)] <- "85+"
cgrps
(cyoung <- cgrps[1:4])
(cworkage <- cgrps[5:13])
(colder <- cgrps[14:length(cgrps)])
cdf <- cdcpop %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(agecode %in% cyoung ~ "young",
                          agecode %in% cworkage ~ "workage",
                          agecode %in% colder ~ "older",
                          TRUE ~ "other")) %>%
  filter(agroup!="other") %>%
  group_by(stabbr, year, agroup) %>%
  summarise(pop=sum(pop))

pop <- bind_rows(wdf %>% mutate(src="weldon"),
                 cdf %>% mutate(src="cdc"))

pop %>%
  filter(stabbr %in% globals$case_study_states, year <= 2030) %>%
  group_by(src, stabbr, year) %>%
  mutate(pct=pop / sum(pop) * 100) %>%
  select(-pop) %>%
  spread(src, pct) %>%
  mutate(diff=weldon - cdc)

# get 2020-2030 change
pop %>%
  filter(stabbr %in% globals$case_study_states, year %in% c(2020, 2030)) %>%
  group_by(src, stabbr, year) %>%
  mutate(pct=pop / sum(pop) * 100) %>%
  select(-pop) %>%
  spread(year, pct) %>%
  mutate(change=`2030` - `2020`) %>%
  select(stabbr, agroup, src, change) %>%
  spread(src, change) %>%
  mutate(diff=cdc - weldon)


#****************************************************************************************************
#                State data ####
#****************************************************************************************************
cssd <- "./data_raw/Case Study States/"


globals$case_study_states
# "CA" "NH" "NY" "OH" "TN" "TX"

#.. CA ----
# previously grouped by year and age, from county-sex-race-age data
cap <- readRDS(paste0(cssd, "CA/", "ca_stage.rds"))
max(cap$agerc)

cadf <- cap %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(agerc %in% 0:19 ~ "young",
                          agerc %in% 20:64 ~ "workage",
                          agerc %in% 65:100 ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(perwt)) %>%
  mutate(stabbr="CA", src="state")


#.. NH ----
clow <- seq(0, 85, 5)
chi <- seq(4, 89, 5)
(cgrps <- paste0(clow, "-", chi))
cgrps[length(cgrps)] <- "85+"
cgrps
(cyoung <- cgrps[1:4])
(cworkage <- cgrps[5:13])
(colder <- cgrps[14:length(cgrps)])

nhp <- readRDS(paste0(cssd, "NH/", "nh_stage.rds"))
nhdf <- nhp %>%
  filter(agegroup!="Total") %>%
  gather(year, pop, -agegroup) %>%
  mutate(year=as.integer(year)) %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(agegroup %in% cyoung ~ "young",
                          agegroup %in% cworkage ~ "workage",
                          agegroup %in% colder ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(pop)) %>%
  mutate(stabbr="NH", src="state")


#.. NY ----
nyp <- readRDS(paste0(cssd, "NY/", "ny_stage.rds"))
glimpse(nyp)
count(nyp, COUNTY, COUNTY_DESCR) # it is just NYS
count(nyp, SEXCODE, SEX_DESCR) # 0 is all
count(nyp, AGEGRPCODE, AGEGRP_DESCR) %>% ht # AGEGRPCODE -999 is total, 999 is median, 85 is 85+
nyp %>% filter(AGEGRPCODE!=999) %>% group_by(AGEGRPCODE==-999) %>% summarise(YR_2020=sum(YR_2020), YR_2040=sum(YR_2040)) # exact, we can drop -999
count(nyp, RACECODE, RACE_DESCR) # it is just total


nydf <- nyp %>%
  filter(COUNTY==0, SEXCODE==0, !AGEGRPCODE %in% c(-999, 999), RACECODE==0) %>% # COUNTY, RACECODE just to be safe
  select(age=AGEGRPCODE, starts_with("YR")) %>%
  gather(year, pop, -age) %>%
  mutate(year=str_sub(year, -4, -1) %>% as.integer) %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(age %in% 0:19 ~ "young",
                          age %in% 20:64 ~ "workage",
                          age %in% 65:85 ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(pop)) %>%
  mutate(stabbr="NY", src="state")



#.. OH ----
ohp <- readRDS(paste0(cssd, "OH/", "oh_stage.rds"))
glimpse(ohp)

clow <- seq(0, 85, 5)
chi <- seq(4, 89, 5)
(cgrps <- paste0(clow, "-", chi))
cgrps[length(cgrps)] <- "85+"
cgrps
(cyoung <- cgrps[1:4])
(cworkage <- cgrps[5:13])
(colder <- cgrps[14:length(cgrps)])


ohdf <- ohp %>%
  filter(agegroup!="TOTAL") %>%
  gather(year, pop, -agegroup) %>%
  mutate(year=as.integer(year)) %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(agegroup %in% cyoung ~ "young",
                          agegroup %in% cworkage ~ "workage",
                          agegroup %in% colder ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(pop)) %>%
  mutate(stabbr="OH", src="state")


#.. TN ----
clow <- seq(0, 85, 5)
chi <- seq(4, 89, 5)
(cgrps <- paste0(clow, " to ", chi))
cgrps[1] <- "under 5"
cgrps[length(cgrps)] <- "85 and up"
cgrps
(cyoung <- cgrps[1:4])
(cworkage <- cgrps[5:13])
(colder <- cgrps[14:length(cgrps)])

tnp <- readRDS(paste0(cssd, "TN/", "tn_stage.rds"))
tndf <- tnp %>%
  filter(year %in% seq(2010, 2040, 10)) %>%
  mutate(agroup=case_when(agegroup %in% cyoung ~ "young",
                          agegroup %in% cworkage ~ "workage",
                          agegroup %in% colder ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(pop)) %>%
  mutate(stabbr="TN", src="state")


#.. TX ----
txp <- readRDS(paste0(cssd, "TX/", "TX_stage.rds"))
glimpse(txp)
count(txp, area_name)
count(txp, age_in_yrs_num) %>% ht


txdf <- txp %>%
  select(year, age=age_in_yrs_num, pop=total) %>%
  filter(year %in% seq(2010, 2040, 10), age!=-1) %>%
  mutate(agroup=case_when(age %in% 0:19 ~ "young",
                          age %in% 20:64 ~ "workage",
                          age %in% 65:95 ~ "older",
                          TRUE ~ "other")) %>%
  group_by(year, agroup) %>%
  summarise(pop=sum(pop)) %>%
  mutate(stabbr="TX", src="state")



#.. Examine data ----
pop2 <- bind_rows(pop, cadf, nhdf, nydf, ohdf, tndf, txdf)

pop2 %>%
  filter(src!="cdc") %>%
  filter(stabbr %in% globals$case_study_states, year %in% c(2020, 2030)) %>%
  filter(stabbr %in% c("CA", "NH", "NY", "OH", "TN", "TX")) %>%
  group_by(src, stabbr, year) %>%
  mutate(pct=pop / sum(pop) * 100) %>%
  select(-pop) %>%
  spread(year, pct) %>%
  mutate(change=`2030` - `2020`) %>%
  select(stabbr, agroup, src, change) %>%
  spread(src, change) %>%
  mutate(diff=weldon - state)

# CA  state says they are getting older faster than weldon, young declining, and workage changing about the same
# NH state getting a bit older faster than weldon, workage declining a bit faster
# NY state getting a bit older faster than weldon, workage declining a bit faster
# OH very small differences
# TN small but not tiny differences
# Tx state getting a bit older faster than weldon, young declining faster

# Groups:   stabbr [6]
# stabbr agroup   state weldon    diff
# <chr>  <chr>    <dbl>  <dbl>   <dbl>
# 1 CA     older    4.82   2.49  -2.33  
# 2 CA     workage -2.56  -2.67  -0.118 
# 3 CA     young   -2.26   0.179  2.44  
# 4 NH     older    6.79   5.49  -1.30  
# 5 NH     workage -5.85  -5.09   0.770 
# 6 NH     young   -0.932 -0.404  0.529 
# 7 NY     older    3.82   2.29  -1.53  
# 8 NY     workage -3.41  -2.57   0.836 
# 9 NY     young   -0.415  0.278  0.693 
# 10 OH     older    3.13   3.03  -0.101 
# 11 OH     workage -2.88  -2.69   0.195 
# 12 OH     young   -0.243 -0.338 -0.0944
# 13 TN     older    2.64   2.48  -0.158 
# 14 TN     workage -2.21  -1.95   0.260 
# 15 TN     young   -0.433 -0.534 -0.102 
# 16 TX     older    2.80   2.10  -0.702 
# 17 TX     workage -1.43  -1.53  -0.109 
# 18 TX     young   -1.37  -0.564  0.810 


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************


