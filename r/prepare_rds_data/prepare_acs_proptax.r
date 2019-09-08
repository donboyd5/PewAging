
# NOTE: See documentation far below

#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")
search()

library("DBI")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
# define locations
dbdir <- "D:/Data/CensusACS/20175year/RSQLITE/"
hcsvdir <- "D:/Data/CensusACS/20175year/csv_hus/"
hfnbase <- "psam_hus"

dbf <- paste0(dbdir, "acs.sqlite")


#****************************************************************************************************
#                Getting property value and property tax from ACS ####
#****************************************************************************************************
# see key variable definitions below

#.. ONETIME: get and save housing records ----
hfiles <- paste0(hcsvdir, hfnbase, c("a", "b", "c", "d"), ".csv")
hfiles

# define columns to read
colexp <- cols_only(RT="c", SERIALNO="c", ST="c",
                    ADJHSG="d", ADJINC="d", WGTP="d", NP="i", TYPE="i", 
                    TEN="i", VALP="d", HINCP="d", TAXP="i")

f <- function(hfile) read_csv(hfile, col_types = colexp, n_max=-1)
system.time(hus <- ldply(hfiles, f, .progress="text"))
glimpse(hus)
ht(hus)
hus <- hus %>%
  setNames(str_to_lower(names(.)))
saveRDS(hus, "./data/acs_hus20175year.rds")
#.. END ONETIME ----


# get owner-occupied housing units and match them with household heads ----
hus <- readRDS("./data/acs_hus20175year.rds")
glimpse(hus)

# get owner occupied, adjust as needed
husown <- hus %>%
  filter(ten %in% 1:2) %>%
  mutate_at(vars(adjhsg, adjinc), ~ . / 1e6)
# describe(husown) # takes a minute or two, but good
summary(husown)
glimpse(husown)

# ONETIME: get householder person records so we can match with the owner occupied housing ----
acsdb <- dbConnect(RSQLite::SQLite(), dbf)
tname <- "acs2017_5year"
dbListTables(acsdb)
dbListFields(acsdb, tname)
getall <- tbl(acsdb, tname) # dplyr does lazy loading, so this does not really grab full table
# getall <- tbl(acsdb, sql("SELECT * FROM acs2016_5year")) # dplyr does lazy loading, so this does not really grab full table
str(getall)
glimpse(getall)

phhold <- getall %>% 
  filter(sporder==1) %>%
  select(serialno, agep, adjinc.p=adjinc, pwgtp) %>%
  mutate(adjinc.p=adjinc.p / 1e6)
# ht(slg) # tail not supported
# DON'T USE glimpse in this situation - it takes a long time
system.time(phhold <- collect(phhold, n=Inf)) # ~ 1 min
glimpse(phhold)

# ONETIME: combine husown and pphold and save ----
hhp <- husown %>%
  left_join(phhold)
saveRDS(hhp, "./data/acs_hhp20175year.rds")


#****************************************************************************************************
#                Analyze the property value and property tax data from ACS ####
#****************************************************************************************************


#.. plot rvalp by age, case study states ----
glimpse(hhp2)
p <- hhp2 %>%
  filter(stabbr %in% globals$case_study_states, agep >= 20) %>%
  ggplot(aes(x=agep, y=rvalp, colour=stabbr, weight=wgtp)) +
  geom_smooth(se=FALSE)
# save plot details so that we can clean it without having to rebuild all the time
d <- ggplot_build(p)
str(d)

data <- d$data[[1]]
glimpse(data)
summary(data)
clrs <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c')
capt1 <- "Source: Author's analysis of American Community Survey, 2013-2017 5-Year PUMS"
capt2 <- "Note: Housing values are smoothed using a generalized additive model"
capt <- paste0(capt1, "\n", capt2)
title <- "Average home value by age in case study states"
brks <- seq(0, 10e6, 50e3)
p1 <- data %>%
  mutate(stabbr=sort(globals$case_study_states)[group]) %>%
  ggplot(aes(x, y, colour=stabbr)) +
  geom_line(size=rel(1.3)) + 
  scale_x_continuous(name="Householder age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="Home value in $ thousands (self-reported)", breaks=brks, limits=c(0, NA), labels = scales::dollar(brks/1000)) +
  theme_bw() +
  scale_colour_manual(values=clrs) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle(title, subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p1
mult <- 2
w <- 6.5 * mult; h <- 5 * mult
w <- 8; h <- 11 / 2
ggsave(plot=p1, filename="./results/rvalp_byage_css_xus.png", width=w, height=h, units="in")


#.. plot rptax / rvalp by age, case study states ----
# FIRST, build a plot with smoothed rptax and rvalp, THEN compute pct
p3 <- hhp2 %>%
  filter(stabbr %in% globals$case_study_states, agep>=20) %>%
  select(stabbr, wgtp, agep, rptax, rvalp) %>%
  gather(variable, value, rptax, rvalp) %>%
  ggplot(aes(x=agep, y=value, colour=stabbr, weight=wgtp)) +
  geom_smooth(se=FALSE) +
  facet_wrap(~variable)
d3 <- ggplot_build(p3) # this can take a little time
names(d3)

data3 <- d3$data[[1]]
count(data3, group)
data3a <- data3 %>%
  mutate(stabbr=sort(globals$case_study_states)[group],
         variable=ifelse(PANEL==1, "rptax", "rvalp")) %>%
  select(stabbr, variable, agep=x, value=y) %>%
  spread(variable, value) %>%
  mutate(taxpct=rptax / rvalp * 100) %>%
  arrange(stabbr, agep)
ht(data3a)

data3a %>%
  ggplot(aes(agep, taxpct, colour=stabbr)) +
  geom_line()

clrs <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c')
capt1 <- "Source: Author's analysis of American Community Survey, 2013-2017 5-Year PUMS"
capt2 <- "Note: Property tax and housing values are smoothed using a generalized additive model"
capt <- paste0(capt1, "\n", capt2)
title1 <- "Average homeowner property tax as % of home value by age"
title2 <- "in case study states"
title <- paste0(title1, "\n", title2)
p4 <- data3a %>%
  ggplot(aes(agep, taxpct, colour=stabbr)) +
  geom_line(size=rel(1.3)) + 
  scale_x_continuous(name="Householder age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="Property tax as % of home value\n(based on self-reported values)",
                     breaks=seq(0, 5, 0.25), limits=c(0, NA)) +
  theme_bw() +
  scale_colour_manual(values=clrs) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle(title, subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p4
# mult <- 2
# w <- 6.5 * mult; h <- 5 * mult
w <- 8; h <- 11 / 2
ggsave(plot=p4, filename="./results/taxpct_byage_css_xus.png", width=w, height=h, units="in")


#****************************************************************************************************
#                What would happen to prop tax if we had 2040 income dist rather than 2020? ####
#****************************************************************************************************
wpop <- readRDS("./data/popproj/wc_popproj.rds") # Weldon Cooper pop projections
glimpse(wpop)
count(wpop, popgroup) # get age cuts needed
popgroup <- count(wpop, popgroup) %>% 
  filter(popgroup!="poptot")

# prepare the Weldon Cooper popshares data - for each state and income group, get share of state pop different years
wpop.wide <- wpop %>% 
  filter(sex=="Total", popgroup!="poptot") %>%
  mutate(year=paste0("pop", year)) %>%
  group_by(year, stabbr) %>%
  select(stabbr, year, popgroup, value) %>%
  spread(year, value)

# make a data frame to link popgroups on
agebrks <- c(-1, seq(4, 84, 5), 1e9)
agegroups <- tibble(age=1:100, agebrk=cut(age, agebrks)) %>%
  group_by(agebrk) %>%
  summarise(n=n()) %>%
  mutate(popgroup=popgroup$popgroup) %>%
  select(-n)
agegroups # good, we can use this for linking

# get the tax data from the ACS and collapse by stabbr, age group ----
hhp <- readRDS("./data/hhp20175year.rds")
glimpse(hhp)
ht(hhp)

# function to estimate the unit's property tax
ptax.low <- c(0, 1, seq(50, 950, 50), seq(1000, 5000, 100), 5500, seq(6000, 10000, 1000))
ptax.high <- c(ptax.low[-1] - 1, 12000) # need to choose a high value????
ptax.vals <- tibble(taxp=1:68, ptax.low=ptax.low, ptax.high=ptax.high) %>%
  mutate(ptax.mid=(ptax.low + ptax.high) / 2)
ptax.vals %>% ht

hhp2 <- hhp %>%
  select(-adjinc.p) %>%
  left_join(ptax.vals %>% select(taxp, ptax=ptax.mid)) %>%
  mutate(stabbr=stcodes$stabbr[match(st, stcodes$stfips)],
         rvalp=valp * adjhsg,
         rptax=ptax * adjhsg,
         rhincp=hincp * adjinc)
glimpse(hhp2)

# get acs by age group
hhp.age <- hhp2 %>% 
  mutate(agebrk=cut(agep, agebrks)) %>%
  group_by(stabbr, agebrk) %>%
  summarise(wtdn=sum(wgtp), 
            rptax=weighted.mean(rptax, wgtp)) %>%
  left_join(agegroups) %>% # this allows us to link to Weldon Cooper projections
  group_by(stabbr) %>%
  mutate(totpop=sum(wtdn)) %>%
  ungroup
count(hhp.age, popgroup)
hhp.age
hhp.age %>% filter(stabbr=="NY")
hhp.age %>% filter(popgroup=="pop15_19") # does not include DC


# put the shares on the tax data
shr <- function(var) var / sum(var)
rptaxshares <- hhp.age %>% 
  filter(popgroup!="pop15_19") %>%
  select(-totpop) %>%
  select(stabbr, agebrk, popgroup, rptax, wtdn, everything()) %>%
  group_by(stabbr) %>%
  mutate(hhtot=sum(wtdn)) %>%
  left_join(wpop.wide, by=c("stabbr", "popgroup")) %>%
  mutate_at(vars(wtdn, starts_with("pop2")), list(ptax= ~rptax * hhtot * . / sum(.)))

rptaxshares %>% 
  filter(stabbr=="TX") %>%
  adorn_totals()

# prepare acs data for merging against pop projections data
# total population and mean rtax by state and age group, plus total pop [2017, 2018 pooled]
# thus, all the pops are 2-year sums; the taxes are average annual tax
spop.age <- perdf %>% 
  mutate(agebrk=cut(age, agebrks)) %>%
  group_by(stabbr, agebrk) %>%
  summarise(wtdn=sum(avgweight), 
            rtax=weighted.mean(rtax, avgweight)) %>%
  left_join(agegroups) %>% # this allows us to link to Weldon Cooper projections
  group_by(stabbr) %>%
  mutate(totpop=sum(wtdn)) %>%
  ungroup
spop.age
spop.age %>% filter(stabbr=="NY")


#****************************************************************************************************
#                Data documentation ####
#****************************************************************************************************

# ADJHSG      7
# Adjustment factor for housing dollar amounts (6 implied decimal places)
# 1045360          .2012 factor
# 1030392          .2013 factor
# 1013801          .2014 factor
# 1012636          .2015 factor
# 1000000          .2016 factor
# 
# NOTE: The values of ADJHSG inflation-adjusts reported housing costs to 2016 dollars and applies to variables CONP, ELEP, FULP, GASP, GRNTP, INSP, MHP, MRGP, SMOCP, RNTP, SMP, and WATP in the housing record. ADJHSG does not apply to AGS or TAXP because they are categorical variables that should not be inflation-adjusted.
# 
# ADJINC      7
# Adjustment factor for income and earnings dollar amounts (6 implied decimal places)
# 1056030          .2012 factor (1.010207 * 1.04536021)
# 1038170          .2013 factor (1.007549 * 1.03039158)
# 1022342          .2014 factor (1.008425 * 1.01380104)
# 1013916          .2015 factor (1.001264 * 1.01263642)
# 1007588          .2016 factor (1.007588 * 1.00000000)


# TEN Character 1
# Tenure
# b .N/A (GQ/vacant)
# 1 .Owned with mortgage or loan (include home equity loans)
# 2 .Owned free and clear
# 3 .Rented
# 4 .Occupied without payment of rent

# VALP        7
# Property value
# bbbbbbb          .N/A (GQ/vacant units, except "for-sale-only" and "sold, not occupied"/not owned or being bought)
# 0000001..9999999 .$1 to $9999999 (Rounded and top-coded***)
# 
# 
# FTAXP       1
# Property taxes (yearly amount) allocation flag
# b    .N/A (GQ)
# 0    .No
# 1    .Yes

# TAXP Character 2
# Property taxes (yearly amount, no adjustment factor is applied)
# bb .N/A (GQ/vacant/not owned or being bought)
# 01 .None
# 02 .$ 1 - $ 49
# 03 .$ 50 - $ 99
# 04 .$ 100 - $ 149
# 05 .$ 150 - $ 199
# 06 .$ 200 - $ 249
# 07 .$ 250 - $ 299
# 08 .$ 300 - $ 349
# 09 .$ 350 - $ 399
# 10 .$ 400 - $ 449
# 11 .$ 450 - $ 499
# 12 .$ 500 - $ 549
# 13 .$ 550 - $ 599
# 14 .$ 600 - $ 649
# 15 .$ 650 - $ 699
# 16 .$ 700 - $ 749
# 17 .$ 750 - $ 799
# 18 .$ 800 - $ 849
# 19 .$ 850 - $ 899
# 20 .$ 900 - $ 949
# 21 .$ 950 - $ 999
# 22 .$1000 - $1099
# 23 .$1100 - $1199
# 24 .$1200 - $1299
# 25 .$1300 - $1399
# 26 .$1400 - $1499
# 27 .$1500 - $1599
# 28 .$1600 - $1699
# 29 .$1700 - $1799
# 30 .$1800 - $1899
# 31 .$1900 - $1999
# 32 .$2000 - $2099
# 33 .$2100 - $2199
# 34 .$2200 - $2299
# 35 .$2300 - $2399
# 36 .$2400 - $2499
# 37 .$2500 - $2599
# 38 .$2600 - $2699
# 39 .$2700 - $2799
# 40 .$2800 - $2899
# 41 .$2900 - $2999
# 42 .$3000 - $3099
# 43 .$3100 - $3199
# 44 .$3200 - $3299
# 45 .$3300 - $3399
# 46 .$3400 - $3499
# 47 .$3500 - $3599
# 48 .$3600 - $3699
# 49 .$3700 - $3799
# 50 .$3800 - $3899
# 51 .$3900 - $3999
# 52 .$4000 - $4099
# 53 .$4100 - $4199
# 54 .$4200 - $4299
# 55 .$4300 - $4399
# 56 .$4400 - $4499
# 57 .$4500 - $4599
# 58 .$4600 - $4699
# 59 .$4700 - $4799
# 60 .$4800 - $4899
# 61 .$4900 - $4999
# 62 .$5000 - $5499
# 63 .$5500 - $5999
# 64 .$6000 - $6999
# 65 .$7000 - $7999
# 66 .$8000 - $8999
# 67 .$9000 - $9999
# 68 .$10000+ (Top-coded)


