

#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")
search()
library("DBI")


#****************************************************************************************************
#                Get crosswalks ####
#****************************************************************************************************
(agexwalk <- read_excel("./data/popgroups_crosswalk.xlsx", sheet="xwalk", skip=2))
agexwalk <- agexwalk %>%
  filter(uagroup!="a65p")
agexwalk

# prepare pop projections - filter and add merging variable pgroup
wpop.tots <- wpop %>% 
  filter(sex=="Total", popgroup!="poptot") %>%
  select(year, stabbr, popgroup, pop=value) %>%
  left_join(agexwalk %>% select(popgroup=groupname, pgroup=lab))

# check - good, it looks right
wpop.tots %>%
  group_by(stabbr, year) %>%
  summarise(pop=sum(pop)) %>%
  spread(year, pop)


#****************************************************************************************************
#               get ACS household heads - serial #, stabbr, weight, np -- and save ####
#****************************************************************************************************

hfiles <- paste0(hcsvdir, hfnbase, c("a", "b", "c", "d"), ".csv")
hfiles

# define columns to read
colexp <- cols_only(SERIALNO="c", ST="c", WGTP="d", NP="i")

f <- function(hfile) read_csv(hfile, col_types = colexp, n_max=-1)
system.time(hheads <- ldply(hfiles, f, .progress="text"))
glimpse(hheads)
ht(hheads)

hhh2 <- hheads %>%
  setNames(str_to_lower(names(.))) %>%
  mutate(stabbr=stcodes$stabbr[match(st, stcodes$stfips)])
glimpse(hhh2)
count(hhh2, stabbr)
quantile(hhh2$wgtp, 0:10/10)
saveRDS(hhh2, "./data/ACS/acs_hheads20175year.rds")

# df2 <- readRDS("./data/ACS/acs_hheads20175year.rds")
# ht(df2)


#****************************************************************************************************
#               get ACS person records, with hh link, and save ####
#****************************************************************************************************
#.. get ENTIRE population so that we can match them with housholds!
acsdb <- dbConnect(RSQLite::SQLite(), dbf)
tname <- "acs2017_5year"
dbListTables(acsdb)
dbListFields(acsdb, tname)
getall <- tbl(acsdb, tname) # dplyr does lazy loading, so this does not really grab full table
str(getall)
glimpse(getall)

phhold <- getall %>%
  select(serialno, sporder, agep, pwgtp)
# ht(slg) # tail not supported
# DON'T USE glimpse in this situation - it takes a long time
system.time(phhold <- collect(phhold, n=Inf)) # ~ 1 min
glimpse(phhold)
count(phhold, sporder)
saveRDS(phhold, "./data/ACS/acs_phhold20175year.rds")

df <- readRDS("./data/ACS/acs_phhold20175year.rds")
glimpse(df)


#****************************************************************************************************
#  calculate ACS "headship rates" by state, per OH analysis, link to Weldon Cooper projections, and save hhpop_by_hgroup ####
#****************************************************************************************************
#.. combine previously created hheads and person in hhold data
hheads <- readRDS("./data/ACS/acs_hheads20175year.rds")
phhold <- readRDS("./data/ACS/acs_phhold20175year.rds")

hhhp <- left_join(hheads %>% select(serialno, stabbr, np, wgtp),
                  phhold, by="serialno")
system.time(hhhp2 <- hhhp %>% left_join(hhhp %>% filter(sporder==1) %>% select(serialno, hhhage=agep)))
glimpse(hhhp2)
ht(hhhp2)

# get hcuts and pcuts for defining hh head and person pop age groups
hcuts <- c(-Inf, agexwalk %>% filter(grouptype=="cex") %>% .[["minage_source"]], 100) %>% unique %>% sort
pcuts <- c(-Inf, agexwalk %>% filter(grouptype=="cooper") %>% .[["minage_source"]], 100) %>% unique %>% sort
hcuts
pcuts

a <- proc.time()
grouped <- hhhp2 %>%
  filter(wgtp > 0, np>0) %>%
  mutate(hgroup=cut(hhhage, hcuts, right=FALSE),
         pgroup=cut(agep, pcuts, right=FALSE)) %>% 
  ungroup
b <- proc.time()
b - a

#.. use these grouped data of hhhead-pop to estimate the distribution of households in 2010, ..., 2040 ----

# first, we need to collapse the data to get #hhheads and #pop by hhage-cex and popage--cooper groups
# and, within each popgroup, get the % of pop in each hhage group
# collapse the grouped individual file to a grouped hh file and share the hhnum equally across the person records
# or would it be better to do it in proportion to person weight within each hh? that would be much more computation
stgroupsums <- grouped %>%
  group_by(stabbr, hgroup, pgroup) %>%
  summarise(hnum=sum(wgtp / np), pnum=sum(pwgtp)) # spread the household weight across all persons in household 
ht(stgroupsums)
sum(stgroupsums$hnum)
sum(stgroupsums$pnum)

usgroupsums <- stgroupsums %>%
  group_by(hgroup, pgroup) %>%
  summarise(hnum=sum(hnum), pnum=sum(pnum)) %>%
  mutate(stabbr="US")

usstgroupsums <- bind_rows(stgroupsums, usgroupsums) # this is the base file we need to distribute hh across hgroups based on pop

#.. now get # hh per pop using pop projections, and collapse by hgroup to get # hh by hgroup ----
# in ACS: within each pop group, get share of pop in each hh grup
# also get ratio of hh to pop
# then, using pop proj, spread pop across hh groups, then
#  multiply by hh/pop ratio to get hh in each pop-hh group
# then sum within each hh group to get # hh by hh group

usstgroupsums <- readRDS("./data/usstgroupsums.rds")
glimpse(usstgroupsums)

# for each pgroup, for each hgroup, we need the ratio of hnum to pnum and we also need, for the pgroup, the share of hh in each hgroup
ratios <- usstgroupsums %>%
  mutate(hpratio=hnum / pnum) %>%
  group_by(stabbr, pgroup) %>%
  mutate(popshare=pnum / sum(pnum)) %>%
  ungroup %>%
  arrange(stabbr, pgroup, hgroup)
glimpse(ratios)

ratios %>%
  filter(stabbr=="NY") %>%
  arrange(pgroup, hgroup) %>%
  head(14)
ht(ratios)

ratios %>%
  filter(stabbr=="CA") %>%
  arrange(pgroup, hgroup) %>%
  write_csv("./ca.csv")

# how do the pop totals look?
ratios %>%
  group_by(stabbr) %>%
  summarise(pop=sum(pnum), hh=sum(hnum))

# good, now we can estimate hhnum by popgroup, if we have pop

#.. distribute pop proj by state and year across hh age groups ----
hhest <- wpop.tots %>%
  left_join(ratios) %>%
  # spread pop in each pgroup across the hgroups using popshare
  group_by(stabbr, year, pgroup) %>%
  mutate(pop_in_hgroup=pop * popshare,
         hh_in_hgroup=pop_in_hgroup * hpratio)

hhest %>%
  filter(year==2010, stabbr=="NY", popgroup=="pop15_19")

hhest %>%
  group_by(stabbr, year) %>%
  summarise(pop=sum(pop_in_hgroup), hh=sum(hh_in_hgroup)) %>%
  filter(stabbr=="NY")

#.. get sums of pop and hh by hgroup, put agegroup linking variables on file and save ----
agexwalk
hhpop_by_hgroup <- hhest %>%
  group_by(stabbr, year, hgroup) %>%
  summarise(pop=sum(pop_in_hgroup), hhnum=sum(hh_in_hgroup)) %>%
  ungroup %>%
  left_join(agexwalk %>% filter(grouptype=="cex") %>% select(hgroup=lab, cexgroup=groupname))
count(hhpop_by_hgroup, hgroup, cexgroup) # good, it is properly ordered

# compare growth rates for pop vs hhnum by state and age group
hhpop_by_hgroup %>%
  gather(variable, value, pop, hhnum) %>%
  spread(year, value) %>%
  mutate(pch2040=`2040` / `2020` * 100 - 100) %>%
  kable(digits=c(rep(0, 8), 1), format.args=list(big.mark=","))
# generally fairly similar

saveRDS(hhpop_by_hgroup, "./data/ACS/acs_hhpop_by_hgroup.rds") # IMPORTANT!! ----


# compare growth rates for pop vs hhnum by state and hh age group
hhproj %>%
  gather(variable, value, pop, hhnum) %>%
  spread(year, value) %>%
  mutate(pch2040=`2040` / `2020` * 100 - 100) %>%
  filter(stabbr %in% c("CA", "NH")) %>%
  arrange(stabbr, cexgroup, variable)

