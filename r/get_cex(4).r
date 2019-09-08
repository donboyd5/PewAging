

#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("BEAData")
library("DBI")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
#.. CEX files locations ----
cexd <- "D:/Data/CensusCEX/2017/"
cexdi <- paste0(cexd, "intrvw17/")

#.. ACS file locations ----
dbdir <- "D:/Data/CensusACS/20175year/RSQLITE/"
hcsvdir <- "D:/Data/CensusACS/20175year/csv_hus/"
hfnbase <- "psam_hus"

dbf <- paste0(dbdir, "acs.sqlite")


#****************************************************************************************************
#                Get data to use throoughout, including Weldon Cooper projections ####
#****************************************************************************************************
#.. age groups crosswalks ----
(agexwalk <- read_excel("./data/popgroups_crosswalk.xlsx", sheet="xwalk", skip=2))
agexwalk <- agexwalk %>%
  filter(uagroup!="a65p")
agexwalk

#.. Weldon Cooper population projections ----
wpop <- readRDS("./data/popproj/wc_popproj.rds")
glimpse(wpop)
# count(wpop, popgroup)

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
#                ONETIME Get ACS data and save ####
#****************************************************************************************************
#.. get hh heads ----
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
saveRDS(hhh2, "./data/hheads20175year.rds")

#.. get ENTIRE population so that we can match them with housholds! ----
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
saveRDS(phhold, "./data/phhold20175year.rds")


#****************************************************************************************************
#  ONETIME calculate ACS "headship rates" by state per OH analysis link to Weldon Cooper projections and save hhpop_by_hgroup ####
#****************************************************************************************************
#.. combine previously created hheads and person in hhold data
hheads <- readRDS("./data/hheads20175year.rds")
phhold <- readRDS("./data/phhold20175year.rds")
a <- proc.time()
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

saveRDS(hhpop_by_hgroup, "./data/hhpop_by_hgroup.rds") # IMPORTANT!! ----


#****************************************************************************************************
#                Get original source CEX Table 1300 summary data and save as cex_expend with vname ####
#****************************************************************************************************
#.. Get xwalk to taxable expenditures and aggregation rules ----
xwalk1 <- read_excel(paste0("./data/", "CES_Taxable_Crosswalk.xlsx"))
xwalk1
xwalk2 <- xwalk1 %>%
  filter(!is.na(level)) %>%
  select(-starts_with("taxable")) # keep tax rules separate from xwalk, get them later
ht(xwalk2)

#.. Table 1300, age.pdf ----
# D:\Data\CensusCEX\2017\Tables
dir <- paste0(cexd, "Tables/")

df <- read_excel(paste0(dir, "age.xlsx"), skip = 2)
df
names(df)
ncol(df)
(exp_start <- which(df$Item=="Average annual expenditures"))
(exp_end <- which(df$Item=="Sources of income and personal taxes:") -1)

keep <- c("Number of consumer units (in thousands)", "Age of reference person", "People")
df2 <- df %>%
  set_names(c("item", "total", "a00to24", "a25to34", "a35to44", "a45to54", "a55to64", "a65p", "a65to74", "a75p")) %>%
  mutate(group=case_when(row_number() < exp_start ~ "misc",
                         (row_number() >= exp_start) & (row_number() <= exp_end) ~ "expend",
                         TRUE ~ "other"),
         itemlag=lag(item)) %>%
  filter(item=="Mean" | item %in% keep) %>%
  mutate(item=ifelse(item=="Mean", itemlag, item)) %>%
  mutate_at(vars(-item, -group), ~naz(as.numeric(.)))
df2$item %>% sort
count(df2, group, item) %>% as.data.frame
df2
ht(df2)

#.. link to xwalk ----
cx <- df2 %>%
  filter(group %in% c("misc", "expend")) %>%
  select(-itemlag) %>%
  left_join(xwalk2 %>% select(vname, item, level))
glimpse(cx)
count(cx, vname)
ht(cx)

#.. check totals ----
cx %>%
  filter(group=="expend", level < 9, level %in% 0:1) %>%
  group_by(level) %>%
  summarise(total=sum(total))

#.. compute total expends ----
# create 3 files, each by age group - numbers, misc, expend and then merge them
nunits <- cx %>%
  filter(item=="Number of consumer units (in thousands)") %>%
  select(-group, -level) %>%
  mutate(vname="nunitsk") %>%
  gather(agegroup, nunitsk, -item, -vname) %>%
  select(vname, agegroup, nunitsk, item)
nunits
saveRDS(nunits, "./data/cexunits.rds")

misc <- cx %>%
  filter(group=="misc", vname!="nunitsk") %>%
  select(-group, -level) %>%
  gather(agegroup, value, -item, -vname) %>%
  select(vname, agegroup, value, item)
misc

expend <- cx %>%
  filter(group=="expend", level < 9) %>%
  select(-group) %>%
  gather(agegroup, exp_avg, -vname, -item, -level, -vname)
ht(expend)

cex_expend <- expend %>%
  left_join(nunits %>% select(agegroup, nunitsk)) %>%
  mutate(exp_m=exp_avg * nunitsk / 1000) %>%
  select(vname, level, agegroup, nunitsk, exp_avg, exp_m, item)
ht(cex_expend)
saveRDS(cex_expend, "./data/cex_expend")


#****************************************************************************************************
#                analyze taxable sales  ####
#****************************************************************************************************
#.. Get tax rules ----
taxrules <- read_excel(paste0("./data/", "CES_Taxable_Crosswalk.xlsx"))
taxrules <- taxrules %>%
  filter(str_sub(vname, 1, 1)=="v") %>%
  mutate_at(vars(starts_with("taxable")), list(~naz(.))) %>%
  select(vname, starts_with("tax"), item)
ht(taxrules)
taxrules %>%
  filter(taxable_felix!=0)

# .. Merge with expend data ----
cex_expend <- readRDS("./data/cex_expend")
glimpse(cex_expend)
count(cex_expend, vname, item)

# get goods & services spending
gsexp <- cex_expend %>%
  filter(vname %in% c("v001", "v099", "v100", "v101", "v102")) %>%
  select(vname, agegroup, exp_avg, exp_m) %>%
  gather(vtype, value, exp_avg, exp_m) %>%
  spread(vname, value) %>%
  mutate(gsexp=v001 - v099 - v100 - v101 - v102)
gsexp

taxexp <- cex_expend %>%
  left_join(taxrules %>% select(-item)) %>%
  filter(taxable_felix != 0) %>% # to make it easier to see
  mutate(taxexp=exp_m * taxable_felix,
         avgtaxexp=exp_avg * taxable_felix) %>%
  group_by(agegroup) %>%
  summarise_at(vars(avgtaxexp, taxexp), ~sum(.))
ht(taxexp)

txblshare <- gsexp %>%
  select(agegroup, gsexp) %>%
  left_join(taxexp) %>%
  mutate(taxpct=taxexp / gsexp * 100)
txblshare %>% arrange(desc(taxpct))


#****************************************************************************************************
#                graph income, expend, gsexp, and taxexp ####
#****************************************************************************************************
misc
stack <- bind_rows(
  misc %>% filter(vname %in% c("incomebt", "ageref")) %>% select(vname, agegroup, value),
  gsexp %>% filter(vtype=="exp_avg") %>% select(agegroup, totexp=v001, gsexp) %>% gather(vname, value, totexp, gsexp),
  taxexp %>% select(agegroup, value=avgtaxexp) %>% mutate(vname="avgtaxexp")
)
stack

agefact <- stack %>%
  filter(vname=="ageref") %>%
  select(ageref=value, agegroup) %>%
  arrange(ageref)

# brks <- seq(20, 80, 10)
brks <- c(21.4, 29.8, 39.3, 49.7, 59.4, 69, 81.5)
labs <- c("20 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75-plus")

p <- stack %>%
  filter(!agegroup %in% c("total", "a65p")) %>%
  filter(vname %in% c("incomebt", "ageref", "totexp", "gsexp", "avgtaxexp")) %>%
  spread(vname, value) %>%
  gather(vname, value, -agegroup, -ageref) %>%
  mutate(vname=factor(vname, 
                      levels=c("incomebt", "totexp", "gsexp", "avgtaxexp"),
                      labels=c("Income before tax", "Total expenditures", "Goods & services\nexpenditures", "Commonly taxable\nexpenditures"))) %>%
  ggplot(aes(ageref, value, colour=vname)) +
  geom_line(size=1.1) +
  scale_color_manual(values=c("blue", "darkgreen", "orange", "red")) +
  scale_y_continuous(name="Amount in dollars", breaks=seq(0, 200e3, 10e3), labels=scales::dollar, limits = c(0, NA)) +
  scale_x_continuous(name="Age of householder", breaks=brks, labels=labs) +
  ggtitle("Income and expenditures by age of householder", subtitle="Consumer Expenditure Survey, 2017") +
  guides(colour=guide_legend(title=NULL)) +
  theme_bw()
p


#****************************************************************************************************
#                compute tax change per CEX Weldon ####
#****************************************************************************************************
#.. get all needed files ----
# (nunits <- readRDS("./data/cexunits.rds"))
wpop.tots %>%
  filter(stabbr=="NY", year %in% c(2020, 2030, 2040)) %>%
  spread(year, pop)

hhproj <- readRDS("./data/hhpop_by_hgroup.rds")
ht(hhproj)

# compare growth rates for pop vs hhnum by state and age group
hhproj %>%
  gather(variable, value, pop, hhnum) %>%
  spread(year, value) %>%
  mutate(pch2040=`2040` / `2020` * 100 - 100) %>%
  filter(stabbr %in% c("CA", "NH")) %>%
  arrange(stabbr, variable, cexgroup)

cex_expend <- readRDS("./data/cex_expend")
glimpse(cex_expend)
count(cex_expend, vname, item)

taxrules <- read_excel(paste0("./data/", "CES_Taxable_Crosswalk.xlsx"))
taxrules <- taxrules %>%
  filter(str_sub(vname, 1, 1)=="v") %>%
  mutate_at(vars(starts_with("taxable")), list(~naz(.))) %>%
  select(vname, starts_with("tax"), item)
ht(taxrules)
taxrules %>%
  filter(taxable_felix!=0)

taxexp <- cex_expend %>%
  left_join(taxrules %>% select(-item)) %>%
  filter(taxable_felix != 0) %>% # to make it easier to see
  mutate(taxexp=exp_m * taxable_felix,
         avgtaxexp=exp_avg * taxable_felix) %>%
  group_by(agegroup) %>%
  summarise_at(vars(avgtaxexp, taxexp), ~sum(.)) %>% 
  ungroup %>%
  mutate(units=taxexp / avgtaxexp * 1000)
taxexp

# now bring in new numbers of units
hhproj %>%
  filter(stabbr=="US") %>%
  select(stabbr, year, cexgroup, hhnum) %>%
  mutate(hhnum=hhnum / 1000) %>%
  spread(year, hhnum) %>%
  adorn_totals("row")

# make a cex taxbase file
taxbase <- taxexp %>%
  filter(agegroup!="total", agegroup!="a65p") %>%
  mutate(share.base=units / sum(units))
taxbase
taxbase %>% adorn_totals()

# get hh projections by state and agegroup
projshares <- hhproj %>%
  mutate(hhnum=hhnum / 1000) %>%
  group_by(stabbr, year) %>%
  mutate(projshare=hhnum / sum(hhnum)) %>%
  ungroup
projshares %>% filter(stabbr=="US", year==2020) %>% adorn_totals()

#.. compute tax-change measure that reflect only composition change ----
taxchange.comp <- taxbase %>% 
  select(cexgroup=agegroup, avgtaxexp) %>%
  left_join(projshares %>% select(cexgroup, stabbr, year, projshare) %>% spread(year, projshare)) %>%
  select(stabbr, cexgroup, everything()) %>%
  mutate_at(vars(-c(stabbr, cexgroup, avgtaxexp)), list(tax=~avgtaxexp * .)) %>%
  arrange(stabbr, cexgroup)
ht(taxchange.comp)

# state total pch when only composition changes
taxchange.comp %>%
  select(stabbr, contains("_")) %>%
  group_by(stabbr) %>%
  summarise_all(~sum(.)) %>%
  mutate(pch2040=`2040_tax` / `2020_tax` * 100 - 100) %>%
  filter(stabbr %in% globals$case_study_states)

taxchange.comp %>%
  select(stabbr, contains("_")) %>%
  group_by(stabbr) %>%
  summarise_all(~sum(.)) %>%
  mutate(pch2040=`2040_tax` / `2020_tax` * 100 - 100) %>%
  mutate(css=ifelse(stabbr %in% globals$case_study_states, 1, 0),
         stname=getstname(stabbr)) %>%
  arrange(css, stname) %>%
  write_csv("./results/cex_txsales_composition_chg_state_.csv")

# pch by age group when only age composition changes
taxchange.comp %>%
  select(stabbr, cexgroup, contains("tax")) %>%
  mutate(pch2040=`2040_tax` / `2020_tax` * 100 - 100) %>%
  filter(stabbr %in% globals$case_study_states)

#.. compute total tax-change measure that includes BOTH growth and composition change ----
f <- function(x, avgtaxexp) x * avgtaxexp / 1e3

taxchange.tot <- taxbase %>% 
  select(cexgroup=agegroup, avgtaxexp) %>%
  left_join(projshares %>% select(cexgroup, stabbr, year, hhnum)) %>% 
  spread(year, hhnum) %>%
  group_by(stabbr) %>%
  mutate_at(vars(-c(stabbr, cexgroup, avgtaxexp)), list(tax=~avgtaxexp * .)) %>%
  select(stabbr, cexgroup, avgtaxexp, everything())
taxchange.tot %>% filter(stabbr=="US")

# look at pch in total
taxchange.tot %>%
  select(-avgtaxexp) %>%
  select(stabbr, cexgroup, contains("tax")) %>%
  group_by(stabbr) %>%
  summarise_at(vars(-cexgroup), ~sum(.)) %>%
  mutate(pch2040=`2040_tax` / `2020_tax` * 100 - 100) %>%
  mutate(css=ifelse(stabbr %in% globals$case_study_states, 1, 0),
         stname=getstname(stabbr)) %>%
  arrange(css, stname) %>%
  write_csv("./results/cex_txsales_total_chg_state_.csv")
  
head(taxchange.tot)  


#****************************************************************************************************
#                EXAMPLE: Multidplyr - I am not using this but the syntax is useful to have handy ####
#****************************************************************************************************

#.. ONETIME parallel processing to group by hh and get age of hhead hhhead and persons within EACH household ----
library("multidplyr")
library("parallel")
# get hcuts and pcuts for defining hh head and person pop age groups
hcuts <- c(-Inf, agexwalk %>% filter(grouptype=="cex") %>% .[["minage_source"]], 100) %>% unique %>% sort
pcuts <- c(-Inf, agexwalk %>% filter(grouptype=="cooper") %>% .[["minage_source"]], 100) %>% unique %>% sort
hcuts
pcuts

cluster <- create_cluster(cores = 8)
set_default_cluster(cluster)
cluster_copy(cluster, hcuts)
cluster_copy(cluster, pcuts)
cluster_ls(cluster) # Check registered items
# cluster_get(cluster, 'hcuts') 

a <- proc.time()
grouped <- hhhp %>%
  filter(wgtp > 0, np>0) %>%
  # filter(row_number()<=30e3) %>%
  # sample_frac(.05) %>%
  arrange(stabbr, serialno, sporder) %>%
  partition(stabbr, serialno) %>%
  # group_by(serialno) %>%
  mutate(hhhage=first(agep)) %>%
  mutate(hgroup=cut(hhhage, hcuts, right=FALSE),
         pgroup=cut(agep, pcuts, right=FALSE)) %>%
  collect() %>% 
  ungroup
b <- proc.time()
b - a
# saveRDS(grouped, "./data/acs_state_agegroups.rds") # 22 minutes




