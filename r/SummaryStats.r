


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
library("BEAData")

data(package="BEAData")


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
css <- c("CA", "NH", "NY", "OH", "TN", "TX")



#****************************************************************************************************
#                Population aging ####
#****************************************************************************************************
popproj <- readRDS("./data/popproj/wc_popproj.rds")
glimpse(popproj)
count(popproj, popgroup)
count(popproj, year)
unique(popproj$popgroup)


pop2064 <- c("pop20_24", "pop25_29", "pop30_34", "pop35_39", "pop40_44", "pop45_49", "pop50_54", "pop55_59", "pop60_64")
pop65p <- c("pop65_69", "pop70_74", "pop75_79", "pop80_84", "pop85plus")
pop70p <- c("pop70_74", "pop75_79", "pop80_84", "pop85plus")
pop75p <- c("pop75_79", "pop80_84", "pop85plus")

# 65p, 75p, 85p, and changes
df <- popproj %>%
  filter(stabbr %in% c("US", css), sex=="Total") %>%
  mutate(poptot=value * (popgroup=="poptot"),
         pop2064=value * (popgroup %in% pop2064),
         pop65p=value * (popgroup %in% pop65p),
         pop75p=value * (popgroup %in% pop75p),
         pop85p=value * (popgroup=="pop85plus")) %>%
  group_by(stabbr, year) %>%
  summarise_at(vars(poptot, pop2064, pop65p, pop75p, pop85p), funs(sum)) %>%
  mutate_at(vars(pop2064, pop65p, pop75p, pop85p), funs(s=. / poptot * 100)) %>%
  mutate(oa.depratio=pop65p / pop2064 * 100,
         stname=getstname(stabbr)) %>%
  select(stname, everything()) %>%
  ungroup
df  


df %>%
  select(stname, year, ends_with("_s")) %>%
  gather(variable, value, ends_with("_s")) %>%
  spread(year, value) %>%
  mutate(c2030=`2030` - `2020`,
         c2040=`2040` - `2020`) %>%
  select(variable, everything()) %>%
  arrange(variable, stname) %>%
  write_csv("./results/css_c2040.csv")


df %>%
  select(stname, year, oa.depratio) %>%
  gather(variable, value, oa.depratio) %>%
  spread(year, value) %>%
  mutate(c2030=`2030` - `2020`,
         c2040=`2040` - `2020`) %>%
  select(variable, everything()) %>%
  arrange(variable, stname) %>%
  write_csv("./results/css_oadepratio_c2040.csv")


df %>%
  select(stname, year, poptot, pop2064, pop65p, pop75p, pop85p) %>%
  gather(variable, value, poptot, pop2064, pop65p, pop75p, pop85p) %>%
  spread(year, value) %>%
  mutate(pch2030=`2030` / `2020` * 100 - 100,
         pch2040=`2040` / `2020` * 100 - 100) %>%
  select(variable, everything()) %>%
  arrange(variable, stname) %>%
  write_csv("./results/css_pch2040.csv")



#****************************************************************************************************
#                Economy ####
#****************************************************************************************************
# GDP; consumption?
glimpse(sgdp.a_all)
count(sgdp.a_all, component, compname)
count(sgdp.a_all, ind, indclass, indname)
count(sgdp.a_all, ind, indclass, indname) %>% filter(ind>60)

iclass <- c(1, 2, 3, 6, 10, 11, 12, 34, 35, 36, 45, 50, 59, 69, 70, 74, 81, 82)
df <- sgdp.a_all %>%
  filter(component==200, ind %in% iclass, year==2016, stabbr %in% c("US", css))

df %>% filter(stabbr=="US") %>% write_csv("./results/tmp.csv")

itab <- read_csv("ind, iname
1, gdp
2, priv
3, agmin
6, agmin
10, util
11, constr
12, manuf
34, trade
35, trade
36, transp
45, info
50, firel
59, probus
69, educ
70, healthss
74, other
81, other
82, govt")
itab

df2 <- df %>%
  left_join(itab) %>%
  group_by(stabbr, iname) %>%
  summarise(gdp=sum(value))

df2 %>%
  ungroup %>%
  spread(iname, gdp) %>%
  mutate(stname=getstname(stabbr)) %>%
  select(-stabbr) %>%
  select(stname, gdp, everything()) %>%
  mutate_at(vars(-stname, -gdp), funs(. / gdp * 100)) %>%
  write_csv("./results/css_gdpshares.csv")



#****************************************************************************************************
#                Tax structure ####
#****************************************************************************************************
glimpse(sgtax.a)
glimpse(slgfin)
count(slgfin, aggvar)
max(slgfin$year)
d <- slgfin %>% select(aggvar) %>% unique %>% .[[1]] %>% sort
d
df <- slgfin %>% 
  filter(year==2015, level==2, aggvar %in% c("osr", "tottax", "iit", "gst", "selsalestax", "proptax"), stabbr %in% c("US", css)) %>%
  mutate(stname=getstname(stabbr)) %>%
  select(stname, aggvar, value) %>%
  spread(aggvar, value) %>%
  mutate_at(vars(-stname), funs(naz)) %>%
  mutate(nontax=osr - tottax,
         othertax=tottax - gst - iit - selsalestax - proptax) %>%
  select(stname, osr, nontax, tottax, iit, gst, selsalestax, proptax, othertax) %>%
  mutate_at(vars(-stname, -osr), funs(s=. / osr * 100))
df

df %>%
  write_csv("./results/css_revshares.csv")
