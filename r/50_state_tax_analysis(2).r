
# 3/4/2019


#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("ggrepel")

library("BEAData")

library("grid")
library("gridExtra")

library("broom")

#****************************************************************************************************
#                Globals ####
#****************************************************************************************************


#****************************************************************************************************
#                Population projections data ####
#****************************************************************************************************
wpop <- readRDS("./data/popproj/wc_popproj.rds") # Weldon Cooper pop projections
ht(wpop)
count(wpop, stabbr) # 52 includes DC, US, but not PR
count(wpop, sex) # "Total"
count(wpop, year) # 2010, 2020, 2030, 2040
count(wpop, popgroup) # pop00_04, pop05_09, ..., pop80_84, pop85plus, poptot


#****************************************************************************************************
#                Projected working age and elderly pop changes in case study states ####
#****************************************************************************************************
count(wpop, popgroup)
df <- wpop %>% 
  filter(sex=="Total", popgroup!="poptot") %>%
  mutate(grp=case_when(popgroup %in% globals$wc_workage ~ "workage",
                       popgroup %in% globals$wc_pop65p ~ "pop65p",
                       TRUE ~ "other")) %>%
  group_by(stabbr, year, grp) %>%
  summarise(pop=sum(value)) %>%
  spread(year, pop) %>%
  mutate(pch2040=`2040` / `2020` * 100 - 100)
ht(df)
count(df, stabbr)


df %>%
  filter(stabbr %in% c("US", globals$case_study_states), grp!="other") %>%
  arrange(grp, stabbr) %>%
  select(stabbr, grp, pch2040) %>%
  spread(grp, pch2040)

df %>% filter(stabbr=="OH")

# old age dep ratio
df %>% 
  filter(grp!="other") %>%
  select(-pch2040) %>%
  gather(year, pop, -stabbr, -grp) %>%
  spread(grp, pop) %>%
  mutate(depratio=pop65p / workage * 100) %>%
  select(stabbr,year, depratio) %>%
  spread(year, depratio) %>%
  filter(stabbr %in% c("US", globals$case_study_states))
    

#****************************************************************************************************
#                Population projections changes ####
#****************************************************************************************************
df <- wpop %>% 
  filter(sex=="Total", popgroup!="poptot") %>%
  group_by(stabbr, year) %>%
  mutate(poptot=sum(value),
         share=value / poptot) %>%
  filter(popgroup %in% globals$wc_pop65p) %>%
  summarise(share65p=sum(share)) %>%
  mutate(year=paste0("share65p", year)) %>%
  spread(year, share65p)
ht(df)
count(df, stabbr)

df2 <- df %>% mutate(change13=share65p2030 - share65p2010,
                     change23=share65p2030 - share65p2020,
                     change24=share65p2040 - share65p2020)
usvals <- df2 %>% filter(stabbr=="US")


p <- df2 %>% 
  filter(!stabbr %in% c("DC", "US")) %>%
  mutate(stype=ifelse(stabbr %in% globals$case_study_states, "Case study\nstates", "Other states"),
         lsize=ifelse(stype=="Other", 3, 4)) %>%
  ggplot(aes(x=share65p2020, y=change24, label=stabbr)) + 
  theme_bw() +
  # geom_text(colour="blue", size=3, position=position_jitter(width=.004, height=.004)) +
  # geom_text(aes(colour=stype, size=stype), fontface = "bold", position=position_nudge(x=.002, y=.002)) +
  geom_text_repel(aes(colour=stype, size=stype), fontface = "bold", point.padding = NA) +
  #geom_text(aes(colour=stype, size=stype), fontface = "bold") +
  scale_x_continuous(name="Projected percent age 65+ in 2020", labels = scales::percent,
                     breaks=seq(0, .3, .01)) +
  scale_y_continuous(name="Change in percent from 2020 to 2040",
                     labels = scales::percent, 
                     breaks=seq(0, .3, .01),
                     limits=c(0, NA)) +
  scale_colour_manual(values=c("darkred", "blue", "#636363")) +
  scale_size_manual(values=c(3.5, 3.5, 3)) +
  geom_hline(yintercept=usvals$change24) +
  geom_vline(xintercept=usvals$share65p2020) +
  geom_hline(yintercept=0, linetype="dashed", colour="darkblue") +
  ggtitle(label="Projected percent of population age 65 or older in 2020, and change to 2040",
          subtitle="Horizontal and vertical lines show United States average") +
  labs(caption="\nSource: Author's analysis of projections from University of Virginia, Weldon Cooper Center for Public Service, www.coopercenter.org/demographics") +
  theme(plot.title = element_text(size=rel(1.4), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=8)) +
  guides(colour=guide_legend(title=NULL), size=guide_legend(title=NULL))
#p

p2 <- p + annotate("text", label="Younger and aging more slowly", colour="darkgreen", 
                   x=min(df2$share65p2020) + .02, y=0.005) +
  annotate("text", label="Older and aging more quickly", colour="darkgreen", 
           x=usvals$share65p2020 + .03, y=max(df2$change24) - .01)
p2

#ggsave(p, file=paste0("./results/age65p_1030_scatter.png"), width=10, height=6.25, units="in")
ggsave(p2, file=paste0("./results/age65p_2040_scatter_repel2.png"), width=10, height=6.25, units="in")



#****************************************************************************************************
#                SOI ####
#****************************************************************************************************
# D:\Data\bdata_package_sourcedata\soi
data(package="bdata")
glimpse(soiall)
count(soiall, year)
count(soiall, vname)
# retirement income: irapay, txblpension, txblsocsec
count(soiall, incgrp)
# quick check on growth rates

vars <- c("agi", "wages", "irapay", "txblpension", "txblsocsec", "txblinterest", "netcgll", "busprofinc", "odividends")
df <- soiall %>% filter(stabbr=="US", incgrp=="all", vname %in% vars) %>%
  select(year, stabbr, vname, value) %>%
  spread(vname, value) %>%
  mutate(retinc=irapay + txblpension + txblsocsec) %>%
  gather(vname, value, -year, -stabbr) %>%
  group_by(vname) %>%
  arrange(year) %>%
  mutate(pch=value / lag(value) * 100 - 100)
df

# Create a table of US retirement income
df %>% filter(stabbr=="US", year==2014) %>%
  write_csv("./results/agius2014.csv")


df %>% select(-stabbr, -value) %>%
  spread(vname, pch)

df %>% filter(vname %in% c("wages", "retinc")) %>%
  ggplot(aes(year, pch, colour=vname)) +
  geom_line()

df %>% filter(vname %in% c("wages", "retinc")) %>%
  select(-pch) %>%
  spread(vname, value) %>%
  mutate(share=retinc / (retinc + wages) * 100) %>%
  ggplot(aes(year, share)) +
  geom_line()

df <- soiall %>% filter(incgrp=="all", vname %in% vars) %>%
  select(year, stabbr, vname, value) %>%
  spread(vname, value) %>%
  mutate(retinc=irapay + txblpension + txblsocsec,
         share=retinc / (retinc + wages) * 100)

df %>% filter(year %in% c(2004, 2014)) %>%
  select(stabbr, year, share) %>%
  spread(year, share) %>%
  mutate(change=`2014` - `2004`) %>%
  arrange(-change)

df %>% filter(stabbr %in% c("US", "DE", "MI", "OK", "ND")) %>%
  ggplot(aes(year, share, colour=stabbr)) +
  geom_line()


#****************************************************************************************************
#                CPS - get/analyze perdf - person data frame ####
#****************************************************************************************************
perdf <- readRDS("./data/perdf.rds")
count(perdf, stabbr, year) %>% 
  spread(year, n) %>% 
  mutate(diff=`2018` - `2017`, pdiff=diff / `2017` * 100) %>%
  arrange(desc(pdiff)) %>%
  ht

perdf %>% filter(age>=18, age<65) %>% # even here we have half paying no state tax!
  group_by(year) %>%
  do(qtiledf(.$rtax)) 

perdf %>% filter(age>=18, !stabbr %in% globals$nonpit_states) %>% # still half paying no state tax!
  group_by(year) %>%
  do(qtiledf(.$rtax)) 


# D FILESTAT 1 733 (1:6)
# Tax Filer status
#   V 1 .Joint, both <65
#   V 2 .Joint, one <65 & one 65+
#   V 3 .Joint, both 65+
#   V 4 .Head of household
#   V 5 .Single
#   V 6 .Nonfiler

perdf %>% filter(age >= 18, age < 65, !stabbr %in% globals$nonpit_states) %>%
  group_by(filestat) %>%
  do(qtiledf(.$rtax)) 

ibrks <- c(-1e9, 0, 10e3, 25e3, 50e3, 100e3, 1e9)
perdf %>% filter(age >= 18, !stabbr %in% globals$nonpit_states) %>%
  mutate(ygroup=cut(rincome, ibrks)) %>%
  group_by(ygroup) %>%
  do(qtiledf(.$rtax))

# get weighted mean rtax by age groups
abrks <- c(-100, 0, 18, 25, 35, 45, 65, 1e9)

# counts
perdf %>% filter(age > 16) %>%
  mutate(agegrp=cut(age, breaks=abrks)) %>%
  group_by(stabbr, agegrp) %>%
  summarise(n=n()) %>%
  spread(agegrp, n)

tmp <- perdf %>% filter(age > 16) %>%
  mutate(agegrp=cut(age, breaks=abrks)) %>%
  group_by(stabbr, agegrp) %>%
  summarise(n=sum(marsupwt) / 1e6, 
            rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE))
tmp

tmp2 <- tmp %>% select(-n) %>%
  group_by(stabbr) %>%
  spread(agegrp, rtax) %>%
  mutate(ratio=`(65,1e+09]` / `(45,65]`,
         diff=`(65,1e+09]` - `(45,65]`)

tmp2 %>% arrange(ratio)
tmp2 %>% arrange(diff)

# Now do age-tax profiles (regressions)
mod2 <- perdf %>% filter(age >= 18, stabbr=="MS") %>%
  mutate(age2=age^2, age3=age^3, rincome2=rincome^2) %>%
  lm(rtax ~ age + age2 + age3 + rincome + rincome2, data=.)
summary(mod2)

mod <- perdf %>% filter(age>=18, stabbr=="MS") %>%
  mutate(age2=age^2, age3=age^3) %>%
  lm(rtax ~ age + age2 + age3, data=.)
summary(mod)

tibble(age=18:75) %>%
  mutate(rtax=coef(mod)["age"]*age +
           coef(mod)["age2"]*age^2 +
           coef(mod)["age3"]*age^3,
         rtax=rtax - rtax[age==18]) %>%
  ggplot(aes(age, rtax)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 100, 5)) +
  scale_y_continuous(breaks=seq(-1000, 2000, 100))

perdf %>% filter(stabbr=="MD", age >= 18) %>%
  group_by(age) %>%
  summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE)) %>%
  ggplot(aes(age, rtax)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 100, 5)) +
  scale_y_continuous(breaks=seq(-1000, 10000, 100))


st <- "MD"
byage <- perdf %>% filter(age>=18, stabbr==st) %>%
  group_by(age) %>%
  summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE),
            rincome=weighted.mean(rincome, marsupwt, na.rm=TRUE))
byage %>%
  ggplot(aes(age, rincome)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 100, 5)) +
  scale_y_continuous(breaks=seq(-10e3, 500e3, 10e3)) +
  ggtitle(st)


st <- "CA"
mod <- perdf %>% filter(age>=18, stabbr==st) %>%
  mutate(age2=age^2, age3=age^3, age4=age^4) %>%
  lm(rtax ~ age + age2 + age3 + age4, data=.)
summary(mod)

tibble(age=18:75) %>%
  mutate(rtax.est=coef(mod)["(Intercept)"] +
           coef(mod)["age"]*age +
           coef(mod)["age2"]*age^2 +
           coef(mod)["age3"]*age^3 +
           coef(mod)["age4"]*age^4) %>%
  left_join(byage) %>%
  gather(variable, value, -age) %>%
  ggplot(aes(age, value, colour=variable)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0, 100, 5)) +
  scale_y_continuous(breaks=seq(-1000, 10000, 100)) +
  ggtitle(st)


#****************************************************************************************************
#                Construct CPS age-tax profiles ####
#****************************************************************************************************
# Now do age-tax profiles (regressions) for real ####
# perdf
# perdf2 <- perdf %>% filter(!(stabbr=="MD" & year==2016)) # remove bad data
# saveRDS(perdf2, "./data/perdf2.rds")
# perdf2 <- readRDS("./data/perdf2.rds")

#.. stage -- state age analysis build the data ----
stage.st <- perdf %>% 
  filter(age>=18) %>%
  group_by(stabbr, age) %>%
  summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE),
            rincome=weighted.mean(rincome, marsupwt, na.rm=TRUE))

stage.us <- perdf %>% 
  filter(age>=18) %>%
  group_by(age) %>%
  summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE),
            rincome=weighted.mean(rincome, marsupwt, na.rm=TRUE)) %>%
  mutate(stabbr="US")

stage <- bind_rows(stage.st, stage.us)

st <- "CA"

#.. build the model ----
f <- function(df){
  mod <- df %>% filter(age >= 18) %>%
    mutate(age2=age^2, age3=age^3, age4=age^4) %>%
    lm(rtax ~ age + age2 + age3 + age4, data=.)
  c.df <- as_tibble(t(coef(mod)))
  vnames <- names(c.df)
  vnames[1] <- "intercept"
  vnames <- paste0("tx.", vnames)
  c.df <- c.df %>% setNames(vnames)
  return(c.df)
}

tx.us <- perdf %>% do(f(.)) %>% mutate(stabbr="US")
tx.df <- perdf %>% group_by(stabbr) %>%
  do(f(.)) %>%
  bind_rows(tx.us)

g <- function(df){
  mod <- df %>% filter(age >= 18) %>%
    mutate(age2=age^2, age3=age^3, age4=age^4) %>%
    lm(rincome ~ age + age2 + age3 + age4, data=.)
  c.df <- as_tibble(t(coef(mod)))
  vnames <- names(c.df)
  vnames[1] <- "intercept"
  vnames <- paste0("inc.", vnames)
  c.df <- c.df %>% setNames(vnames)
  return(c.df)
}

inc.us <- perdf %>% do(g(.)) %>% mutate(stabbr="US")
inc.df <- perdf %>% group_by(stabbr) %>%
  do(g(.)) %>%
  bind_rows(inc.us)


st2 <- stage %>% left_join(tx.df) %>%
  left_join(inc.df) %>%
  mutate(rtax.est=tx.intercept + 
           tx.age * age +
           tx.age2 * age^2 +
           tx.age3 * age^3 +
           tx.age4 * age^4,
         rincome.est=inc.intercept + 
           inc.age * age +
           inc.age2 * age^2 +
           inc.age3 * age^3 +
           inc.age4 * age^4) %>%
  select(stabbr, age, rtax, rincome, rtax.est, rincome.est) %>%
  gather(variable, value, -stabbr, -age)

#.. look at the profiles ----

st2

#.. set up graph info ----
cps_src <- "Source: Annual Social and Economic (ASEC) supplement to the Current Population Survey, pooled 2017 and 2018"
note <- "Smoothed values estimated as a 4th degree polynomial of age"
capt <- paste0(cps_src, "\n", note)

#.. graphs of income and tax by age, US, 2 panels ----
p1 <- st2 %>% filter(stabbr=="US", str_sub(variable, 1, 4)=="rinc") %>%
  mutate(varf=factor(variable, levels=c("rincome", "rincome.est"), labels=c("Actual", "Smoothed"))) %>%
  ggplot(aes(x=age, y=value, colour=varf)) +
  theme_bw() +
  geom_line(size=rel(1.3)) +
  geom_point() +
  scale_x_continuous(name="Age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="Income", breaks=seq(0, 200e3, 10e3), limits=c(0, NA), labels=scales::dollar) +
  scale_colour_manual(values=c("darkgreen", "blue")) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle("Average income by age in the United States", subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.5), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p1

p2 <- st2 %>% filter(stabbr=="US", str_sub(variable, 1, 4)=="rtax") %>%
  mutate(varf=factor(variable, levels=c("rtax", "rtax.est"), labels=c("Actual", "Smoothed"))) %>%
  ggplot(aes(x=age, y=value, colour=varf)) +
  theme_bw() +
  geom_line(size=rel(1.3)) +
  geom_point() +
  scale_x_continuous(name="Age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="State income tax", breaks=seq(0, 50e3, 200), limits=c(0, NA), labels=scales::dollar) +
  scale_colour_manual(values=c("darkgreen", "blue")) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle("Average state income tax by age in the United States", subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.5), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p2

ml <- marrangeGrob(list(p1, p2), nrow=2, ncol=1, top=NULL)
ml
ggsave("./results/income_and_tax_byage.png", ml, width=8, height=11, units="in")


#.. graphs of income and tax by age, case study states ####
sts <- c(globals$case_study_states, "US"); clrs <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c', "black")
sts <- c(globals$case_study_states); clrs <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c')
p1 <- st2 %>% filter(stabbr %in% sts, variable=="rincome.est") %>%
  ungroup %>%
  mutate(stabbr=factor(stabbr, levels=sts)) %>%
  ggplot(aes(x=age, y=value, colour=stabbr)) +
  theme_bw() +
  geom_line(size=rel(1.3)) +
  scale_x_continuous(name="Age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="Income", breaks=seq(0, 200e3, 10e3), limits=c(0, NA), labels=scales::dollar) +
  scale_colour_manual(values=clrs) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle("Average income by age in case study states", subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p1

p2 <- st2 %>% filter(stabbr %in% sts, variable=="rtax.est") %>%
  ungroup %>%
  mutate(stabbr=factor(stabbr, levels=sts)) %>%
  ggplot(aes(x=age, y=value, colour=stabbr)) +
  theme_bw() +
  geom_line(size=rel(1.3)) +
  scale_x_continuous(name="Age", breaks=seq(0, 100, 5)) +
  scale_y_continuous(name="State income tax", breaks=seq(0, 50e3, 200), limits=c(0, NA), labels=scales::dollar) +
  scale_colour_manual(values=clrs) +
  guides(colour=guide_legend(title=NULL)) +
  ggtitle("Average state income tax by age in case study states", subtitle="2017 dollars") +
  labs(caption=capt) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=rel(.8)))
p2

ml <- marrangeGrob(list(p1, p2), nrow=2, ncol=1, top=NULL)
ml
# use one or the other of these
ggsave("./results/income_and_tax_byage_css_xus.png", ml, width=8, height=11, units="in")
ggsave("./results/income_and_tax_byage_css.png", ml, width=8, height=11, units="in")


#.. DON'T NEED graphs of income and tax by age indexed to age 50, individual case study states ####
# sts <- c(globals$case_study_states, "US")
# st <- sts[1]
# p1 <- st2 %>% 
#   filter(stabbr %in% sts, variable %in% c("rincome.est", "rtax.est")) %>%
#   mutate(variable=factor(variable, levels=c("rincome.est", "rtax.est"), labels=c("Income", "State income tax"))) %>%
#   group_by(stabbr, variable) %>%
#   mutate(ivalue=value / value[age==50] * 100) %>%
#   ungroup %>%
#   ggplot(aes(x=age, y=ivalue, colour=variable)) +
#   theme_bw() +
#   geom_line(size=rel(1.3)) +
#   geom_hline(yintercept = 100, linetype="dashed") +
#   scale_x_continuous(name="Age", breaks=seq(0, 100, 5)) +
#   scale_y_continuous(name="Indexed value (age 50=100)", breaks=seq(0, 100, 10), limits=c(0, NA)) +
#   scale_colour_manual(values=c("blue", "darkgreen")) +
#   guides(colour=guide_legend(title=NULL)) +
#   ggtitle("Average income and state income tax by age", subtitle="Indexed to values at age 50") +
#   labs(caption=capt) +
#   theme(plot.title = element_text(size=rel(1.3), face="bold")) +
#   theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
#   theme(axis.title = element_text(face="bold", size=rel(1))) +
#   theme(axis.text = element_text(face="bold", size=rel(1))) +
#   theme(plot.caption = element_text(hjust=0, size=rel(.8)))
# p1 + facet_wrap(~stabbr)
# ggsave("./results/income_and_tax_byage_indexed.png", p1, width=8, height=11, units="in")


#.. DON'T NEED graph of pch in avg tax vs avg income, from 2nd-top to top income range ----
# abrks <- c(-100, 0, 18, 25, 35, 45, 65, 1e9)
# pchdf <- perdf %>% filter(age>=18, !stabbr %in% globals$nonpit_states) %>%
#   mutate(agegrp=cut(age, breaks=abrks)) %>%
#   group_by(stabbr, agegrp) %>%
#   summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE),
#             rincome=weighted.mean(rincome, marsupwt, na.rm=TRUE)) %>%
#   filter(agegrp %in% c("(45,65]", "(65,1e+09]")) %>%
#   gather(variable, value, rtax, rincome) %>%
#   spread(agegrp, value) %>%
#   mutate(pch=`(65,1e+09]` / `(45,65]` - 1) %>%
#   select(stabbr, variable, pch) %>%
#   spread(variable, pch) %>%
#   mutate(diff=rtax - rincome)
# 
# uspch <- pchdf %>% ungroup %>%
#   summarise(rincome=median(rincome), rtax=median(rtax), diff=median(diff))
# 
# 
# p <- pchdf %>% mutate(stype=ifelse(stabbr %in% globals$case_study_states, "Case study\nstates", "Other states"),
#                       lsize=ifelse(stype=="Other states", 3, 4)) %>%
#   ggplot(aes(x=rincome, y=rtax, label=stabbr)) + 
#   theme_bw() +
#   # geom_text(colour="blue", size=3, position=position_jitter(width=.004, height=.004)) +
#   # geom_text(aes(colour=stype, size=stype), fontface = "bold", position=position_nudge(x=.002, y=.002)) +
#   geom_text_repel(aes(colour=stype, size=stype), fontface = "bold", point.padding = NA) +
#   #geom_text(aes(colour=stype, size=stype), fontface = "bold") +
#   scale_x_continuous(name="Percent change in income (greater declines are to left)", labels = scales::percent,
#                      breaks=seq(-1, 1, .05)) +
#   scale_y_continuous(name="Percent change in state income tax (greater declines are lower)",
#                      labels = scales::percent, 
#                      breaks=seq(-1, 1, .05)) +
#   scale_colour_manual(values=c("darkred", "blue", "#636363")) +
#   scale_size_manual(values=c(3.5, 3.5, 3)) +
#   geom_hline(yintercept=uspch$rtax) +
#   geom_vline(xintercept=uspch$rincome) +
#   ggtitle(label="Decline in income and in state income tax, age 65+ cohort relative to age 45-64 cohort",
#           subtitle="Horizontal and vertical lines show United States medians") +
#   labs(caption="\nSource: Annual Social and Economic (ASEC) supplement to the Current Population Survey, pooled 2017 and 2018") +
#   theme(plot.title = element_text(size=rel(1.3), face="bold")) +
#   theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
#   theme(axis.title = element_text(face="bold", size=rel(1))) +
#   theme(axis.text = element_text(face="bold", size=rel(1))) +
#   theme(plot.caption = element_text(hjust=0, size=8)) +
#   guides(colour=guide_legend(title=NULL), size=guide_legend(title=NULL))
# p
# ggsave(p, file=paste0("./results/taxpch_vs_incpch_65vs45to64_scatter.png"), width=10, height=6.25, units="in")


#.. DON'T NEED graph of tax ratios ----
# get average tax by pop group
abrks <- c(-100, 0, 18, 25, 35, 45, 65, 1e9)
taxratio <- perdf %>% filter(age>=18, !stabbr %in% globals$nonpit_states) %>%
  mutate(agegrp=cut(age, breaks=abrks)) %>%
  group_by(stabbr, agegrp) %>%
  summarise(rtax=weighted.mean(rtax, marsupwt, na.rm=TRUE)) %>%
  spread(agegrp, rtax) %>%
  mutate(diff=`(65,1e+09]` - `(45,65]`,
         pch=diff / `(45,65]`) %>%
  arrange(pch)

ustr <- taxratio %>%
  ungroup %>%
  summarise(diff=median(diff), pch=median(pch))

p <- taxratio %>% 
  filter(!stabbr %in% globals$nonpit_states) %>%
  mutate(stype=ifelse(stabbr %in% globals$case_study_states, "Case study\nstates", "Other states"),
         lsize=ifelse(stype=="Other states", 3, 4)) %>%
  ggplot(aes(x=pch, y=diff, label=stabbr)) + 
  theme_bw() +
  # geom_text(colour="blue", size=3, position=position_jitter(width=.004, height=.004)) +
  # geom_text(aes(colour=stype, size=stype), fontface = "bold", position=position_nudge(x=.002, y=.002)) +
  geom_text_repel(aes(colour=stype, size=stype), fontface = "bold", point.padding = NA) +
  #geom_text(aes(colour=stype, size=stype), fontface = "bold") +
  scale_x_continuous(name="Percent change", labels = scales::percent,
                     breaks=seq(-1, 1, .05)) +
  scale_y_continuous(name="Dollar difference, 65+ minus 45-64 age group",
                     labels = scales::dollar, 
                     breaks=seq(-3e3, 1e3, 100)) +
  scale_colour_manual(values=c("darkred", "blue", "#636363")) +
  scale_size_manual(values=c(3.5, 3.5, 3)) +
  geom_hline(yintercept=ustr$diff) +
  geom_vline(xintercept=ustr$pch) +
  # geom_hline(yintercept=0, linetype="dashed", colour="darkblue") +
  ggtitle(label="Change in tax per-capita for 65+ age group relative to 45-64 age group",
          subtitle="Horizontal and vertical lines show United States medians") +
  labs(caption="\nSource: Annual Social and Economic (ASEC) supplement to the Current Population Survey, pooled 2017 and 2018") +
  theme(plot.title = element_text(size=rel(1.5), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=8)) +
  guides(colour=guide_legend(title=NULL), size=guide_legend(title=NULL))
p
ggsave(p, file=paste0("./results/age65ptax_vs45to64_scatter.png"), width=10, height=6.25, units="in")



#****************************************************************************************************
#                Now do Watkins-like calculation of potential revenue loss ####
#****************************************************************************************************
wpop <- readRDS("./data/popproj/wc_popproj.rds") # Weldon Cooper pop projections
glimpse(wpop)
count(wpop, popgroup) # get age cuts needed
popgroup <- count(wpop, popgroup) %>% 
  filter(popgroup!="poptot")

unique(wpop$popgroup)

# make a data frame to link popgroups on
agebrks <- c(-1, seq(4, 84, 5), 1e9)
agegroups <- tibble(age=1:100, agebrk=cut(age, agebrks)) %>%
  group_by(agebrk) %>%
  summarise(n=n()) %>%
  mutate(popgroup=popgroup$popgroup) %>%
  select(-n)
agegroups # good, we can use this for linking


# get the tax data from CPS AND CUT THE WEIGHT IN HALF TO REFLECT 2 years !!!! ----
perdf <- readRDS("./data/perdf.rds") %>%
  mutate(avgweight=marsupwt / 2)
glimpse(perdf)
# perdf has one rec per person in 2017 and one per 2018; rtax is real state tax (2017 dollars)
# it has ~180k obs per year; marsupwt is the weight

# prepare cps data for merging against pop projections data
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

# prepare the Weldon Cooper popshares data - for each state and income group, get share of state pop different years
wpop2 <- wpop %>% 
  filter(sex=="Total", popgroup!="poptot") %>%
  group_by(year, stabbr) %>%
  mutate(totproj=sum(value),
         popshare=value / totproj,
         shareyear=paste0("share", year)) %>%
  ungroup %>%
  select(stabbr, popgroup, shareyear, popshare) %>%
  spread(shareyear, popshare)

wpop2 %>% group_by(stabbr) %>% summarise_at(vars(starts_with("share")), sum) # make sure shares add to 1


# put the shares on the tax data 
ptaxshares <- spop.age %>% 
  group_by(stabbr) %>%
  mutate(sharecps=wtdn / sum(wtdn)) %>%
  ungroup %>%
  left_join(wpop2, by=c("stabbr", "popgroup")) %>%
  mutate(taxcps=rtax * wtdn,
         tax2010=rtax * totpop * share2010,
         tax2020=rtax * totpop * share2020,
         tax2030=rtax * totpop * share2030,
         tax2040=rtax * totpop * share2040)

#.. quick check compare implied pit in the cps data vs cps per census years don't match quite correctly of course ----
sgcomp <- ptaxshares %>%
  group_by(stabbr) %>%
  summarise(taxcps=sum(taxcps) / 1e6) %>%
  left_join(slgfin %>% filter(level==2, aggvar=="iit", year==2016) %>% select(stabbr, taxcen=value) %>% mutate(taxcen=taxcen / 1000)) %>%
  mutate(pdiff=taxcps / taxcen * 100 - 100,
         level="sg")
slgcomp <- ptaxshares %>%
  group_by(stabbr) %>%
  summarise(taxcps=sum(taxcps) / 1e6) %>%
  left_join(slgfin %>% filter(level==1, aggvar=="iit", year==2016) %>% select(stabbr, taxcen=value) %>% mutate(taxcen=taxcen / 1000)) %>%
  mutate(pdiff=taxcps / taxcen * 100 - 100,
         level="slg")

taxcomp <- bind_rows(sgcomp, slgcomp)

taxcomp %>% arrange(-abs(pdiff))

taxcomp %>% filter(stabbr=="NY")
taxcomp %>% filter(stabbr %in% globals$case_study_states) %>% arrange(stabbr, level)

#.. end check ----

ptaxshares %>%
  filter(stabbr=="NY") %>%
  select(-agebrk, -totpop) %>%
  select(stabbr, popgroup, everything()) %>%
  mutate_at(vars(starts_with("tax")), ~ . / 1e6) %>%
  janitor::adorn_totals() %>%
  mutate(pch=tax2040 / tax2020 * 100 - 100) %>%
  kable(digits=c(rep(0, 4), rep(3, 5), rep(0, 5), 1), format.args=list(big.mark=","))

write_csv(ptaxshares, "./results/ptaxshares.csv") # so I can calc by hand

#.. compute total state income tax after credites in $ millions at different pop shares and change ----
totchange <- ptaxshares %>% 
  group_by(stabbr) %>%
  summarise_at(vars(tax2010, tax2020, tax2030, tax2040), ~(sum(.) / 1e6)) %>%
  mutate(diff.tot=tax2040 - tax2020,
         pch.tot=tax2040 / tax2020 * 100 - 100)

# alternative approach - per capita tax, and change - should yield the same pch
percapchange <- ptaxshares %>% 
  group_by(stabbr) %>%
  summarise(rtax2010=weighted.mean(rtax, share2010, na.rm=TRUE),
            rtax2020=weighted.mean(rtax, share2020, na.rm=TRUE),
            rtax2030=weighted.mean(rtax, share2030, na.rm=TRUE),
            rtax2040=weighted.mean(rtax, share2040, na.rm=TRUE)) %>%
  mutate(diff.pc=rtax2040 - rtax2020,
         pch.pc=rtax2040 / rtax2020 * 100 - 100)


taxdiff <- totchange %>% 
  left_join(percapchange) %>%
  arrange(diff.pc, pch.pc)

taxdiff

write_csv(taxdiff, "./results/pitchanges.csv")
saveRDS(taxdiff, "./results/pitchanges.rds")

taxdiff <- readRDS("./results/pitchanges.rds")
pdata <- taxdiff %>% filter(!stabbr %in% c("DC", "US", globals$nonpit_states)) %>% mutate(pch.pc=pch.pc / 100)
usvals <- pdata %>% summarise_at(vars(pch.pc, diff.pc), ~median(., na.rm=TRUE))

l1 <- "Source: Author's analysis of data from Current Population Survey and from University of Virginia, Weldon Cooper Center for Public Service"
l2 <- "Notes: (1) States without broad-based income taxes excluded. (2) Analysis is similar to Felix & Watkins 2013, with updated information."
srcnote <- paste0("\n", l1, "\n", l2)

p <- pdata %>%
  mutate(stype=ifelse(stabbr %in% globals$case_study_states, "Case study\nstates", "Other states"),
         lsize=ifelse(stype=="Other", 3, 4)) %>%
  ggplot(aes(x=pch.pc, y=diff.pc, label=stabbr)) + 
  theme_bw() +
  # geom_text_repel(aes(colour=stype, size=stype), fontface = "bold") +
  geom_text(aes(colour=stype, size=stype), fontface = "bold") +
  scale_x_continuous(name="Percent change in per-capita income tax",
                     labels = scales::percent,
                     breaks=seq(-.5, .5, .01)) +
  scale_y_continuous(name="Dollar change in per-capita income tax",
                     labels = scales::dollar, 
                     breaks=seq(-100, 100, 20)) +
  scale_colour_manual(values=c("darkred", "blue", "#636363")) +
  scale_size_manual(values=c(3.5, 3.5, 3)) +
  geom_hline(yintercept=usvals$diff.pc) +
  geom_vline(xintercept=usvals$pch.pc) +
  geom_hline(yintercept=0, linetype="dashed", colour="darkblue") +
  ggtitle(label="Impact on per-capita income tax of moving from projected 2020 to 2040 age distribution (2017 dollars)",
          subtitle="Horizontal and vertical lines show medians for income-tax states") +
  labs(caption=srcnote) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(plot.subtitle = element_text(size=rel(1), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=8)) +
  guides(colour=guide_legend(title=NULL), size=guide_legend(title=NULL))
p
ggsave(p, file=paste0("./results/pit_impact_scatter.png"), width=10, height=6.25, units="in")

taxdiff %>% select(stabbr, diff.pc, pch.pc) %>%
  arrange(pch.pc)

ptaxshares %>% select(stabbr, popgroup, starts_with("share")) %>%
  filter(popgroup %in% pop65p) %>%
  group_by(stabbr) %>%
  summarise_at(vars(starts_with("share")), funs(sum)) %>%
  mutate(diff=share2040 - share2020)

#.. get pit change as % of own-source revenue ----
totchange %>% arrange(desc(abs(pch.tot)))
# count(slgfin, aggvar)
osrpct <- slgfin %>%
  filter(year==2016, level==1, aggvar %in% c("iit", "osr")) %>%
  mutate(value=value / 1e3) %>% # put in $ millions
  spread(aggvar, value) %>%
  select(stabbr, iit, osr) %>%
  left_join(totchange %>% select(stabbr, pch.iit=pch.tot)) %>%
  mutate(iitchange=iit * pch.iit / 100,
         osrpct=iitchange / osr * 100)
osrpct %>% arrange(desc(abs(osrpct)))


#****************************************************************************************************
#                Consumer expenditure survey ####
#****************************************************************************************************
# prepare the popshares data
popproj <- readRDS("./data/popproj.rds")
glimpse(popproj)
count(popproj, popgroup) # get age cuts needed

popgroup <- count(popproj, popgroup) %>% 
  filter(popgroup!="pop")

# prepare the popshares data
pop2 <- popproj %>% 
  filter(sex=="Total", popgroup!="pop") %>%
  group_by(year, stabbr) %>%
  mutate(totproj=sum(value),
         popshare=value / totproj,
         shareyear=paste0("share", year)) %>%
  ungroup %>%
  select(stabbr, popgroup, shareyear, popshare) %>%
  spread(shareyear, popshare)

# get shares for each state, group, and year (cols)
pop2 <- popproj %>% 
  filter(sex=="Total", popgroup!="pop") %>%
  group_by(year, stabbr) %>%
  mutate(totproj=sum(value), popshare=value / totproj,
         shareyear=paste0("share", year)) %>%
  ungroup %>%
  select(stabbr, popgroup, shareyear, popshare) %>%
  spread(shareyear, popshare)
# check
pop2 %>% group_by(stabbr) %>% summarise_at(vars(starts_with("share")), sum) # make sure they sum to 1 

# calculate taxable consumption as % of income at each age group
age <- c(21.4, 29.6, 39.5, 49.6, 59.3,	68.9, 81.6) # avg age of reference person per CEX
txpct <- c(51.7, 41.1, 38.4, 34.7, 38.4, 46.5, 48.9) # txbl consumption as % of income per my analysis of CEX
agepct <- tibble(age, txpct)

# get coefs that allow us to estimate txpct at any age
mod <- agepct %>% mutate(age2=age*age, age3=age2*age) %>%
  lm(txpct ~ age + age2 + age3, data=.)
summary(mod)

# estimate txpct of income at any age
agespend <- tibble(age=18:85, 
                   txpct=coef(mod)["(Intercept)"] +
                     coef(mod)["age"] * age +
                     coef(mod)["age2"] * age^2 +
                     coef(mod)["age3"] * age^3) %>%
  mutate(type="est") %>%
  bind_rows(agepct %>% mutate(type="data"))
# check that it looks plausible
agespend %>% ggplot(aes(age, txpct, colour=type)) +
  geom_line()

# make a data frame to link popgroups on proj file with pop on CPS
agebrks <- c(-1, seq(4, 84, 5), 1e9)
agegroups <- tibble(age=1:100, agebrk=cut(age, agebrks)) %>%
  group_by(agebrk) %>%
  summarise(n=n()) %>%
  mutate(popgroup=popgroup$popgroup) %>%
  select(-n)
agegroups # good, we can use this for linking


# calc txbl consumption for each person in pooled CPS
perdf2 <- readRDS("./data/perdf2.rds")
glimpse(perdf2)

txc <- perdf2 %>% 
  left_join(agespend %>% filter(type=="est") %>%
              select(-type)) %>%
  mutate(txblsales=rincome * txpct / 100,
         agebrk=cut(age, agebrks)) %>%
  left_join(agegroups) %>%
  group_by(stabbr, popgroup) %>%
  summarise(wtdn=sum(marsupwt), 
            rincome=weighted.mean(rincome, marsupwt, na.rm=TRUE),
            txblsales=weighted.mean(txblsales, marsupwt, na.rm=TRUE)) %>%
  ungroup

txc %>% filter(stabbr=="NY") %>%
  select(popgroup, rincome, txblsales) %>%
  gather(variable, value, -popgroup) %>%
  # must use group=variable because popgroup is a factor!
  ggplot(aes(popgroup, value, colour=variable, group=variable)) + 
  geom_point() + 
  geom_line() +
  theme(axis.text.x=element_text(angle=45,hjust=.9,vjust=0.9))


# now get state averages under different shares
txcshares <- txc %>% left_join(pop2) %>%
  mutate(totpop=sum(wtdn)) %>%
  mutate(txc2010=txblsales * totpop * share2010,
         txc2020=txblsales * totpop * share2020,
         txc2030=txblsales * totpop * share2030,
         txc2040=txblsales * totpop * share2040)
write_csv(txcshares, "./results/txcshares.csv") # so I can calc by hand

# compute total tax, and change
txcchange <- txcshares %>% group_by(stabbr) %>%
  summarise_at(vars(txc2010, txc2020, txc2030, txc2040), funs(sum(., na.rm=TRUE) / 1e6)) %>%
  mutate(diff.tot=txc2030 - txc2010,
         pch.tot=txc2030 / txc2010 * 100 - 100)
txcchange


# put the shares on the tax data 
ptaxshares <- p4 %>% left_join(pop2) %>%
  mutate(tax2010=rtax * totpop * share2010,
         tax2020=rtax * totpop * share2020,
         tax2030=rtax * totpop * share2030,
         tax2040=rtax * totpop * share2040)
write_csv(ptaxshares, "./results/ptaxshares.csv") # so I can calc by hand

# compute total tax, and change
totchange <- ptaxshares %>% group_by(stabbr) %>%
  summarise_at(vars(tax2010, tax2020, tax2030, tax2040), funs(sum(.) / 1e6)) %>%
  mutate(diff.tot=tax2030 - tax2010,
         pch.tot=tax2030 / tax2010 * 100 - 100)



#****************************************************************************************************
#                Consumer expenditure survey - PLAN B ####
#****************************************************************************************************
popproj <- readRDS("./data/popproj.rds")

# CEX 2015 age groupings
txc.age <- read_csv("agegrp.cex, ageref.cex, incpretax, expend, consxfin, txblexp
00to24, 21.4, 31606, 32797, 29836, 16330
25to34, 29.6, 64472, 52062, 45182, 26480
35to44, 39.5, 84938, 65334, 55954, 32641
45to54, 49.6, 95248, 69753, 57578, 33047
55to64, 59.3, 75262, 58781, 49051, 28889
65to74, 68.9, 54067, 49477, 43518, 25167
75+, 81.6, 36408, 38123, 34392, 17816")
txc.age

txc.age <- txc.age %>% mutate(agelabs=agegrp.cex %>% str_replace_all("to", " to "),
                              agelabs=agelabs %>% str_replace("00 to", "Up to"))

pdata <- txc.age %>% select(-consxfin) %>%
  gather(variable, value, -agegrp.cex, -ageref.cex, -agelabs) %>%
  mutate(varf=factor(variable, 
                     levels=c("incpretax", "expend", "txblexp"),
                     labels=c("Income before tax", "Total expenditures", "Commonly taxable\nexpenditures")))
p <- pdata %>%
  ggplot(aes(ageref.cex, value, colour=varf)) +
  geom_line(size=rel(1.3)) +
  geom_point(size=rel(1.2)) +
  scale_y_continuous(name=NULL, breaks=seq(0, 100e3, 10e3), limits=c(0, NA), labels=scales::dollar) +
  scale_x_continuous(name="Age of householder", breaks=txc.age$ageref.cex, labels=txc.age$agelabs) +
  theme_bw() +
  scale_colour_manual(values=c("red", "blue", "darkgreen")) +
  theme(plot.title = element_text(size=rel(2), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1)))

p1 <- p +
  ggtitle("Income and expenditures by householder age") +
  guides(color = guide_legend(title=NULL, nrow = 3)) +
  labs(caption="\nSource: U.S. Bureau of the Census, Consumer Expenditure Survey 2015, Table 1300 plus author's calculations") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'right',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines')) # put space between legend lines of text
p1
ggsave(p1, file=paste0("./results/expend_hhage.png"), width=10, height=6.25, units="in")

p2 <- p +
  ggtitle("Household income and expenditures\nby householder age") +
  guides(color = guide_legend(title=NULL, nrow = 1)) +
  labs(caption="\nSource: U.S. Bureau of the Census, Consumer Expenditure Survey 2015\nTable 1300 plus author's calculations") +
  theme(plot.caption = element_text(hjust=0, size=rel(.8))) +
  theme(legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key = element_rect(size = 5),
        legend.key.size = unit(1, 'lines')) # put space between legend lines of text
p2
ggsave(p2, file=paste0("./results/expend_hhage_alt.png"), width=7, height=8, units="in")

popgroup

# CEX 2create a linking file ####
pop2 <- popproj %>% 
  filter(sex=="Total", popgroup!="pop") %>%
  group_by(year, stabbr) %>%
  mutate(totproj=sum(value), popshare=value / totproj,
         shareyear=paste0("share", year)) %>%
  ungroup %>%
  select(stabbr, popgroup, shareyear, popshare) %>%
  spread(shareyear, popshare)


popgroup <- count(popproj, popgroup) %>% 
  filter(popgroup!="pop")

popgroup.cex <- popgroup %>% mutate(c2=str_sub(popgroup, 1, 2), 
                                    agegrp.cex=ifelse(c2 < 25, "00to24", NA),
                                    agegrp.cex=ifelse(c2 %in% 25:34, "25to34", agegrp.cex),
                                    agegrp.cex=ifelse(c2 %in% 35:44, "35to44", agegrp.cex),
                                    agegrp.cex=ifelse(c2 %in% 45:54, "45to54", agegrp.cex),
                                    agegrp.cex=ifelse(c2 %in% 55:64, "55to64", agegrp.cex),
                                    agegrp.cex=ifelse(c2 %in% 65:74, "65to74", agegrp.cex),
                                    agegrp.cex=ifelse(c2 >= 75, "75+", agegrp.cex)) %>%
  select(-c2)
popgroup.cex


# get total population of each state in base year
perdf2 <- readRDS("./data/perdf2.rds")
totpop2015 <- perdf2 %>% filter(year==2015) %>%
  group_by(stabbr) %>%
  summarise(pop=sum(marsupwt, na.rm=TRUE))

# collapse pop proj shares by CEX age groups
cex_change <- pop2 %>%
  filter(stabbr!="US") %>%
  left_join(popgroup.cex) %>%
  group_by(stabbr, agegrp.cex) %>%
  summarise_at(vars(starts_with("share")), funs(sum)) %>%
  left_join(totpop2015) %>%
  left_join(txc.age) %>%
  mutate(txc2010=share2010 * pop * txblexp / 1e6,
         txc2030=share2030 * pop * txblexp / 1e6) %>%
  group_by(stabbr) %>%
  summarise_at(vars(txc2010, txc2030), funs(sum)) %>%
  mutate(diff=txc2030 - txc2010,
         pch=diff / txc2010 * 100) %>%
  left_join(totpop2015) %>%
  mutate_at(vars(txc2010, txc2030, diff), funs(pc=. * 1e6 / pop))
cex_change
precis(cex_change)


precis(cex_change %>% filter(stabbr != "DC"))

cex_change %>% arrange(pch)
write_csv(cex_change, "./results/cex_change.csv")

quantile(cex_change$pch, probs=c(0, .1, .25, .5, .75, .9, 1))


xgst <- c("DC")
p <- cex_change %>% filter(!stabbr %in% xgst) %>%
  arrange(desc(pch)) %>%
  mutate(stname=factor(stabbr, levels=stcodes$stabbr, labels=stcodes$stname),
         stname2=factor(stname, levels=stname, labels=stname, ordered=TRUE),
         pchround=round(pch, 1)) %>%
  ggplot(aes(x=stname2, y=pch)) +
  geom_bar(stat="identity", fill="blue") +
  scale_y_continuous(name=NULL, breaks=seq(-3, 3, .2)) +
  scale_x_discrete(name=NULL) +
  geom_text(aes(label=pchround), nudge_y=0.1, size=3) +
  theme_bw() +
  ggtitle(label="Taxable sales") +
  coord_flip() +
  geom_vline(xintercept=0) +
  theme(axis.text.y=element_text(hjust=0.5))
# apparently must adjust the state labels AFTER the flip, as y axis (I think)
p



#****************************************************************************************************
#                Median pit changes ####
#****************************************************************************************************
sgtax.a %>% filter(year %in% 2007:2010, vname=="iit", stabbr!="US", value>0) %>%
  select(stabbr, year, value) %>%
  spread(year, value) %>%
  mutate(pch1=`2009` / `2008` * 100 - 100,
         pch2=`2010` / `2009` * 100 - 100) %>%
  summarise_at(vars(pch1, pch2), funs(median))


#****************************************************************************************************
#                Consumer expenditure survey - exp breakdown ####
#****************************************************************************************************

fn <- "D:/Dropbox/Open Projects/NCSL Boston Feb 2017/Presentation/BoydNCSLBoston2017(10).xlsx"

df <- read_excel(fn, sheet="CEX_export", skip=1)
names(df)
df <- df[, which(!is.na(names(df)))] # fix bad data
names(df)


# get df with agerp and agegrp
agevals <- tibble(agegrp=names(df)[-c(1:2)], agerp=df[1, -c(1:2)] %>% t %>% as.vector)

vars <- c("exp_total", "exp_txblcons", "food_home", "food_away", "alcohol", "housing", "housing_shelter",
          "apparel", "transport", "healthcare", "entertainment", "perscare", "reading", "education", "tobacco")

ages <- c("agerp_all", "agerp_00to24", "agerp_25to34", "agerp_35to44", "agerp_45to54", 
          "agerp_55to64", "agerp_65to74", "agerp_75+", "agerp_65+")

df2 <- df %>%
  select(-description) %>%
  filter(vname %in% vars) %>%
  gather(agegrp, value, -vname) %>%
  spread(vname, value) %>%
  mutate(alctob=alcohol + tobacco,
         housing_xshelter=housing - housing_shelter) %>%
  gather(vname, value, -agegrp) %>%
  left_join(agevals)

voi <- c("exp_total", "exp_txblcons", "food_home", "food_away", "alctob", "housing", "apparel", "transport",
         "healthcare", "entertainment", "perscare", "reading", "education")
voilabs <- c("Total", "Taxable", "Food at home", "Food away", "Alcohol & tobacco", "Housing", "Apparel", "Transportation",
             "Health care", "Entertainment", "Personal care", "Reading", "Education")
cbind(voi, voilabs)


srcnote <- "\nSource: U.S. Bureau of the Census, Consumer Expenditure Survey 2015 Table 1300, plus author's calculations"
note <- "Inspired by Oregon Office of Economic Analysis, https://oregoneconomicanalysis.com/2014/03/19/aging-revenues/"
capt <- paste0(srcnote, "\n", note)

capt <- srcnote


p <- df2 %>% filter(agegrp %in% c("agerp_55to64", "agerp_65+"),
                    vname %in% voi) %>%
  select(-agerp) %>%
  spread(agegrp, value) %>%
  mutate(pch=`agerp_65+` / agerp_55to64 - 1,
         vlab=factor(vname, levels=voi, labels=voilabs)) %>%
  arrange(pch) %>%
  mutate(vlab2=factor(vlab, levels=vlab, labels=vlab, ordered=TRUE)) %>%
  ggplot(aes(vlab2, pch)) +
  geom_bar(stat="identity", fill="blue") +
  scale_y_continuous(name=NULL, 
                     breaks=seq(-2, 2, .1), 
                     labels = scales::percent) +
  scale_x_discrete(name=NULL) +
  geom_hline(aes(yintercept = pch[vname=="exp_total"]), linetype="dashed", size=1) +
  geom_hline(yintercept = 0, linetype="solid", size=1) +
  theme_bw() +
  ggtitle(label="Expenditures per household, 65+ households relative to 55-64 year-old households") +
  labs(caption=capt) +
  coord_flip() +
  theme(axis.text.y=element_text(hjust=1)) +
  theme(plot.title = element_text(size=rel(1.3), face="bold")) +
  theme(axis.title = element_text(face="bold", size=rel(1))) +
  theme(axis.text = element_text(face="bold", size=rel(1))) +
  theme(plot.caption = element_text(hjust=0, size=11))
p
ggsave(p, file=paste0("./results/CEX_by_item_hbar.png"), width=10, height=6.25, units="in")
