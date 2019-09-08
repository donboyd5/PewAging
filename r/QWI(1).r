
# 3/4/2019

# https://ledextract.ces.census.gov/static/data.html


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
pop65p <- c("pop65_69", "pop70_74", "pop75_79", "pop80_84", "pop85plus")

# D:\Data\BLS\QWI\Pew
qdir <- "D:/Data/BLS/QWI/Pew/"
qfn <- "qwi_Pew.csv"


#****************************************************************************************************
#                Get data ####
#****************************************************************************************************

df <- read_csv(paste0(qdir, qfn))
glimpse(df)
summary(df)
fivenum(df$Emp)
describe(df)

# geography, geography_label.value stname, agegrp, agegrp_label.value, year, quarter - what is sEmp


qwi <- df %>%
  select(year, quarter, stfips=geography, stname=geography_label.value, agegrp, agegrpf=agegrp_label.value,
         emp=Emp) %>%
  left_join(stcodes %>% select(stabbr, stfips) %>% mutate(stfips=as.numeric(stfips)))
glimpse(qwi)
count(qwi, year) # 2000 forward pretty good but not perfect
count(qwi, agegrp, agegrpf)
count(qwi, stabbr, stname, stfips)

qwi2 <- qwi %>%
  filter(year>=2000) %>%
  group_by(stabbr, year, agegrp, agegrpf) %>%
  summarise(n=n(), emp=mean(emp)) %>%
  ungroup
count(qwi2, n)
qwi2 %>% filter(n < 4) # none are case study states

qwicss <- qwi2 %>%
  filter(stabbr %in% globals$case_study_states)

qwicss %>%
  filter(stabbr=="NY", year>=2016)


p <- qwicss %>%
  mutate(agegrpf=fct_collapse(agegrpf, 
                              `14-24`=c("14-18", "19-21", "22-24"),
                              `55+`=c("55-64", "65-99"))) %>%
  group_by(stabbr, year, agegrpf) %>%
  summarise(emp=sum(emp)) %>%
  group_by(stabbr, year) %>%
  mutate(share=emp / emp[agegrpf=="All Ages (14-99)"] * 100) %>%
  filter(!agegrpf %in% c("All Ages (14-99)")) %>%
  # filter(stabbr=="OH") %>%
  ggplot(aes(year, share, colour=agegrpf)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_colour_manual(values=c("brown", "lightblue", "red", "green", "black")) +
  ggtitle("Employment trends by age group (QWI)") +
  theme_bw() +
  facet_wrap(~stabbr, ncol=2)
ggsave("./results/employment_byage.png", plot=p, width=8, height=9, units="in")


# compare older younger ----
count(qwicss, agegrp, agegrpf)
p <- qwicss %>%
  mutate(agegrpf=fct_collapse(agegrpf, 
                              `19-34`=c("19-21", "22-24", "25-34"),
                              `55+`=c("55-64", "65-99"))) %>%
  group_by(stabbr, year, agegrpf) %>%
  summarise(emp=sum(emp)) %>%
  group_by(stabbr, year) %>%
  mutate(share=emp / emp[agegrpf=="All Ages (14-99)"] * 100) %>%
  filter(!agegrpf %in% c("All Ages (14-99)", "14-18", "35-44", "45-54")) %>%
  # filter(stabbr=="OH") %>%
  #filter(agegrpf=="55+", year>=2005) %>%
  ggplot(aes(year, share, colour=stabbr)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_y_continuous(breaks=seq(10, 50, 2)) +
  scale_x_continuous(breaks=seq(2000, 2020, 1)) +
  # scale_colour_manual(values=c("brown", "lightblue", "red", "green", "black")) +
  ggtitle("Employment trends by age group (QWI)") +
  theme_bw() +
  facet_wrap(~agegrpf, ncol=2)
p
# ggsave("./results/emp_oldyoung.png", plot=p, width=8, height=9, units="in")


p <- qwicss %>%
  mutate(agegrpf=fct_collapse(agegrpf, 
                              `19-34`=c("19-21", "22-24", "25-34"),
                              `55+`=c("55-64", "65-99"))) %>%
  group_by(stabbr, year, agegrpf) %>%
  summarise(emp=sum(emp)) %>%
  group_by(stabbr, agegrpf) %>%
  mutate(iemp=emp / emp[year==2005] * 100) %>%
  filter(agegrpf %in% c("19-34", "55+")) %>%
  filter(year>=2005) %>%
  ggplot(aes(year, iemp, colour=stabbr)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 200, 10)) +
  scale_x_continuous(breaks=seq(2000, 2020, 1)) +
  # scale_colour_manual(values=c("brown", "lightblue", "red", "green", "black")) +
  ggtitle("Employment trends by age group (QWI)") +
  theme_bw() +
  facet_wrap(~agegrpf, ncol=2)
p


p <- qwicss %>%
  mutate(agegrpf=fct_collapse(agegrpf, 
                              `< 55`=c("14-18", "19-21", "22-24", "25-34", "35-44", "45-54"),
                              `55+`=c("55-64", "65-99"))) %>%
  group_by(stabbr, year, agegrpf) %>%
  summarise(emp=sum(emp)) %>%
  group_by(stabbr, agegrpf) %>%
  mutate(iemp=emp / emp[year==2005] * 100) %>%
  # filter(!agegrpf %in% c("All Ages (14-99)")) %>%
  filter(year>=2005) %>%
  ggplot(aes(year, iemp, colour=agegrpf)) +
  geom_line(size=1.2) +
  geom_point() +
  geom_hline(yintercept = 100) +
  scale_y_continuous(breaks=seq(0, 200, 10)) +
  scale_x_continuous(breaks=seq(2000, 2020, 1)) +
  # scale_colour_manual(values=c("brown", "lightblue", "red", "green", "black")) +
  ggtitle("Employment trends by age group (QWI)") +
  theme_bw() +
  facet_wrap(~stabbr, ncol=2)
p
