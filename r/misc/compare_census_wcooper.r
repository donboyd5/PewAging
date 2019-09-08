

# compare population estimates and projections
cpop <- readRDS("./data/popproj/uspopproj.rds") # Census
wpop <- readRDS("./data/popproj/wc_popproj.rds") # Weldon Cooper

glimpse(cpop)
count(cpop, year) # 2016-2060
count(cpop, age)

glimpse(wpop)

# compare total pops by year
cp1 <- cpop %>%
  filter(year <= 2040, sex==0, nativity==0, race_hisp ==0, age==-1) %>%
  select(year, pop)

wp1 <- wpop %>%
  filter(sex=="Total", stabbr=="US", popgroup=="poptot") %>%
  select(year, pop=value)
wp1

bind_rows(cp1 %>% mutate(src="census"),
          wp1 %>% mutate(src="wcooper")) %>%
  ggplot(aes(year, pop, colour=src)) + geom_line() + geom_point() # census has lower pop in 2030 and 2040, but 2020 is about the same

d <- cp1 %>% 
  filter(year %in% c(2010, 2020, 2030, 2040)) %>%
  rename(cpop=pop) %>%
  left_join(wp1 %>% rename(wpop=pop)) %>%
  mutate(wcratio=wpop / cpop * 100)
# wc is 0.3% higher in 2020, 1.6% higher in 2030, 2.6% higher in 2040






