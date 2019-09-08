

NIPAvars
NIPAvars %>%
  filter(vname=="B230RC")

df <- readRDS("./data/popproj/uspopproj.rds")
glimpse(df)
count(df, year)
count(df, age)

# Organization for Economic Co-operation and Development, Working Age Population: Aged 15-64: All Persons for the United States [LFWA64TTUSA647N],
# retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/LFWA64TTUSA647N, March 13, 2019.

# World Bank, Population ages 65 and above for the United States [SPPOP65UPTOZSUSA], retrieved from FRED,
# Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/SPPOP65UPTOZSUSA, March 13, 2019.
# Population ages 65 and above as a percentage of the total population. Population is based on the de facto definition of population, 
# which counts all residents regardless of legal status or citizenship--except for refugees not permanently settled in the country of asylum,
# who are generally considered part of the population of the country of origin.

# World Bank, Age Dependency Ratio: Older Dependents to Working-Age Population for the United States [SPPOPDPNDOLUSA], retrieved from FRED,
# Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/SPPOPDPNDOLUSA, March 13, 2019.
# WB uses 65+ to 15-64. Data are shown as the proportion of dependents per 100 working-age population.


fredr_set_key(globals$fred_apikey)
fredr("LFWA64TTUSA647N", frequency="a")

wap <- fredr("LFWA64TTUSA647N", frequency="a")
wap


# SPPOPDPNDOLUSA depratio
# oa dependent ratio
drvars <- c("LFWA64TTUSA647N", "SPPOP65UPTOZSUSA", "SPPOPDPNDOLUSA")
drvnames <- c("wapop_oecd", "oapop_pct_wb", "oadr_wb")
drdata <- map_dfr(drvars, fredr, frequency="a") %>%
  mutate(year=year(date),
         vname=factor(series_id, levels=drvars, labels=drvnames)) %>%
  select(year, series_id, vname, value)
glimpse(drdata)
ht(drdata)

drd2 <- drdata %>%
  select(year, vname, value) %>%
  spread(vname, value) %>%
  mutate(wapop_oecd=wapop_oecd / 1e6)
ht(drd2)


#.. new ----
# history
drvars <- c("SPPOPDPNDOLUSA")
drvnames <- c("oadr_wb")
oadrhist <- map_dfr(drvars, fredr, frequency="a") %>%
  mutate(year=year(date),
         vname=factor(series_id, levels=drvars, labels=drvnames)) %>%
  select(year, vname, value) %>%
  spread(vname, value)
glimpse(oadrhist)
ht(oadrhist)

# projections
glimpse(wpop)
count(wpop, popgroup)
# "pop00_04", "pop05_09", "pop10_14", 
# "pop15_19", "pop20_24", "pop25_29", "pop30_34", "pop35_39", "pop40_44", "pop45_49", "pop50_54", "pop55_59", "pop60_64",
# "pop65_69", "pop70_74", "pop75_79", "pop80_84", "pop85plus"
ypop <- c("pop00_04", "pop05_09", "pop10_14")
wapop <- c("pop15_19", "pop20_24", "pop25_29", "pop30_34", "pop35_39", "pop40_44", "pop45_49", "pop50_54", "pop55_59", "pop60_64")
oapop <- c("pop65_69", "pop70_74", "pop75_79", "pop80_84", "pop85plus")
oadrproj <- wpop %>%
  filter(sex=="Total", stabbr=="US", popgroup!="poptot") %>%
  select(year, popgroup, pop=value) %>%
  mutate(popgroup=fct_collapse(popgroup,
                               ypop=ypop,
                               wapop=wapop,
                               oapop=oapop)) %>%
  group_by(year, popgroup) %>%
  summarise(pop=sum(pop)) %>%
  spread(popgroup, pop) %>%
  mutate(oadr_wc=oapop / wapop * 100)
oadrproj

oadr <- bind_rows(oadrhist %>% select(year, oadr_wb),
                  oadrproj %>% select(year, oadr_wc)) %>%
  gather(vname, value, -year) %>%
  filter(!is.na(value))
oadr

y10 <- oadr %>% filter(year==2010, vname=="oadr_wc") %>% .[["value"]]
y10 <- oadr %>% filter(year==2010, vname=="oadr_wb") %>% .[["value"]]
y20 <- oadr %>% filter(year==2020) %>% .[["value"]]
y30 <- oadr %>% filter(year==2030) %>% .[["value"]]
y40 <- oadr %>% filter(year==2040) %>% .[["value"]]

oadr %>%
  ggplot(aes(year, value, colour=vname)) +
  geom_line() +
  geom_curve(aes(x = 2010, y = y10, 
                 xend = 2020, yend = y20), size=1.1, linetype="dotted", colour="blue", curvature=0) +
  geom_curve(aes(x = 2020, y = y20, 
                 xend = 2030, yend = y30), size=1.1, linetype="dotted", colour="blue", curvature=0)

oadr %>%
  # filter(!(year==2010 & vname=="oadr_wc")) %>%
  ggplot(aes(year, value, colour=vname)) +
  geom_line() +
  geom_curve(aes(x = 2020, y = y20, 
                 xend = 2030, yend = y30), size=1.1, linetype="dotted", colour="blue", curvature=0)

smoothdf <- oadr %>% filter((vname=="oadr_wb" & year==2017) | (vname=="oadr_wc" & year > 2017))
spline_int <- as_tibble(spline(smoothdf$year, smoothdf$value)) %>% rename(year=x, value=y)

capt1 <- "History (blue line): World Bank, Age Dependency Ratio (https://fred.stlouisfed.org/series/SPPOPDPNDOLUSA). See text for details"
capt2 <- "Projections (green points): Author's calculations from projections from the University of Virginia, Weldon Cooper Center for Public Service"
capt <- paste0(capt1, "\n", capt2)
gtitle <- "Age dependency ratio: Older population per 100 working age population"
ggplot(aes(year, value), data=oadr) +
  geom_line(aes(year, value), data=oadr %>% filter(vname=="oadr_wb"), colour="blue") +
  geom_point(aes(year, value), data=oadr %>% filter(vname=="oadr_wc"), colour="darkgreen") +
  geom_line(aes(x = year, y = value), data=spline_int, colour="darkgreen", linetype="dashed") +
  scale_x_continuous(name=NULL, breaks=seq(1900, 2100, 10)) +
  scale_y_continuous(name="Age dependency ratio", breaks=seq(0, 50, 2), limits=c(0, NA)) +
  theme_bw() +
  ggtitle(gtitle) +
  labs(caption=capt) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0, size=10))

  



# prepare points needed to draw smoothed line from last World Bank point, through later Weldon Cooper projections
smoothdf <- oadr %>% filter((vname=="oadr_wb" & year==2017) | (vname=="oadr_wc" & year > 2017))
spline_int <- as_tibble(spline(smoothdf$year, smoothdf$value)) %>% rename(year=x, value=y)

capt1 <- "History (blue line): World Bank, Age Dependency Ratio (https://fred.stlouisfed.org/series/SPPOPDPNDOLUSA)"
capt2 <- "Projections (green points): Author's analysis of projections from University of Virginia, Weldon Cooper Center"
capt3 <- "World Bank definitions: Older age=65+, Working age=15-64"
capt <- paste0(capt1, "\n", capt2, "\n", capt3)

gtitle <- "Age dependency ratio: Older population per 100 working age population"

ylab1 <- "Age dependency ratio"
ylab2 <- "Population aged 65+ per 100 population aged 15-64"
# ylab <- paste0(ylab1, "\n", ylab2)
ylab <- ylab1

p <- ggplot(aes(year, value), data=oadr) +
  geom_line(aes(year, value), data=oadr %>% filter(vname=="oadr_wb"), colour="blue") +
  geom_point(aes(year, value), data=oadr %>% filter(vname=="oadr_wc"), colour="darkgreen") +
  geom_line(aes(x = year, y = value), data=spline_int, colour="darkgreen", linetype="dashed") +
  scale_x_continuous(name=NULL, breaks=seq(1900, 2100, 10)) +
  scale_y_continuous(name=ylab, breaks=seq(0, 50, 2), limits=c(0, NA)) +
  theme_bw() +
  ggtitle(gtitle) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=10))
# p

p2 <- p + gband(2011, 2029, alpha=.4) + 
  annotate("text", x=2020, y=13,
           hjust=0.5,
           vjust=0,
           label="Baby boomers\nturn 65")
# 1946 – 1964 baby boom birth years
p2
