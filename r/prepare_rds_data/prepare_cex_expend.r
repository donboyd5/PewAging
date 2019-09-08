

# NOTE: See documentation far below

#****************************************************************************************************
#                Locations ####
#****************************************************************************************************
cexd <- "D:/Data/CensusCEX/2017/"

#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")
search()


#****************************************************************************************************
#                Get original source CEX Table 1300 summary data and save as cex_expend with vname ####
#****************************************************************************************************
#.. Get xwalk to taxable expenditures and aggregation rules ----
xwalk1 <- read_excel(paste0("./data/", "CES_Taxable_Crosswalk.xlsx"), skip=1)
xwalk1
xwalk2 <- xwalk1 %>%
  filter(!is.na(level)) %>%
  select(vname, item, level, linenum) # keep tax rules separate from xwalk, get them later
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
# create 2 files, each by age group - misc data, and expend data
misc <- cx %>%
  filter(group=="misc") %>%
  select(-group, -level) %>%
  gather(agegroup, value, -item, -vname) %>%
  select(vname, agegroup, value, item)
misc
count(misc, vname)
saveRDS(misc, "./data/cex_misc.rds")

cex_expend <- cx %>%
  filter(group=="expend", level < 9) %>%
  select(-group) %>%
  gather(agegroup, exp_avg, -vname, -item, -level) %>%
  left_join(misc %>% filter(vname=="nunitsk") %>% select(agegroup, nunitsk=value)) %>%
  mutate(exp_m=exp_avg * nunitsk / 1000) %>%
  select(vname, level, agegroup, nunitsk, exp_avg, exp_m, item)
ht(cex_expend)
saveRDS(cex_expend, "./data/cex_expend")

