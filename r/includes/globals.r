
#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
if(!exists("globals")) globals <- list()

globals$case_study_states <- c("CA", "NH", "NY", "OH", "TN", "TX") # these are in order of state name, alphabetically!!
globals$nonpit_states <- c("AK", "FL", "NV", "NH", "SD", "TN", "TX", "WA", "WY")
globals$wc_young <- c("pop00_04", "pop05_09", "pop10_14", "pop15_19")
globals$wc_workage <- c("pop20_24", "pop25_29", "pop30_34", "pop35_39", "pop40_44", "pop45_49", "pop50_54", "pop55_59", "pop60_64")
globals$wc_older <- c("pop65_69", "pop70_74", "pop75_79", "pop80_84", "pop85plus")
globals$young_vec <- 0:19
globals$workage_vec <- 20:64
globals$older_vec <- 65:110
globals$fred_apikey <- "a5e1199baac333154cbffcba3b263c28"
