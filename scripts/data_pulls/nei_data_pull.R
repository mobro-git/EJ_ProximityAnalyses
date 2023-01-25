library(tidyverse)
library(readxl)

nei_data <- read_xlsx("data/2019 point selected OAP facilities 20663.xlsx") %>%
  select(`company name`, `site name`, `tri facility id`,
         state, address, city, `zip code`,
         `site latitude`, `site longitude`,
         `pollutant code`, `pollutant desc`, `pollutant type(s)`, `hap type`, 
         `total emissions`, `emissions uom`, 
         `primary naics code`, `primary naics description`, 
         `facility source type`, `unit type`, `unit description`, 
         `design capacity`, 
         `emission factor`, `ef numerator uom`, `ef denominator uom`, `calculation method`, 
         `source data set`) %>%
  # missing "Methyl Chloroform", "Carbon Tetrachloride", "Vinylidene Chloride" - in Framework Rule RIA but not in NEI table
  filter(`pollutant desc` %in% c("Ammonia","Antimony","Chlorine","Chloroform","Chromium (VI)",
                                 "Chromium III","Cobalt","Ethylene Dichloride","Ethylidene Dichloride",
                                 "Hydrochloric Acid","Hydrogen Fluoride","Methylene Chloride","Nickel",
                                 "Tetrachloroethylene","Trichloroethylene","Vinyl Chloride")) %>%
  mutate(use = case_when(
    `pollutant desc` %in% c("Antimony","Chromium (VI)","Chromium III","Cobalt","Nickel") ~ "catalyst", 
    TRUE~"feedstock"))
