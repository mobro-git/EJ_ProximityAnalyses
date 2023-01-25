
############   Setup ----

rm(list = ls())
gc()

# Load libraries
source("scripts/packages.R")

# Load ACS data, pull from API if .Rdata file doesnt exist
#source("acs_api_query.R")
load("data/acs_data/acs_data_2019_block group.Rdata")

# Load NATA data
source("scripts/nata_data_pull.R")

## get urban areas for determining rural status
urban_areas <- urban_areas()

####################################################
#########################   Production facility data
####################################################

facilities <- read_excel("data/Allocation_Final_list production facilities 2022.xls") %>%
  rename(Latitude = LATITUDE,
         Longitude = LONGITUDE,
         GHG_co2e = `GHG QUANTITY (METRIC TONS CO2e)`,
         City = `CITY NAME`,
         State = STATE) %>%
  mutate(Label = `FACILITY NAME`) %>%
  select(Longitude,Latitude,everything()) %>%
  filter(Label != "CHEMOURS CHAMBERS WORKS")

facilities_lat_lon <- facilities %>%
  select(Longitude,Latitude,Label) %>%
  rename(lon = Longitude,
         lat = Latitude)

facilities_sf = st_as_sf(facilities,
                      coords=c(x="Longitude",y="Latitude"),
                      crs=4326) %>%
  st_transform(3488)

### Indicating rural vs urban facilities. 1 = rural, 0 = urban

uac <- urban_areas %>% st_transform(3488)

facilities_sf_urban <- st_intersection(facilities_sf,uac) %>%
  mutate(rural = 0)

facilities_sf_rural <- facilities_sf %>%
  mutate(rural = fifelse(Label %in% unique(facilities_sf_urban$Label),0,1)) %>%
  as.data.frame() %>%
  select(rural,Label)

facilities_map <- facilities_sf %>%
  left_join(facilities_sf_rural, by = "Label") %>%
  left_join(facilities_lat_lon, by = "Label")

####################################################
#########################   Plot Production facility
####################################################

facilities_map_t <- usmap_transform(facilities_map)

fac_map <- plot_usmap(include=c(.northeast_region,.south_region,.north_central_region), #,.west_north_central,.west_region,.west_south_central
                  labels=TRUE,
                  fill = "#C5CFE3",
                  alpha = 0.5) +
  ggrepel::geom_label_repel(data = facilities_map_t,
                            aes(x = x, y = y,
                                label = Label),
                            size = 5, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "#C60404", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_map_t,
             aes(x = x, y = y, size = GHG_co2e),
             color = "purple", alpha = 0.5) +
  geom_point(data = facilities_map_t,
             aes(x = x, y = y),
             color = "#C60404") +
  scale_size_continuous(range = c(1, 16),
                        label = scales::comma) +
  labs(size = expression("GHG Releases (mt CO"[2]*"e)")) +
  theme(legend.position = c(0.85, 0.1))

fac_map

ggsave("output/Allocation Rule/allocation_rule_facilities_map.png", fac_map, width = 10, height = 6)

####################################################
############  Prep facilities for proximity analysis
####################################################

# add the census tract to the data in case the geography is a lower resolution
data_ct <- data %>% mutate(Tract=substr(GEOID,1,11))

# get the geography geometry from the acs data
shp = data_ct %>%
  filter(variable=="pop") %>%
  select(GEOID,Tract) %>%
  arrange(GEOID) %>%
  st_transform(3488)

###### THIS STEP TAKES TIME
# get the geography geometry from the acs data

tr <- readRDS("data/tr.rds")

# tr = shp %>% group_by(Tract) %>% summarize(geometry=st_union(geometry))
# saveRDS(tr, "data/tr.rds")

tr_pts <- tr %>% st_centroid()

###### THIS STEP TAKES TIME
## identify rural and urban geoid

urban_tracts <- readRDS("data/urban_tracts.rds")

# urban_tracts <- st_intersection(tr_pts,uac)
# saveRDS(urban_tracts,"data/urban_tracts.rds")

#########
##rural_tracts <- st_intersection(tr_pts,uac) (Note this is the same as urban?)
#########

shp_rural <- shp %>% mutate(rural = fifelse(Tract %in% urban_tracts$Tract,0,1))

# identify rural and urban census blocks
sq_miles <- shp %>% mutate(sq_miles = units::set_units(st_area(shp),"mi^2")) %>%
  select(GEOID,sq_miles)

units(sq_miles$sq_miles) <- NULL

# prepare for merge with data
sq_miles %<>% st_set_geometry(NULL) %>% as.data.table() %>% setkey('GEOID')

# draw a buffer around the facilities
# buffer_dist is in miles so we need to multiply by 1609.34 meters/mile

communities = st_buffer(facilities_map, dist=1*1609.34)

communities_3mi = st_buffer(facilities_map, dist=3*1609.34)

communities_5mi = st_buffer(facilities_map, dist=5*1609.34)

communities_10mi = st_buffer(facilities_map, dist=10*1609.34)

# find the census geographies within the buffer around the facilities

buffer = st_intersection(communities,shp) %>%
  select(GEOID,Tract,Label)

buffer_3mi = st_intersection(communities_3mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_5mi = st_intersection(communities_5mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_10mi = st_intersection(communities_10mi,shp) %>%
  select(GEOID,Tract,Label)

# # get GEOID to facility list
facility_buffer <- st_intersection(facilities_map, buffer) %>% select(Label, GEOID) %>% st_set_geometry(NULL)

facility_buffer_3mi <- st_intersection(facilities_map, buffer_3mi) %>% select(Label, GEOID) %>% st_set_geometry(NULL)

facility_buffer_5mi <- st_intersection(facilities_map, buffer_5mi) %>% select(Label, GEOID) %>% st_set_geometry(NULL)

facility_buffer_10mi <- st_intersection(facilities_map, buffer_10mi) %>% select(Label, GEOID) %>% st_set_geometry(NULL)

# drop the geometry to work with the data alone
table_full <- data_ct %>%
  st_set_geometry(NULL) %>%
  as.data.table() %>%
  setkey('GEOID')

table_1 <- table_full[sq_miles]

# merge the acs and nata data
table_2 <- table_1 %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(white_pct=(white/pop)*100,
         minority_black=(black/pop)*100,
         minority_other=((pop-(white + black))/pop)*100,
         minority_hispanic=(hispanic/hispanic_denominator)*100,
         pov99=pov99/pop*100,
         pov50=pov50/pop*100,
         income=income/1000,
         rural = fifelse(Tract %in% urban_tracts$Tract,0,1)) %>%
  left_join(nata_data,by=c("Tract"="Tract")) %>%
  left_join(nata_data_resp,by=c("Tract"="Tract")) %>%
  as.data.table() %>%
  setkey('GEOID')

# merge the acs and facility data

facility_demographics_1mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer),by="Label")
facility_demographics_3mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer_3mi),by="Label")
facility_demographics_5mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer_5mi),by="Label")
facility_demographics_10mi_pre <- merge(as.data.table(facilities_map), as.data.table(buffer_10mi),by="Label")

facility_demographics_1mi_mid <- merge(facility_demographics_1mi_pre, table_2, by="GEOID") %>%
  select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,
         white,black,indian,asian,hispanic,income,pov50,pov99,
         total_risk,total_risk_resp) %>%
  rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_3mi_mid <- merge(facility_demographics_3mi_pre, table_2, by="GEOID") %>%
  select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,
         white,black,indian,asian,hispanic,income,pov50,pov99,
         total_risk,total_risk_resp) %>%
  rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_5mi_mid <- merge(facility_demographics_5mi_pre, table_2, by="GEOID") %>%
  select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,
         white,black,indian,asian,hispanic,income,pov50,pov99,
         total_risk,total_risk_resp) %>%
  rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_10mi_mid <- merge(facility_demographics_10mi_pre, table_2, by="GEOID") %>%
  select(Label,City,GHG_co2e,GEOID,sq_miles,rural.x,rural.y,pop,
         white,black,indian,asian,hispanic,income,pov50,pov99,
         total_risk,total_risk_resp) %>%
  rename(rural_facility = rural.x, rural_blockgroup = rural.y)

facility_demographics_1mi <- facility_demographics_1mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(
    blockgroups_n = n(),
    sq_miles = sum(sq_miles, na.rm=TRUE),
    pop = sum(pop, na.rm=TRUE),
    white = sum(white, na.rm=TRUE),
    black = sum(black, na.rm=TRUE),
    indian = sum(indian, na.rm=TRUE),
    asian = sum(asian, na.rm=TRUE),
    hispanic = sum(hispanic, na.rm=TRUE),
    income = mean(income, na.rm=TRUE),
    pov50 = mean(pov50, na.rm=TRUE),
    pov99 = mean(pov99, na.rm=TRUE),
    total_risk = mean(total_risk, na.rm=TRUE),
    total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_1mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>%
  ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_1mi,
         rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,
         income,pov50,pov99,total_risk,total_risk_resp) %>%
  distinct()

  write.xlsx(facility_demographics_1mi,"output/Allocation Rule/facility_data/allocation_rule_facility_demographics_1mi.xlsx", overwrite = TRUE)

facility_demographics_3mi <- facility_demographics_3mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(blockgroups_n = n(),
         sq_miles = sum(sq_miles, na.rm=TRUE),
         pop = sum(pop, na.rm=TRUE),
         white = sum(white, na.rm=TRUE),
         black = sum(black, na.rm=TRUE),
         indian = sum(indian, na.rm=TRUE),
         asian = sum(asian, na.rm=TRUE),
         hispanic = sum(hispanic, na.rm=TRUE),
         income = mean(income, na.rm=TRUE),
         pov50 = mean(pov50, na.rm=TRUE),
         pov99 = mean(pov99, na.rm=TRUE),
         total_risk = mean(total_risk, na.rm=TRUE),
         total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_3mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>%
  ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_3mi,
         rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,
         income,pov50,pov99,total_risk,total_risk_resp) %>%
  distinct()

write.xlsx(facility_demographics_3mi,"output/Allocation Rule/facility_data/allocation_rule_facility_demographics_3mi.xlsx", overwrite = TRUE)

facility_demographics_5mi <- facility_demographics_5mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(blockgroups_n = n(),
         sq_miles = sum(sq_miles, na.rm=TRUE),
         pop = sum(pop, na.rm=TRUE),
         white = sum(white, na.rm=TRUE),
         black = sum(black, na.rm=TRUE),
         indian = sum(indian, na.rm=TRUE),
         asian = sum(asian, na.rm=TRUE),
         hispanic = sum(hispanic, na.rm=TRUE),
         income = mean(income, na.rm=TRUE),
         pov50 = mean(pov50, na.rm=TRUE),
         pov99 = mean(pov99, na.rm=TRUE),
         total_risk = mean(total_risk, na.rm=TRUE),
         total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_5mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>%
  ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_5mi,
         rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,
         income,pov50,pov99,total_risk,total_risk_resp) %>%
  distinct()

write.xlsx(facility_demographics_5mi,"output/Allocation Rule/facility_data/allocation_rule_facility_demographics_5mi.xlsx", overwrite = TRUE)

facility_demographics_10mi <- facility_demographics_10mi_mid %>%
  group_by(Label,City,GHG_co2e) %>%
  mutate(blockgroups_n = n(),
         sq_miles = sum(sq_miles, na.rm=TRUE),
         pop = sum(pop, na.rm=TRUE),
         white = sum(white, na.rm=TRUE),
         black = sum(black, na.rm=TRUE),
         indian = sum(indian, na.rm=TRUE),
         asian = sum(asian, na.rm=TRUE),
         hispanic = sum(hispanic, na.rm=TRUE),
         income = mean(income, na.rm=TRUE),
         pov50 = mean(pov50, na.rm=TRUE),
         pov99 = mean(pov99, na.rm=TRUE),
         total_risk = mean(total_risk, na.rm=TRUE),
         total_risk_resp = mean(total_risk_resp, na.rm=TRUE)) %>%
  mutate(pop_sq_mile_10mi = pop/sq_miles,
         rural_bg_pct = signif(sum(rural_blockgroup/blockgroups_n, na.rm=TRUE),2)) %>%
  ungroup() %>%
  select(Label,City,GHG_co2e,blockgroups_n,sq_miles,pop,pop_sq_mile_10mi,
         rural_facility,rural_bg_pct,white,black,indian,asian,hispanic,
         income,pov50,pov99,total_risk,total_risk_resp) %>%
  distinct()

write.xlsx(facility_demographics_10mi,"output/Allocation Rule/facility_data/allocation_rule_facility_demographics_10mi.xlsx", overwrite = TRUE)

####################################################
########################  Conduct proximity analysis
####################################################

table <- as.data.frame(table_2)

# variables along which the comparisons should be made for the tables
comparison_vars = c("white_pct",'minority_black','minority_other','minority_hispanic',
                    "income",
                    "pov99","pov50",
                    "total_risk","total_risk_resp")

# descriptions of the comparison variables to be included in the tables
desc_vars = c("% White","% Black or African American ","% Other","% Hispanic",
              "Median Income [1,000 2019$]",
              "% Below Poverty Line","% Below Half the Poverty Line",
              "Total Cancer Risk (per million)",
              'Total Respiratory (hazard quotient)')

# get the national level averages
summary_table = data.frame(Variable=desc_vars)
summary_table_sd = data.frame(Variable=desc_vars)

for (v in 1:length(comparison_vars)) {
  summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
  a = (table$pop*table[,comparison_vars[v]])/table$pop
  summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the rural area level averages
rural <- table %>% filter(rural==1)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (National Average)"] = sum(rural$pop*rural[,comparison_vars[v]],na.rm=T)/sum(rural$pop,na.rm=T)
  a = (rural$pop*rural[,comparison_vars[v]])/rural$pop
  summary_table_sd[v,"Rural Areas (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local = table$GEOID %in% unique(buffer$GEOID)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
  a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
  summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
  a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
  summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
  a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
  summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
  a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
  summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:7] = signif(summary_table[,2:7],2)
summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)

summary_table_all <- summary_table
summary_table_all_sd <- summary_table_sd

# export
list_of_datasets <- list("Means" = summary_table_all, "Standard Deviations" = summary_table_all_sd)
write.xlsx(list_of_datasets,"output/Allocation Rule/summary_tables/allocation_rule_summary_tables_national.xlsx", overwrite = TRUE)

####################################################
###############  Conduct proximity analysis by plant - rural
####################################################

facilities_rural <- facilities_map %>% filter(rural == 1)

for (i in 1:length(facilities_rural)){

facility <- paste0(facilities_rural[i,]$Label)
communities = st_buffer(facilities_rural[i,],dist=1*1609.34)
communities_3mi = st_buffer(facilities_rural[i,],dist=3*1609.34)
communities_5mi = st_buffer(facilities_rural[i,],dist=5*1609.34)
communities_10mi = st_buffer(facilities_rural[i,],dist=10*1609.34)

# find the census geographies within the buffer around the facilities
buffer = st_intersection(communities,shp) %>%
  select(GEOID,Tract,Label)

buffer_3mi = st_intersection(communities_3mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_5mi = st_intersection(communities_5mi,shp) %>%
  select(GEOID,Tract,Label)

buffer_10mi = st_intersection(communities_10mi,shp) %>%
  select(GEOID,Tract,Label)

# get the national level averages
summary_table = data.frame(Variable=desc_vars)
summary_table_sd = data.frame(Variable=desc_vars)

# for (v in 1:length(comparison_vars)) {
#   summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
#   a = (table$pop*table[,comparison_vars[v]])/table$pop
#   summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
# }

# # get the state-level averages
# state <- table %>% filter(State==facilities[i,]$`STATE`)
# for (v in 1:length(comparison_vars)) {
#   summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
#   a = (state$pop*state[,comparison_vars[v]])/state$pop
#   summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
# }

# get the national rural area level averages
rural <- table %>% filter(rural==1)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (National Average)"] = sum(rural$pop*rural[,comparison_vars[v]],na.rm=T)/sum(rural$pop,na.rm=T)
  a = (rural$pop*rural[,comparison_vars[v]])/rural$pop
  summary_table_sd[v,"Rural Areas (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the rural area level averages in that state
state <- table %>% filter(rural==1 & State==facilities_rural[i,]$`State`)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Rural Areas (State Average)"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
  a = (state$pop*state[,comparison_vars[v]])/state$pop
  summary_table_sd[v,"Rural Areas (State Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local = table$GEOID %in% unique(buffer$GEOID)
for (v in 1:length(comparison_vars)) {
  summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
  a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
  summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
  a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
  summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
  a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
  summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# get the population weighted averages around the production facilities
local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
for (v in 1:length(comparison_vars))  {
  summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
  a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
  summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
}

# only include two significant figures in the summary table
summary_table[,2:7] = signif(summary_table[,2:7],2)
summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)

# export
list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
write.xlsx(list_of_datasets,paste0("output/Allocation Rule/summary_tables/allocation_rule_summary_tables_",facility,".xlsx"), overwrite = TRUE)

}

####################################################
###############  Conduct proximity analysis by plant- URBAN
####################################################

# facilities_urban <- facilities[facilities$Label=="CHEMOURS LOUISVILLE WORKS" | facilities$Label=="DAIKIN AMERICA INC.",]
facilities_urban <- facilities_map %>% filter(rural == 0)

for (i in 1:length(facilities_urban)){

  facility <- paste0(facilities_urban[i,]$Label)
  communities = st_buffer(facilities_urban[i,],dist=1*1609.34)
  communities_3mi = st_buffer(facilities_urban[i,],dist=3*1609.34)
  communities_5mi = st_buffer(facilities_urban[i,],dist=5*1609.34)
  communities_10mi = st_buffer(facilities_urban[i,],dist=10*1609.34)

  # find the census geographies within the buffer around the facilities
  buffer = st_intersection(communities,shp) %>%
    select(GEOID,Tract,Label)

  buffer_3mi = st_intersection(communities_3mi,shp) %>%
    select(GEOID,Tract,Label)

  buffer_5mi = st_intersection(communities_5mi,shp) %>%
    select(GEOID,Tract,Label)

  buffer_10mi = st_intersection(communities_10mi,shp) %>%
    select(GEOID,Tract,Label)

  # get the national level averages
  summary_table = data.frame(Variable=desc_vars)
  summary_table_sd = data.frame(Variable=desc_vars)

  # for (v in 1:length(comparison_vars)) {
  #   summary_table[v,"Overall (National Average)"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
  #   a = (table$pop*table[,comparison_vars[v]])/table$pop
  #   summary_table_sd[v,"Overall (National Average) SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  # }

  # # get the state-level averages
  # state <- table %>% filter(State==facilities[i,]$`STATE`)
  # for (v in 1:length(comparison_vars)) {
  #   summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
  #   a = (state$pop*state[,comparison_vars[v]])/state$pop
  #   summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  # }

  # get the national rural area level averages
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"National Average"] = sum(table$pop*table[,comparison_vars[v]],na.rm=T)/sum(table$pop,na.rm=T)
    a = (table$pop*table[,comparison_vars[v]])/table$pop
    summary_table_sd[v,"National Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # get the rural area level averages in that state
  state <- table %>% filter(State==facilities_urban[i,]$`State`)
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"State Average"] = sum(state$pop*state[,comparison_vars[v]],na.rm=T)/sum(state$pop,na.rm=T)
    a = (state$pop*state[,comparison_vars[v]])/state$pop
    summary_table_sd[v,"State Average SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # get the population weighted averages around the production facilities
  local = table$GEOID %in% unique(buffer$GEOID)
  for (v in 1:length(comparison_vars)) {
    summary_table[v,"Within 1 mile of HFC production facility"] = sum(table$pop[local]*table[local,comparison_vars[v]],na.rm=T)/sum(table$pop[local],na.rm=T)
    a = (table$pop[local]*table[local,comparison_vars[v]])/table$pop[local]
    summary_table_sd[v,"Within 1 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # get the population weighted averages around the production facilities
  local_3mi = table$GEOID %in% unique(buffer_3mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 3 miles of HFC production facility"] = sum(table$pop[local_3mi]*table[local_3mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_3mi],na.rm=T)
    a = (table$pop[local_3mi]*table[local_3mi,comparison_vars[v]])/table$pop[local_3mi]
    summary_table_sd[v,"Within 3 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # get the population weighted averages around the production facilities
  local_5mi = table$GEOID %in% unique(buffer_5mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 5 miles of HFC production facility"] = sum(table$pop[local_5mi]*table[local_5mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_5mi],na.rm=T)
    a = (table$pop[local_5mi]*table[local_5mi,comparison_vars[v]])/table$pop[local_5mi]
    summary_table_sd[v,"Within 5 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # get the population weighted averages around the production facilities
  local_10mi = table$GEOID %in% unique(buffer_10mi$GEOID)
  for (v in 1:length(comparison_vars))  {
    summary_table[v,"Within 10 miles of HFC production facility"] = sum(table$pop[local_10mi]*table[local_10mi,comparison_vars[v]],na.rm=T)/sum(table$pop[local_10mi],na.rm=T)
    a = (table$pop[local_10mi]*table[local_10mi,comparison_vars[v]])/table$pop[local_10mi]
    summary_table_sd[v,"Within 10 mile of HFC production facility SD"] = sqrt(sum((a-mean(a, na.rm=TRUE))^2/(length(a)-1), na.rm=TRUE))
  }

  # only include two significant figures in the summary table
  summary_table[,2:7] = signif(summary_table[,2:7],2)
  summary_table_sd[,2:7] = signif(summary_table_sd[,2:7],2)

  # export
  list_of_datasets <- list("Means" = summary_table, "Standard Deviations" = summary_table_sd)
  write.xlsx(list_of_datasets,paste0("output/Allocation Rule/summary_tables/allocation_rule_summary_tables_",facility,".xlsx"), overwrite = TRUE)

}
