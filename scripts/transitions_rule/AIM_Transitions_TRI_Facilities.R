
####################################################
##########################################  PREAMBLE
####################################################

## Clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('tidycensus','tigris','tidyverse','magrittr','data.table','sf','foreach','doSNOW','scales','odbc','colorspace','openxlsx','here','readxl','openxlsx',
                      'usmap','ggplot2','stringr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

####################################################
###################################   HFC facilities
####################################################

facilities_raw <- read_excel("data/Transitions_hfc_facilities-4-8-22_w_additions V2_frsID_updatedlatlon.xlsx") %>%
              rename(facility_id='FRSID',
                     ghg_quantity = `2020  GHG`,
                     FACILITY.NAME = `Facility Name`)


facilities_map <- facilities_raw %>% relocate(Longitude,Latitude)

facilities = st_as_sf(facilities_raw,
                      coords=c(x="Longitude",y="Latitude"),
                      crs=4326) %>%
             st_transform(3488)

####################################################
################################  All TRI facilities
####################################################

tri_facilities_raw <- read_csv("data/tri_data/tri_2020_us.csv") %>%
                  select('3. FRS ID','12. LATITUDE','13. LONGITUDE') %>%
                  rename(frs_id='3. FRS ID',
                         LATITUDE = '12. LATITUDE',
                         LONGITUDE = '13. LONGITUDE') %>%
                  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
                  unique()


tri_facilities_map <- tri_facilities_raw %>% relocate(LONGITUDE,LATITUDE)

tri_facilities = st_as_sf(tri_facilities_raw,
                          coords=c(x="LONGITUDE",y="LATITUDE"),
                          crs=4326) %>%
                          st_transform(3488)

####################################################
###########################################  Buffers
####################################################

# draw a buffer around the facilities
# buffer_dist is in miles so we need to multiply by 1609.34 meters/mile
communities = st_buffer(facilities,dist=1*1609.34)
communities_3mi = st_buffer(facilities,dist=3*1609.34)
communities_5mi = st_buffer(facilities,dist=5*1609.34)
communities_10mi = st_buffer(facilities,dist=10*1609.34)

# find the tri facilities within the buffer around the facilities
buffer = st_intersection(communities,tri_facilities) %>%
  select(facility_id,"FACILITY.NAME",frs_id) %>%
  group_by(facility_id,FACILITY.NAME) %>%
  tally(name='nearby_tri_1mi') %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

buffer_3mi = st_intersection(communities_3mi,tri_facilities) %>%
  select(facility_id,"FACILITY.NAME",frs_id) %>%
  group_by(facility_id,FACILITY.NAME) %>%
  tally(name='nearby_tri_3mi') %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

buffer_5mi = st_intersection(communities_5mi,tri_facilities) %>%
  select(facility_id,"FACILITY.NAME",frs_id) %>%
  group_by(facility_id,FACILITY.NAME) %>%
  tally(name='nearby_tri_5mi') %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

buffer_10mi = st_intersection(communities_10mi,tri_facilities) %>%
  select(facility_id,"FACILITY.NAME",frs_id) %>%
  group_by(facility_id,FACILITY.NAME) %>%
  tally(name='nearby_tri_10mi') %>%
  st_set_geometry(NULL) %>%
  as.data.frame()

nearby_tri_join <- facilities_map %>%
  left_join(buffer, by = c("facility_id", "FACILITY.NAME")) %>%
  left_join(buffer_3mi, by = c("facility_id", "FACILITY.NAME")) %>%
  left_join(buffer_5mi, by = c("facility_id", "FACILITY.NAME")) %>%
  left_join(buffer_10mi, by = c("facility_id", "FACILITY.NAME"))

# Merge is dropping facilities that dont have TRI w/in proximity for all 4 values (1,3,5,10 mi)
# nearby_tri_merged <- merge(buffer,buffer_3mi) %>%
#   merge(.,buffer_5mi) %>%
#   merge(.,buffer_10mi) %>%
#   merge(.,facilities_map)

nearby_tri_table <- nearby_tri_join %>%
  mutate(Location = paste(City, State, sep = ", ")) %>%
  rename(Facility = FACILITY.NAME,
         `Neighboring TRI Facilities within a 1-Mile Radius` = nearby_tri_1mi,
         `Neighboring TRI Facilities within a 3-Mile Radius` = nearby_tri_3mi,
         `Neighboring TRI Facilities within a 5-Mile Radius` = nearby_tri_5mi,
         `Neighboring TRI Facilities within a 10-Mile Radius` = nearby_tri_10mi) %>%
  select(Facility,Location,
         `Neighboring TRI Facilities within a 1-Mile Radius`,
         `Neighboring TRI Facilities within a 3-Mile Radius`,
         `Neighboring TRI Facilities within a 5-Mile Radius`,
         `Neighboring TRI Facilities within a 10-Mile Radius`)

transition_nearby_tri_table <- nearby_tri_table

write.xlsx(nearby_tri_table,"output/Transitions Rule/tri_facilities/transitions_nearby_tri_facilities.xlsx", overwrite = TRUE)

####################################################
#########################   Plot Production facility
####################################################

nearby_tri <- nearby_tri_join %>%
  relocate(Longitude,Latitude) %>%
  rename(lon = Longitude,
         lat = Latitude)

facilities_t <- usmap_transform(nearby_tri) %>%
  mutate(FACILITY.NAME = str_to_title(FACILITY.NAME))

plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central),
           labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_t,
                            aes(x = x , y = y ,
                                label = paste0(Label,' - ', nearby_tri_1mi, " TRI facilities")),
                            size = 4, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_t,
             aes(x = x, y = y, size = nearby_tri_1mi),
             color = "firebrick2", alpha = 0.5) +
  scale_size_continuous(range = c(1, 20),
                        label = scales::comma) +
  labs(size = expression("Nearby TRI Facilities \n      (within 1 mile)")) +
  theme(legend.position = c(0.85, 0.1))
ggsave("output/Transitions Rule/tri_facilities/TRI_1mi_map.png",width=11,height=8)

plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central),
           labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_t,
                            aes(x = x , y = y,
                                label = paste0(Label,' - ', nearby_tri_3mi, " TRI facilities")),
                            size = 4, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_t,
             aes(x = x, y = y, size = nearby_tri_3mi),
             color = "firebrick2", alpha = 0.5) +
  scale_size_continuous(range = c(1, 20),
                        label = scales::comma) +
  labs(size = expression("Nearby TRI Facilities \n      (within 3 miles)")) +
  theme(legend.position = c(0.85, 0.1))
ggsave("output/Transitions Rule/tri_facilities/TRI_3mi_map.png",width=11,height=8)

plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central),
           labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_t,
                            aes(x = x , y = y,
                                label = paste0(Label,' - ', nearby_tri_5mi, " TRI facilities")),
                            size = 4, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_t,
             aes(x = x, y = y, size = nearby_tri_3mi),
             color = "firebrick2", alpha = 0.5) +
  scale_size_continuous(range = c(1, 20),
                        label = scales::comma) +
  labs(size = expression("Nearby TRI Facilities \n      (within 5 miles)")) +
  theme(legend.position = c(0.85, 0.1))
ggsave("output/Transitions Rule/tri_facilities/TRI_5mi_map.png",width=11,height=8)

plot_usmap(include=c(.northeast_region,.south_region,.north_central_region,.west_north_central,.west_region,.west_south_central),
           labels=TRUE, fill = "yellow", alpha = 0.05) +
  ggrepel::geom_label_repel(data = facilities_t,
                            aes(x = x , y = y,
                                label = paste0(Label,' - ', nearby_tri_10mi, " TRI facilities")),
                            size = 4, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.5,
                            segment.color = "red", segment.size = 1,
                            seed = 1002) +
  geom_point(data = facilities_t,
             aes(x = x, y = y, size = nearby_tri_3mi),
             color = "firebrick2", alpha = 0.5) +
  scale_size_continuous(range = c(1, 20),
                        label = scales::comma) +
  labs(size = expression("Nearby TRI Facilities \n      (within 10 miles)")) +
  theme(legend.position = c(0.85, 0.1))
ggsave("output/Transitions Rule/tri_facilities/TRI_10mi_map.png",width=11,height=8)

