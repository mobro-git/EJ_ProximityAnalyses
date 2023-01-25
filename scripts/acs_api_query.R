###################################################
#################################  GET CENSUS - ACS
###################################################

## This will take approximately 1-2 hours if ACS data are not already downloaded

## census api key
## get one at: https://api.census.gov/data/key_signup.html
#  census_api_key("73898e028f52e49f9e5fbb0ca774d91d38c1672d", install=T,overwrite=T)
#readRenviron("~/.Renviron")

# # geography at which to draw data
geography = "block group"
# 
# # year of ACS to draw data
year = 2019
# 
# ## Get dictionary of ACS variables
acs_variables <- load_variables(year, "acs5", cache = TRUE)
# # write.xlsx(acs_variables,"data\\acs_variables_2019.xlsx") # write to optionally search spreadsheet
# 
# ## Define variables of interest from dictionary
variables = c(pop="B02001_001",white="B02001_002",black="B02001_003",indian="B02001_004",asian="B02001_005",hispanic="B03003_003",hispanic_denominator="B03003_001",
              pov50="C17002_002",pov99="C17002_003",deficit="B17011_001",income="B19013_001")
# 
# # cache the geometries downloaded from census
options(tigris_use_cache=TRUE)
# 
# # file name for the stored acs data
acs_file = paste0("/acs_data_",year,"_",geography,".Rdata")
# 
# # download the acs data if it doesn't exist
if (file.exists(file.path("data\\acs_data",acs_file))) {
  load(file.path("data\\acs_data",acs_file))
  
} else {
  
  # setup the cluster to download and process the data
  cl = makeCluster(5,outfile="")
  registerDoSNOW(cl)
  
  # list of state abbreviations 
  states = c(state.abb,"DC")  #state abb is a vector of state abbrev.
  #states = c("AL","AR","CA","IL","IN","IA","LA","TX","WV")
  
  
  # tidycensus will only return tract or block group data for a single state, so
  # we need to loop through each state and combine the results
  data = foreach (i=1:length(states),.combine=rbind,
                  .packages=c("tidycensus","tidyverse")) %dopar% {
                    print(paste("starting state:",states[i]))
                    get_acs(geography=geography,
                            state=states[i],
                            variables=variables, 
                            year=year,
                            geometry=TRUE) %>%
                      select(-NAME,-moe)
                  }
  
  #   # save the acs data
  if (!dir.exists("data\\acs_data"))
    dir.create("data\\acs_data")
  save(file=file.path("data\\acs_data",acs_file),list="data")
  
  stopCluster(cl)
  
}
