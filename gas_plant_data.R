# Respiratory
gas_plant_file_resp = "gas_plant2014v2_national_resphi_by_tract_poll.xlsx"

download_file("https://www.epa.gov/sites/production/files/2018-08/",
              gas_plant_file_resp,
              dir=gas_plant_dir)

# load the gas_plant data
gas_plant_data = read_excel(file.path(gas_plant_dir,gas_plant_file)) %>%
 rename(total_risk='Total Cancer Risk (per million)')

gas_plant_data_resp = read_excel(file.path(gas_plant_dir,gas_plant_file_resp)) %>%
 rename(total_risk_resp='Total Respiratory (hazard quotient)') %>%
 select(Tract, total_risk_resp)