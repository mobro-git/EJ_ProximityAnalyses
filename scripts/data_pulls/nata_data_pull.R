####################################################
############   Supporting function to download files
####################################################

## downloads a file if not present locally and extracts the contents if it is a
## compressed archive

## inputs:
##   url: url where the remote file resides
##   file_name: name of the remote file
##   dir: local directory to save the file [defulat is "."]
##   check_file: optional name of local file, where if present the download will
##              not occur. useful if the remote file is a compressed archive and
##               it shouldn't be downloaded if its contents is already present
##   tolower: covnert file names in a zip file to lower case [default is FALSE]
#

download_file = function(url,file_name,dir=".",check_file=NA,tolower=F) {

  library(httr)

  # create the directory if needed
  if (!dir.exists(dir))
    dir.create(dir)

  # if no check file name is provided use the remote file name
  if (is.na(check_file))
    check_file = file_name

  # if the check file is present then do nothing further
  if (file.exists(file.path(dir,check_file)))
    return()

  # download the file
  cat(paste0("downloading ",file_name,"... \n"))
  download.file(url = url, destfile = paste(dir,"\\",file_name, sep=""),mode="wb")


  # # if the file is a zip archive extract the contents and delete the archive
  # if (tools::file_ext(file_name)=="zip") {
  #   cat("extracting zip file...\n")
  #   unzip(file.path(dir,file_name),exdir=dir)
  #   if (tolower) {
  #     files = unzip(file.path(dir,file_name),list=T)
  #     for (file in files)
  #       file.rename(file.path(dir,file),file.path(dir,tolower(file)))
  #   }
  #   file.remove(file.path(dir,file_name))
  # }

}


####################################################
############   NATA cancer and respiratory risk data
####################################################

# directory to store the nata data
nata_dir = "data\\nata_data"

## Cancer Risk

# 2014(2017) nata file containing national cancer risks by toxic
#nata_file = "nata2014v2_national_cancerrisk_by_tract_poll.xlsx"
#nata_file = "national_cancerrisk_by_tract_poll.xlsx"
nata_file = "2019_National_CancerRisk_by_tract_poll.xlsx"

# # download the nata data if it doesn't already exist
# download_file("https://www.epa.gov/sites/production/files/2018-08/",
# download_file("https://www.epa.gov/system/files/other-files/2022-03/",
download_file("https://www.epa.gov/system/files/documents/2022-12/2019_National_CancerRisk_by_tract_poll.xlsx",
               nata_file,
               dir=nata_dir)


# # Respiratory
#nata_file_resp = "nata2014v2_national_resphi_by_tract_poll.xlsx"
#nata_file_resp =  "national_resphi_by_tract_poll.xlsx"
nata_file_resp =  "2019_National_RespHI_by_tract_poll.xlsx"

#download_file("https://www.epa.gov/sites/production/files/2022-03/",
download_file("https://www.epa.gov/system/files/documents/2022-12/2019_National_RespHI_by_tract_poll.xlsx",
              nata_file_resp,
              dir=nata_dir)

# load the nata data
nata_data = read_xlsx(file.path(nata_dir,nata_file)) %>%
  rename(total_risk=`Total Cancer Risk (per million)`)

nata_data_resp = read_xlsx(file.path(nata_dir,nata_file_resp)) %>%
  rename(total_risk_resp=`Total Respiratory (hazard quotient)`) %>%
  select(Tract, total_risk_resp)
