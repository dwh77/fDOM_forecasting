#### Download S3 data to local folders to set up zenodo 

library(arrow)
library(tidyverse)
library(aws.s3)

#set directory to write files too: using an external drive for more storage
lake_directory <- "D:/fDOM_4cast_zenodo"

#### NOAA stage 2 ------------------------------------------------

noaa_df <- (arrow::s3_bucket('bio230121-bucket01/flare/drivers/met/gefs-v12/stage2', endpoint_override = 'renc.osn.xsede.org', anonymous = TRUE))

##can use this line to start exploring sub directories
#open_dataset(noaa_df) #will show what the columns are
#noaa_df$ls()
# noaa_df$ls("reference_datetime=2023-06-22")
# noaa_df$ls("reference_datetime=2023-06-22/site_id=fcre")

## CCR
noaa_df_ccr <-  arrow::open_dataset(noaa_df) |>
  filter(site_id == "ccre",
         variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) 
 
write_dataset(noaa_df_ccr, path = file.path(lake_directory, "archive/drivers/noaa/gefs-v12-reprocess/stage2/ccr/"))


## FCR
noaa_df_fcr <-  arrow::open_dataset(noaa_df) |>
  filter(site_id == "fcre",
         variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) 

write_dataset(noaa_df_fcr, path = file.path(lake_directory, "archive/drivers/noaa/gefs-v12-reprocess/stage2/fcr/"))


#### FCR water temp 4casts ------------------------------------------------

##old forecasts up to 2024-02-18
fcr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

fcr_df_flare_old <- arrow::open_dataset(fcr_backup_forecasts) |>
  filter(site_id == "fcre",
         depth %in% c(1.5), #no 1.6
         variable == "temperature",
         parameter <= 31,
         model_id == "test_runS3") #other models for FCR, this is the only one for BVR in backups bucket

write_dataset(fcr_df_flare_old, path = file.path(lake_directory, "archive/drivers/flare/fcr/backups/"))


## current water temp 4casts
fcr_new_flare_forecasts <- arrow::open_dataset("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=Temp_C_mean?endpoint_override=renc.osn.xsede.org")

#through reference data of ~2025-01-10
fcr_df_flare_newA <- fcr_new_flare_forecasts |>
  dplyr::filter(site_id %in% c("fcre"),
                model_id == "glm_aed_v1",
                depth_m == 1.5) 

write_dataset(fcr_df_flare_newA, path = file.path(lake_directory, "archive/drivers/flare/fcr/new/glm_aed_v1/"))


#updated model to get forecast after ~2025-01-10
fcr_df_flare_newB <- fcr_new_flare_forecasts |>
  dplyr::filter(site_id %in% c("fcre"),
                model_id == "glm_aed_flare_v3",
                depth_m == 1.6) 

write_dataset(fcr_df_flare_newB, path = file.path(lake_directory, "archive/drivers/flare/fcr/new/glm_aed_flare_v3/"))



#### BVR water temp 4casts ------------------------------------------------

#old forecasts up to 2024-02-18
bvr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

bvr_df_flare_old <- arrow::open_dataset(bvr_backup_forecasts) |>
  filter(site_id == "bvre",
         depth %in% c(1.5), #no 1.6
         variable == "temperature",
         parameter <= 31,
         model_id == "test_runS3") 

write_dataset(bvr_df_flare_old, path = file.path(lake_directory, "archive/drivers/flare/bvr/backups/"))


## current water temp since 2024-03-01
bvr_new_flare_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/forecasts/parquet/"),
                                            endpoint_override = 'renc.osn.xsede.org',
                                            anonymous = TRUE)

bvr_df_flare_new <- arrow::open_dataset(bvr_new_flare_forecasts) |>
  dplyr::filter(site_id %in% c("bvre"),
                model_id == 'glm_flare_v1',
                variable == 'temperature',
                depth == 1.5) 

write_dataset(bvr_df_flare_new, path = file.path(lake_directory, "archive/drivers/flare/bvr/new/glm_flare_v1/"))



#### CCR water temp 4casts ------------------------------------------------

#old forecasts up to 2024-02-18
ccr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

ccr_df_flare_old <- arrow::open_dataset(ccr_backup_forecasts) |>
  filter(site_id == "ccre",
         depth %in% c(1.5), 
         variable == "temperature",
         parameter <= 31) 

write_dataset(ccr_df_flare_old, path = file.path(lake_directory, "archive/drivers/flare/ccr/backups/"))


## current water temp since 2024-03-01
ccr_new_flare_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/forecasts/parquet/"),
                                            endpoint_override = 'renc.osn.xsede.org',
                                            anonymous = TRUE)

ccr_df_flare_new <- arrow::open_dataset(ccr_new_flare_forecasts) |>
  dplyr::filter(site_id %in% c("ccre"),
                model_id == 'glm_flare_v1',
                variable == 'temperature',
                depth == 1.5) 

write_dataset(ccr_df_flare_new, path = file.path(lake_directory, "archive/drivers/flare/ccr/new/glm_flare_v1/"))

   
