#### Downloaded hindcasts that are to large to reload interatively from S3 buckets 

#packages 
library(tidyverse)


#### NOAA weather forecasts for BVR and FCR ####

#Getting NOAA forecasts from 2020-09-25 to 2024-02-18
old_met_bucket <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/drivers/noaa/gefs-v12-reprocess/stage2/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

noaa_old_daily <- arrow::open_dataset(old_met_bucket) |>
  dplyr::filter(
       site_id == 'fcre',  #filtering by bvre returns the same forecasts
       variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_old_daily, "C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_25sep20-18feb24.csv", row.names = F)

# noaa_daily <- read.csv("../Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_25sep20-18feb24.csv")
# # noaa_dailyz <- noaa_daily |> 
# #   filter(parameter <= 30)


#### FCR and BVR water temp forecasts ####

##This gets water temp forecasts w/ reference datetimes from 2022-10-02 to 2024-02-18

##FCR
fcr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/site_id=fcre/"),
                                  endpoint_override = 'renc.osn.xsede.org',
                                  anonymous = TRUE)

fcr_df_flare_old <- arrow::open_dataset(fcr_backup_forecasts) |>
  filter(depth %in% c(1.5), #no 1.6
         variable == "temperature",
         parameter <= 31,
         model_id == "test_runS3" #other models for FCR, this is the only one for BVR in backups bucket
         ) |>
  dplyr::collect()

write.csv(fcr_df_flare_old, "C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/GeneratedData/FCR_FLARE_7nov22-18feb24.csv", row.names = F)

# fcr_flare <- read.csv("../Data/GeneratedData/FCR_FLARE_7nov22-18feb24.csv")
# 
# fcr_flare <- fcr_flare |> 
#   rename(datetime_date = datetime) |> 
#   filter(parameter <= 31) |> 
#   filter(as.Date(reference_datetime) > ymd("2022-11-07") ) #remove odd date that has dates one month behind reference datetime


## BVR 
bvr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/site_id=bvre/"),
                                  endpoint_override = 'renc.osn.xsede.org',
                                  anonymous = TRUE)

bvr_df_flare_old <- arrow::open_dataset(bvr_backup_forecasts) |>
  filter(depth %in% c(1.5),
         variable == "temperature",
         parameter <= 31
         ) |>
  dplyr::collect()


write.csv(bvr_df_flare_old, "C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/GeneratedData/BVR_FLARE_7nov22-18feb24.csv", row.names = F)

# bvr_flare <- read.csv("../Data/GeneratedData/BVR_FLARE_7nov22-18feb24.csv")
# 
# bvr_flare <- bvr_flare |> 
#   rename(datetime_date = datetime) |> 
#   filter(parameter <= 31) |> 
#   filter(as.Date(reference_datetime) > ymd("2022-11-07") ) #remove odd date that has dates one month behind reference datetime


#### CCR NOAA forecast ####

noaa_df <- (arrow::s3_bucket('bio230121-bucket01/flare/drivers/met/gefs-v12/stage2', endpoint_override = 'renc.osn.xsede.org', anonymous = TRUE))

noaa_df_ccr <-  arrow::open_dataset(noaa_df) |>
  filter(site_id == "ccre",
         variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_df_ccr, "C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/GeneratedData/CCR_NOAA_stage2_dailyaverage_29sep20-27jun24.csv", row.names = F)


#### CCR water temp forecasts ####
ccr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/site_id=ccre/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

ccr_df_flare_old <- arrow::open_dataset(ccr_backup_forecasts) |>
  filter(depth %in% c(1.5), 
         variable == "temperature",
         parameter <= 31
  ) |>  
  dplyr::collect()

write.csv(ccr_df_flare_old, "C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/GeneratedData/CCR_FLARE_1jan23-18feb24.csv", row.names = F)



















