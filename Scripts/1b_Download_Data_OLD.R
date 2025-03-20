#### Downloaded needed targets, water temp hindcasts, and NOAA hindcasts from S3 buckets 

#packages 
library(tidyverse)
library(arrow)

#### FCR, BVR, and CCR fDOM target data  ------------------------------------------

#set p for temp corrections 
p <- -0.01

#### FCR Water Q data
fcr_L1 <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/refs/heads/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
fcr_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986" )
fcrfull <- rbind(fcr_edi, fcr_L1)

fcr_waterQ <- fcrfull |> 
  mutate(fdom_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) )   ) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fDOM_QSU_mean = mean(fdom_TC, na.rm = T)) |> 
  mutate(site_id = "fcre",
         depth_m = 1.6) |> 
  select(Date, site_id, depth_m, fDOM_QSU_mean) |> 
  rename(datetime = Date) |> 
  filter(datetime < ymd("2025-03-19")) #### SINCE march 19th was midday when run; run 19mar25 - april when updating


#### BVR Water Q data
bvr_L1 <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/refs/heads/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
bvr_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e" )
bvrfull <- rbind(bvr_edi, bvr_L1)

bvr_waterQ <- bvrfull |> 
  mutate(fdom_TC = EXOfDOM_QSU_1.5/(1 + (p*(EXOTemp_C_1.5 - 20)) )   ) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fDOM_QSU_mean = mean(fdom_TC, na.rm = T)) |> 
  mutate(site_id = "bvre",
         depth_m = 1.5) |> 
  select(Date, site_id, depth_m, fDOM_QSU_mean) |> 
  rename(datetime = Date) |> 
  filter(datetime < ymd("2025-03-19")) #### SINCE march 19th was midday when run; run 19mar25 - april when updating


#### CCR water Q data
ccr_L1 <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data-qaqc/ccre-waterquality_L1.csv")
ccr_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1069/3/4afb209b30ebed898334badd3819d854")
ccrfull <- rbind(ccr_edi, ccr_L1)

ccr_waterQ <- ccrfull |> 
  mutate(fdom_TC = EXOfDOM_QSU_1/(1 + (p*(EXOTemp_C_1 - 20)) )   ) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fDOM_QSU_mean = mean(fdom_TC, na.rm = T)) |> 
  mutate(site_id = "ccre",
         depth_m = 1.5) |> 
  select(Date, site_id, depth_m, fDOM_QSU_mean) |> 
  rename(datetime = Date) |> 
  filter(datetime < ymd("2025-03-19")) #### SINCE march 19th was midday when run; run 19mar25 - april when updating


####bind reservoirs together and write csv
res_fdom <- rbind(fcr_waterQ, bvr_waterQ, ccr_waterQ)

res_fdom_formated <- res_fdom |> 
  pivot_longer(-c(1:3), names_to = "variable", values_to = "observation") 
  
  
write.csv(res_fdom_formated, "Data/GeneratedData/Targets_fDOM_allReservoirs.csv", row.names = F)


#### NOAA weather forecasts for BVR and FCR ------------------------------------------

#Getting NOAA forecasts from 2020-09-30 to current day
new_met_bucket <-arrow::s3_bucket(file.path("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2/"),
                                  endpoint_override = 'renc.osn.xsede.org',
                                  anonymous = TRUE)

noaa_new_daily <- arrow::open_dataset(new_met_bucket) |>
  dplyr::filter(
    site_id == 'fcre',  #filtering by bvre returns the same forecasts
    variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_new_daily, "Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_30sep20-18mar25.csv", row.names = F)


#### FCR and BVR water temp forecasts  ------------------------------------------

##This gets water temp forecasts w/ reference datetimes from 2022-10-02 to current 

##FCR

#old forecasts up to 2024-02-18
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

fcr_df_flare_old_forbind <- fcr_df_flare_old |> 
  rename(datetime_date = datetime) |> 
  mutate(site_id = "fcre") |> 
  filter(parameter <= 31) |> 
  filter(as.Date(reference_datetime) > ymd("2022-11-07") ) |>  #remove odd date that has dates one month behind reference datetime
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)


## current water temp since 2024-02-18
fcr_new_flare_forecasts <- arrow::open_dataset("s3://anonymous@bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/duration=P1D/variable=Temp_C_mean?endpoint_override=renc.osn.xsede.org")
  
fcr_df_flare_newA <- fcr_new_flare_forecasts |>
    dplyr::filter(site_id %in% c("fcre"),
                  reference_datetime > ymd("2025-01-08"),
                  #model_id == "glm_aed_v1",
                  depth_m == 1.5) |>
    dplyr::rename(depth = depth_m) |> 
    dplyr::collect()

fcr_df_flare_new_NEW <- fcr_new_flare_forecasts |>
  dplyr::filter(model_id == "glm_aed_flare_v3",
                depth_m == 1.6) |>
  dplyr::rename(depth = depth_m) |>
  dplyr::collect()
  

fcr_df_flare_new_forbind <- fcr_df_flare_new |> 
  filter(reference_datetime > ymd_hms("2024-02-18 00:00:00")) |> 
  select(-reference_date) |> 
  mutate(variable = "temperature",
         reference_datetime = as.character(reference_datetime)) |> 
  rename(datetime_date = datetime) |> 
  mutate(parameter = as.numeric(parameter)) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)



## bind water temp forecasts together 
fcr_water_temp_4cast_data <- rbind(fcr_df_flare_old_forbind, fcr_df_flare_new_forbind) |>
  filter(parameter <= 31)

##horizon check
#z <- fcr_water_temp_4cast_data |> 
#mutate(Horizon = as.Date(datetime_date) - as.Date(reference_datetime)) |>
  #   group_by(reference_datetime) |> summarise(horizon = max(Horizon))

write.csv(fcr_water_temp_4cast_data, "Data/GeneratedData/FCR_FLARE_11nov22-11jan25.csv", row.names = F)


## BVR 

#old forecasts up to 2024-02-18
bvr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/site_id=bvre/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

bvr_df_flare_old <- arrow::open_dataset(bvr_backup_forecasts) |>
  filter(depth %in% c(1.5), #no 1.6
         variable == "temperature",
         parameter <= 31,
         model_id == "test_runS3" #other models for FCR, this is the only one for BVR in backups bucket
  ) |>
  dplyr::collect()

bvr_df_flare_old_forbind <- bvr_df_flare_old |> 
  rename(datetime_date = datetime) |> 
  mutate(site_id = "bvre") |> 
  filter(parameter <= 31) |> 
  filter(as.Date(reference_datetime) > ymd("2022-11-07") ) |>  #remove odd date that has dates one month behind reference datetime
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)


## current water temp since 2024-02-18
bvr_new_flare_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/forecasts/parquet/"),
                                    endpoint_override = 'renc.osn.xsede.org',
                                    anonymous = TRUE)

bvr_df_flare_new <- arrow::open_dataset(bvr_new_flare_forecasts) |>
  dplyr::filter(site_id %in% c("bvre"),
                model_id == 'glm_flare_v1',
                variable == 'temperature',
                depth == 1.5) |>
  dplyr::collect()

bvr_df_flare_new_forbind <- bvr_df_flare_new |> 
  filter(reference_datetime > ymd_hms("2024-02-18 00:00:00")) |> 
  select(-reference_date) |> 
  mutate(variable = "temperature",
         reference_datetime = as.character(reference_datetime)) |> 
  rename(datetime_date = datetime) |> 
  mutate(parameter = as.numeric(parameter)) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)


## bind water temp forecasts together 
bvr_water_temp_4cast_data <- rbind(bvr_df_flare_old_forbind, bvr_df_flare_new_forbind) |>
  filter(parameter <= 31)

##horizon check
# z <- bvr_water_temp_4cast_data |>
# mutate(Horizon = as.Date(datetime_date) - as.Date(reference_datetime)) |>
#   group_by(reference_datetime) |> summarise(horizon = max(Horizon))

write.csv(bvr_water_temp_4cast_data, "Data/GeneratedData/BVR_FLARE_11nov22-24jan25.csv", row.names = F)



#### CCR NOAA forecast ------------------------------------------

noaa_df <- (arrow::s3_bucket('bio230121-bucket01/flare/drivers/met/gefs-v12/stage2', endpoint_override = 'renc.osn.xsede.org', anonymous = TRUE))

noaa_df_ccr <-  arrow::open_dataset(noaa_df) |>
  filter(site_id == "ccre",
         variable %in% c("precipitation_flux", "surface_downwelling_shortwave_flux_in_air")) |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_df_ccr, "Data/GeneratedData/CCR_NOAA_stage2_dailyaverage_27sep20-18mar25.csv", row.names = F)


#### CCR water temp forecasts ------------------------------------------

#reforecasts ADD ran from 2022-11-20 to 2023-01-02; looks like may just be one reference day though
ccre_reforecast <- arrow::s3_bucket(file.path("bio230121-bucket01/ccre-reforecast/forecasts/parquet/"),
                                    endpoint_override = 'renc.osn.xsede.org',
                                    anonymous = TRUE)

ccre_reforecast_df <- arrow::open_dataset(ccre_reforecast) |> 
  filter(site_id == 'ccre', 
         model_id == 'glm_flare_reforecast',
         variable == 'temperature',
         depth == 1.5) |> 
  collect()

ccre_reforecast_forbind <- ccre_reforecast_df |> 
  mutate(datetime_date = as.Date(datetime)) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)



#backup bucket forecasts: 2022-12-01 to 2024-02-18
ccr_backup_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/vt_backup/forecasts/parquet/site_id=ccre/"),
                                         endpoint_override = 'renc.osn.xsede.org',
                                         anonymous = TRUE)

ccr_df_flare_old <- arrow::open_dataset(ccr_backup_forecasts) |>
  filter(depth %in% c(1.5), 
         variable == "temperature",
         parameter <= 31
  ) |>  
  dplyr::collect()

ccr_df_flare_old_forbind <- ccr_df_flare_old |> 
  mutate(datetime_date = as.Date(datetime),
         site_id = 'ccre') |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)


#newer forecasts to present
ccr_new_flare_forecasts <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/forecasts/parquet/"),
                                            endpoint_override = 'renc.osn.xsede.org',
                                            anonymous = TRUE)

ccr_df_flare_new <- arrow::open_dataset(ccr_new_flare_forecasts) |>
  dplyr::filter(site_id %in% c("ccre"),
                model_id == 'glm_flare_v1',
                variable == 'temperature',
                depth == 1.5) |>
  dplyr::collect()

ccr_df_flare_new_forbind <- ccr_df_flare_new |> 
  filter(reference_datetime > ymd_hms("2024-02-18 00:00:00")) |> 
  select(-reference_date) |> 
  mutate(variable = "temperature",
         reference_datetime = as.character(reference_datetime)) |> 
  rename(datetime_date = datetime) |> 
  mutate(parameter = as.numeric(parameter)) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id)


##bind forecasts together
ccr_water_temp_4cast_data <- rbind(ccre_reforecast_forbind, ccr_df_flare_old_forbind, ccr_df_flare_new_forbind) |>
  filter(parameter <= 31)

##horizon check
# z <- ccr_water_temp_4cast_data |>
# mutate(Horizon = as.Date(datetime_date) - as.Date(reference_datetime)) |>
#   group_by(reference_datetime) |> summarise(horizon = max(Horizon))

write.csv(ccr_water_temp_4cast_data, "Data/GeneratedData/CCR_FLARE_1jan23-24jan25.csv", row.names = F)









