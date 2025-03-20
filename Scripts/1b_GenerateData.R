#### Downloaded needed targets, water temp hindcasts, and NOAA hindcasts from S3 buckets 

#packages 
library(tidyverse)
library(arrow)

#### NOTE TO READ
#NOAA and water temp forecasts parquet files will need to be download to recreate making the generated data
#You can just copy the 'GeneratedData' folder from zenodo to skip this script 
#Or you can download the whole zotero folder and recreate making the csvs in the 'GeneratedData' folder
#In code below you will just have to update the file path in lines w/ 'open_dataset("D:/....)' to the folder 
#that zenodo data was download.



#### NOAA weather forecasts for BVR and FCR ------------------------------------------

#Getting NOAA forecasts for FCR from 2020-09-30 to 2025-03-19
#open dataset from stage2 data parquet and summarize to daily data need for models and forecasts
noaa_fcr <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/noaa/gefs-v12-reprocess/stage2/fcr/") |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(site_id, reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_fcr, "Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_27sep20-19mar25.csv", row.names = F)


#### NOAA weather forecasts for CCR ------------------------------------------

#Getting NOAA forecasts for CCR from 2020-09-30 to 2025-03-19
#open dataset from stage2 data parquet and summarize to daily data need for models and forecasts
noaa_ccr <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/noaa/gefs-v12-reprocess/stage2/ccr/") |>
  mutate(datetime_date = as.Date(datetime)) |>
  group_by(site_id, reference_datetime, datetime_date, variable, parameter) |>
  summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
  dplyr::collect()

write.csv(noaa_ccr, "Data/GeneratedData/CCR_NOAA_stage2_dailyaverage_27sep20-19mar25.csv", row.names = F)



#### FCR water temp 4casts ----------------------------------------------------

## old forecasts from 2022-11-13 to 2024-02-18 (ref date)
fcr_flare_old <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/fcr/backups/") |>
  rename(datetime_date = datetime) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |>
  mutate(reference_datetime = ymd_hms(reference_datetime)) |> 
  mutate(reference_datetime = as.Date(reference_datetime)) |> 
  mutate(reference_datetime = as.character(reference_datetime)) |> 
  collect()

## new forecasts w/ aedV1 model from 2024-03-01 to 2025-01-10 (for ref dates)
fcr_flare_new_aedv1 <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/fcr/new/glm_aed_v1/") |>
  mutate(parameter = as.numeric(parameter)) |> 
  filter(parameter <= 31,
         reference_datetime > ymd_hms("2024-02-18 00:00:00")) |> 
  select(-reference_datetime) |> 
  dplyr::rename(depth = depth_m) |> 
  mutate(variable = "temperature",
         reference_date = as.character(reference_date)) |> 
  rename(datetime_date = datetime,
         reference_datetime = reference_date) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |> 
  collect()

## new forecasts w/ aedV3 model from 2025-01-11 to 2025-03-19 (for ref dates)
fcr_flare_new_aedv3 <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/fcr/new/glm_aed_flare_v3/") |>
  mutate(parameter = as.numeric(parameter)) |> 
  filter(parameter <= 31,
         reference_datetime > ymd_hms("2025-01-10 00:00:00")) |>
  select(-reference_datetime) |> 
  dplyr::rename(depth = depth_m) |> 
  mutate(variable = "temperature",
         reference_date = as.character(reference_date)) |> 
  rename(datetime_date = datetime,
         reference_datetime = reference_date) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |>
  mutate(depth = 1.5) |> 
  collect()

## bind forecasts together and write csv
fcr_water_temp_4cast_data <- rbind(fcr_flare_old, fcr_flare_new_aedv1, fcr_flare_new_aedv3) 
  
write.csv(fcr_water_temp_4cast_data, "Data/GeneratedData/FCR_FLARE_11nov22-19mar25.csv", row.names = F)



#### BVR water temp 4casts ----------------------------------------------------

#old forecasts from 2022-11-08 to 2024-02-18 (for ref dates)
bvr_flare_old <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/bvr/backups/") |> 
  rename(datetime_date = datetime) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |>
  mutate(reference_datetime = ymd_hms(reference_datetime)) |> 
  mutate(reference_datetime = as.Date(reference_datetime)) |> 
  mutate(reference_datetime = as.character(reference_datetime)) |> 
  collect()
  

## new forecasts w/ aedV1 model from 2024-03-01 to 2025-03-20 (for ref dates)
bvr_flare_new_aedv1 <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/bvr/new/glm_flare_v1/") |>
  mutate(parameter = as.numeric(parameter)) |> 
  filter(parameter <= 31,
         reference_datetime > ymd_hms("2024-02-18 00:00:00")) |> 
  select(-reference_datetime) |> 
  mutate(variable = "temperature",
         reference_date = as.character(reference_date)) |> 
  rename(datetime_date = datetime,
         reference_datetime = reference_date) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |> 
  collect()

## bind forecasts together and write csv
bvr_water_temp_4cast_data <- rbind(bvr_flare_old, bvr_flare_new_aedv1) 

write.csv(bvr_water_temp_4cast_data, "Data/GeneratedData/BVR_FLARE_8nov22-20mar25.csv", row.names = F)



#### CCR water temp 4casts ----------------------------------------------------

#old forecasts from 2023-01-02 to 2024-02-18 (for ref dates)
ccr_flare_old <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/ccr/backups/") |> 
  rename(datetime_date = datetime) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |>
  mutate(reference_datetime = ymd_hms(reference_datetime)) |> 
  mutate(reference_datetime = as.Date(reference_datetime)) |> 
  mutate(reference_datetime = as.character(reference_datetime)) |> 
  collect()

## new forecasts w/ aedV1 model from 2024-02-24 to 2025-03-20 (for ref dates)
ccr_flare_new_aedv1 <- open_dataset("D:/fDOM_4cast_zenodo/archive/drivers/flare/ccr/new/glm_flare_v1/") |>
  mutate(parameter = as.numeric(parameter)) |> 
  filter(parameter <= 31,
         reference_datetime > ymd_hms("2024-02-18 00:00:00")) |>
  select(-reference_datetime) |> 
  mutate(variable = "temperature",
         reference_date = as.character(reference_date)) |> 
  rename(datetime_date = datetime,
         reference_datetime = reference_date) |> 
  select(reference_datetime, datetime_date, site_id, depth, family, parameter, variable, prediction, model_id) |> 
  collect()

## bind forecasts together and write csv
ccr_water_temp_4cast_data <- rbind(ccr_flare_old, ccr_flare_new_aedv1) 

write.csv(ccr_water_temp_4cast_data, "Data/GeneratedData/CCR_FLARE_2jan23-20mar25.csv", row.names = F)



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








