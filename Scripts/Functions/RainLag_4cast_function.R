#### Function to run AR model forecasts

library(tidyverse)


##start of function
generate_fDOM_RainLag_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
                                   forecast_horizon,
                                   n_members,
                                   output_folder,
                                   calibration_start_date,
                                   model_id,
                                   targets_df, # where are the targets you are forecasting?
                                   noaa_4cast,
                                   var, # what variable(s)?
                                   site, # what site(s),
                                   forecast_depths = 'focal',
                                   project_id = 'vera4cast') {
  

  if (site == 'fcre' & forecast_depths == 'focal') {
    forecast_depths <- 1.6
  }
  
  if (site == 'bvre' & forecast_depths == 'focal') {
    forecast_depths <- 1.5
  }
  #-------------------------------------
  
  # Get targets
  message('Getting targets')
  targets <- targets_df |>
    filter(variable %in% var,
           site_id %in% site,
           depth_m %in% forecast_depths,
           datetime <= forecast_date)
  #-------------------------------------
  
  # Get the weather data
  message('Getting weather')
  
  head(noaa_4cast)
  
  # split it into historic and future
  
  historic_weather <- noaa_4cast |> 
    filter(reference_datetime == datetime_date) |> #get data from just day of forecast issued
    group_by(reference_datetime, variable) |> 
    summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>  #get daily means (from ensembles) for each variable
    pivot_wider(names_from = variable, values_from = prediction) |> 
    filter(ymd(reference_datetime) < forecast_date
    ) |> 
    mutate(reference_datetime = as.Date(reference_datetime)) |> 
    rename(datetime = reference_datetime) |> 
    select(datetime, precipitation_flux)
  
  
  forecast_weather <- noaa_4cast |>
    filter(variable %in% c("precipitation_flux"),
           ymd(reference_datetime) == forecast_date)
  
  
  ###Trials for lag 

  #dummy data frame to hold prior 10 day values needed to get to ensembles 
  forecasthold <- data.frame(datetime_date = rep(seq((forecast_date-10), forecast_date+16, by = "1 day"), 31),
                             parameter = c(rep(0, 27), rep(1, 27), rep(2, 27), rep(3, 27), rep(4, 27), 
                                           rep(5, 27), rep(6, 27), rep(7, 27), rep(8, 27), rep(9, 27), rep(10, 27),
                                           rep(11, 27), rep(12, 27), rep(13, 27), rep(14, 27), rep(15, 27), rep(16, 27),
                                           rep(17, 27), rep(18, 27), rep(19, 27), rep(20, 27), rep(21, 27), rep(22, 27),
                                           rep(23, 27), rep(24, 27), rep(25, 27), rep(26, 27),
                                           rep(27, 27), rep(28, 27), rep(29, 27), rep(30, 27)
                                           )) |> 
    mutate(datetime_date = as.character(datetime_date))
    

  #mega pipe that gets historic weather 10 days before, bind and sets up 31 parameters, binds to noaa forecast so each forecast ensemble has enough historic data in 'parameter' to determine previous 10 days rain
  forecast_weather_withLag <-  historic_weather |> 
    filter(datetime >= forecast_date-10) |> 
    rename(datetime_date = datetime, 
           prediction = precipitation_flux) |> 
    mutate(datetime_date = as.character(datetime_date)) |> 
    mutate(reference_datetime = as.character(forecast_date)) |> 
    right_join(forecasthold, by = 'datetime_date') |> 
    mutate(reference_datetime = as.character(forecast_date)) |> 
    left_join(forecast_weather, by = c("datetime_date", "reference_datetime", "parameter")) |> 
    mutate(prediction = ifelse(is.na(prediction.x), prediction.y, prediction.x)) |> 
    group_by(reference_datetime, parameter) |> 
    mutate(rain10daysum = zoo::rollsum(prediction, 10, fill = NA, align = "right")) |> 
    select(datetime_date, reference_datetime, parameter, prediction, rain10daysum)
    
  
  
  #-------------------------------------
  
  
  # Fit model
  message('Fitting model')
  
  fit_df <- targets |>
    filter(datetime < forecast_date,
           datetime >= calibration_start_date ## THIS is the furthest date that we have all values for calibration
    ) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    left_join(historic_weather) |>
    mutate(#precip_lag1 = lag(precipitation_flux, 1),
           precip_10daysum = zoo::rollsum(precipitation_flux, 10, fill = NA, align = "right"))
  
  fdom_model <- lm(fit_df$fDOM_QSU_mean ~  fit_df$precip_10daysum )
  
  model_fit <- summary(fdom_model)
  
  coeffs <- model_fit$coefficients
  # params_se <- model_fit$coefficients[,2] 
  
  # #### get param uncertainty
  #get param distribtuions for parameter uncertainity
  # param_df <- data.frame(beta_int = rnorm(31, coeffs[1], params_se[1]),
  #                        beta_SW = rnorm(31, coeffs[2], params_se[2]),
  #                        beta_rain = rnorm(31, coeffs[3], params_se[3]),
  #                        beta_rainLag = rnorm(31, coeffs[4], params_se[4]))
  # )
  
  
  #-------------------------------------
  
  # Set up forecast data frame
  
  message('Make forecast dataframe')
  
  #establish forecasted dates
  forecasted_dates <- seq(from = ymd(forecast_date), to = ymd(forecast_date) + forecast_horizon, by = "day")
  

  #set up table to hold forecast output 
  forecast_full_unc <- tibble(date = rep(forecasted_dates, times = n_members),
                              ensemble_member = rep(1:n_members, each = length(forecasted_dates)),
                              reference_datetime = forecast_date,
                              Horizon = date - reference_datetime,
                              forecast_variable = var,
                              value = as.double(NA),
                              uc_type = "noaa") 

  
  #-------------------------------------
  
  message('Generating forecast')
  
  #for loop to run forecast 
  for(i in 2:length(forecasted_dates)) {
    
    #pull prediction dataframe for the relevant date
    fdom_pred <- forecast_full_unc %>%
      filter(date == forecasted_dates[i])
    
    #pull driver ensemble for the relevant date; here we are using all 31 NOAA ensemble member
    met_precip_lag_driv <- forecast_weather_withLag %>%
      filter(ymd(reference_datetime) == forecast_date) |> 
      filter(ymd(datetime_date) == forecasted_dates[i])
    

    #run model
    fdom_pred$value <- coeffs[1] + (met_precip_lag_driv$rain10daysum * coeffs[2]) 
    
    #insert values back into the forecast dataframe
    forecast_full_unc <- forecast_full_unc %>%
      rows_update(fdom_pred, by = c("date","ensemble_member","forecast_variable","uc_type"))
    
  } #end for loop
  
  #clean up file to match vera format 
  
  forecast_df <- forecast_full_unc |>
    rename(datetime = date,
           variable = forecast_variable,
           prediction = value,
           parameter = ensemble_member) |>
    mutate(family = 'ensemble',
           duration = "P1D",
           depth_m = forecast_depths,
           project_id = project_id,
           model_id = model_id,
           site_id = site
    ) |>
    select(datetime, reference_datetime, model_id, site_id,
           parameter, family, prediction, variable, depth_m) #, duration, project_id
  
  return(write.csv(forecast_df, file = paste0(output_folder, forecast_date, ".csv"), row.names = F))

  
}  ##### end function


########### TEst function #######
# forecast_date <- ymd("2024-07-06")
# model_id <- "fDOM_RainLagRegress_dwh"
# var <- "fDOM_QSU_mean"
# site <- "fcre"
# forecast_depths <- 1.6
# project_id <- "vera4cast"
# n_members <- 31
# forecast_horizon <- 16
# 
# calibration_start_date <- ymd("2020-09-27") #going back to start of noaa4casts since we're not limited by FLARE output
# 
# # targets_df <- readr::read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz", show_col_types = FALSE) |>
# #   filter(
# #     #datetime >= ymd("2022-01-01"),
# #     site_id == "fcre",
# #     depth_m == 1.6,
# #     variable %in% c("fDOM_QSU_mean"))
# targets_df <- targets_df
# 
# 
# #noaa_4cast <- read.csv("C:/Users/dwh18/OneDrive/Desktop/R_projects/fdom_4cast/Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_27sep20-12aug24.csv")
# noaa_4cast <- noaa_4cast
# 
# output_folder <- paste0("C:/Users/dwh18/Downloads/", model_id, "_", forecast_date, ".csv")
# 
# 
# # ##run function
# generate_fDOM_RainLab_forecast(forecast_date = forecast_date, forecast_horizon = forecast_horizon, n_members = n_members,
#                        output_folder = output_folder, model_id = model_id, targets_df = targets_df,
#                        noaa_4cast = noaa_4cast, var = var,site = site, forecast_depths = forecast_depths, 
#                        project_id = project_id,
#                       calibration_start_date = calibration_start_date )
# 
# 
# read.csv("C:/Users/dwh18/Downloads/fDOM_RainLagRegress_dwh_2024-07-06.csv2024-07-06.csv")|>
#   mutate(date = as.Date(datetime)) |>
#     # filter(forecast_date > ymd("2023-01-03")) |>
#   ggplot(aes(x = date, y = prediction, color = as.character(parameter)))+
#   geom_line()




