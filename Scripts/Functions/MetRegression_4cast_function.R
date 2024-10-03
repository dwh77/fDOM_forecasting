#### Function to run fDOM meteorology regression forecasts

library(tidyverse)


##start of function
generate_fDOM_regress_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
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
    rename(datetime = reference_datetime)
  
  
  forecast_weather <- noaa_4cast |>
    filter(ymd(reference_datetime) == forecast_date) 
  
  
  #-------------------------------------
  
  
  # Fit model
  message('Fitting model')
  
  fit_df <- targets |>
    filter(datetime < forecast_date,
           datetime >= calibration_start_date ## THIS is the furthest date that we have all values for calibration
    ) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    left_join(historic_weather) |>
    mutate(precip_lag1 = lag(precipitation_flux, 1))
  
  fdom_model <- lm(fit_df$fDOM_QSU_mean ~ fit_df$surface_downwelling_shortwave_flux_in_air +
                     fit_df$precipitation_flux + fit_df$precip_lag1 )
  
  model_fit <- summary(fdom_model)
  
  coeffs <- model_fit$coefficients
  params_se <- model_fit$coefficients[,2] 
  
  #### get param uncertainty
  ##get param distribtuions for parameter uncertainity
   param_df <- data.frame(beta_int = rnorm(31, coeffs[1], params_se[1]),
                          beta_SW = rnorm(31, coeffs[2], params_se[2]),
                          beta_rain = rnorm(31, coeffs[3], params_se[3]),
                          beta_rainLag = rnorm(31, coeffs[4], params_se[4])
   )
   
   ####get process uncertainty
   #find residuals
   fit_df_noNA <- na.omit(fit_df)
   mod <- predict(fdom_model, data = fit_df_noNA)
   residuals <- mod - fit_df_noNA$fDOM_QSU_mean
   sigma <- sd(residuals, na.rm = TRUE) # Process Uncertainty Noise Std Dev.; this is your sigma
  
  
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
                              uc_type = "total") 

  
  #-------------------------------------
  
  message('Generating forecast')
  
  #for loop to run forecast 
  for(i in 2:length(forecasted_dates)) {
    
    #pull prediction dataframe for the relevant date
    fdom_pred <- forecast_full_unc %>%
      filter(date == forecasted_dates[i])
    
    #pull driver ensemble for the relevant date; here we are using all 31 NOAA ensemble members
    met_sw_driv <- forecast_weather %>%
      filter(variable == "surface_downwelling_shortwave_flux_in_air") |> 
      filter(ymd(reference_datetime) == forecast_date) |> 
      filter(ymd(datetime_date) == forecasted_dates[i])
    
    met_precip_driv <- forecast_weather %>%
      filter(variable == "precipitation_flux") |> 
      filter(ymd(reference_datetime) == forecast_date) |> 
      filter(ymd(datetime_date) == forecasted_dates[i])
    
    met_precip_lag_driv <- forecast_weather %>%
      filter(variable == "precipitation_flux") |> 
      filter(ymd(reference_datetime) == forecast_date) |> 
      filter(ymd(datetime_date) == forecasted_dates[i-1])
    

    #run model
    fdom_pred$value <- param_df$beta_int + (met_sw_driv$prediction *  param_df$beta_SW) + 
      (met_precip_driv$prediction * param_df$beta_rain) + 
      (met_precip_lag_driv$prediction * param_df$beta_rainLag) +
      rnorm(n = 31, mean = 0, sd = sigma) #process uncert
    
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
           parameter, family, prediction, variable, depth_m) #removing  'duration, project_id' to save csv space
  
  return(write.csv(forecast_df, file = paste0(output_folder, forecast_date, ".csv"), row.names = F))

  
}  ##### end function


########### Test function #######
##uncomment all lines below and run to run an example 4cast for one day 

# forecast_date <- ymd("2024-05-06")
# model_id <- "fDOM_MetRegression_dwh"
# var <- "fDOM_QSU_mean"
# site <- "fcre"
# forecast_depths <- 1.6
# project_id <- "vera4cast"
# n_members <- 31
# forecast_horizon <- 16
# 
# calibration_start_date <- ymd("2020-09-27") #going back to start of noaa4casts since we're not limited by FLARE output
# 
# targets_fdom <- read_csv("./Data/GeneratedData/Targets_fDOM_allReservoirs.csv")
# targets_df <- targets_fdom |>
#   filter(site_id == "fcre")
# 
# 
# noaa_4cast <- read.csv("./Data/GeneratedData/FCR_NOAA_stage2_dailyaverage_27sep20-12aug24.csv")
# 
# # update for local folder
# output_folder <- paste0("C:/Users/dwh18/Downloads/", model_id, "_")
# 
# 
# # ##run function
# generate_fDOM_regress_forecast(forecast_date = forecast_date, forecast_horizon = forecast_horizon, n_members = n_members,
#                        output_folder = output_folder, model_id = model_id, targets_df = targets_df,
#                        noaa_4cast = noaa_4cast, var = var,site = site, forecast_depths = forecast_depths,
#                        project_id = project_id,
#                       calibration_start_date = calibration_start_date )
# 
# 
# read.csv("C:/Users/dwh18/Downloads/fDOM_MetRegression_dwh_2024-05-06.csv")|>
#   mutate(date = as.Date(datetime)) |>
#   ggplot(aes(x = date, y = prediction, color = as.character(parameter)))+
#   geom_line()




