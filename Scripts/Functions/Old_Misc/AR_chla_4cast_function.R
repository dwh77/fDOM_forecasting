#### Function to run AR model forecasts

## helper functions
##function to pull current value 
current_value <- function(dataframe, variable, start_date){
  
  value <- dataframe |> 
    mutate(datetime = as.Date(datetime)) |> 
    filter(datetime == start_date,
           variable == variable) |> 
    pull(observation)
  
  return(value)
}

#function to generate 30 ensembles of fDOM IC based on standard deviation arround current observation
get_IC_uncert <- function(curr_fdom, n_members, ic_sd = 0.1){
  rnorm(n = n_members, mean = curr_fdom, sd = ic_sd)
}

## main fuction

generate_fDOM_forecast <- function(forecast_date, # a recommended argument so you can pass the date to the function
                                   forecast_horizon,
                                   n_members,
                                   output_folder,
                                   calibration_start_date,
                                   model_id,
                                   targets_url, # where are the targets you are forecasting?
                                   water_temp_4cast_data,
                                   noaa_4cast,
                                   # water_temp_4cast_url, #get url for water temp used as covariate
                                   # weather_forecast,
                                   var, # what variable(s)?
                                   site, # what site(s),
                                   forecast_depths = 'focal',
                                   project_id = 'vera4cast') {
  
  # Put your forecast generating code in here, and add/remove arguments as needed.
  # Forecast date should not be hard coded
  # This is an example function that also grabs weather forecast information to be used as co-variates
  
  if (site == 'fcre' & forecast_depths == 'focal') {
    forecast_depths <- 1.6
  }
  
  if (site == 'bvre' & forecast_depths == 'focal') {
    forecast_depths <- 1.5
  }
  #-------------------------------------
  
  # Get targets
  message('Getting targets')
  targets <- readr::read_csv(targets_url, show_col_types = F) |>
    filter(variable %in% c(var, "Chla_ugL_mean"), 
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
  
  #Get water temp forecasts
  message('Getting water temp 4casts')
  
  head(water_temp_4cast_data)
  
  
  # split it into historic and future
  historic_watertemp <- water_temp_4cast_data |>
    filter(as.Date(datetime_date) == as.Date(reference_datetime)) |> 
    # calculate a daily mean (remove ensemble)
    group_by(reference_datetime, variable) |>
    summarise(prediction = mean(prediction, na.rm = T), .groups = "drop") |>
    pivot_wider(names_from = variable, values_from = prediction) |> 
    filter(as.Date(reference_datetime) < forecast_date
    ) |> 
    mutate(reference_datetime = as.Date(reference_datetime)) |> 
    rename(datetime = reference_datetime)
  
  
  forecast_watertemp <- water_temp_4cast_data |>
    filter(as.Date(reference_datetime) == forecast_date) 
  
  
  #-------------------------------------
  
  
  
  # Fit model
  message('Fitting model')
  
  fit_df <- targets |>
    filter(datetime < forecast_date,
           datetime >= calibration_start_date ## THIS is the furthest date that we have all values for calibration
    ) |>
    pivot_wider(names_from = variable, values_from = observation) |>
    left_join(historic_weather) |>
    left_join(historic_watertemp) |>
    mutate(fDOM_lag1 = lag(fDOM_QSU_mean, 1),
           Chla_lag1 = lag(Chla_ugL_mean, 1),
           precip_lag1 = lag(precipitation_flux, 1))
  
  fdom_model <- lm(fit_df$fDOM_QSU_mean ~ fit_df$fDOM_lag1 + fit_df$surface_downwelling_shortwave_flux_in_air +
                     fit_df$precipitation_flux + fit_df$precip_lag1 + fit_df$temperature + fit_df$Chla_lag1)
  
  model_fit <- summary(fdom_model)
  
  coeffs <- model_fit$coefficients[,1]
  params_se <- model_fit$coefficients[,2] 
  
  # #### get param uncertainty
  #get param distribtuions for parameter uncertainity
  param_df <- data.frame(beta_int = rnorm(31, coeffs[1], params_se[1]),
                         beta_fdomLag = rnorm(31, coeffs[2], params_se[2]),
                         beta_SW = rnorm(31, coeffs[3], params_se[3]),
                         beta_rain = rnorm(31, coeffs[4], params_se[4]),
                         beta_rainLag = rnorm(31, coeffs[5], params_se[5]),
                         beta_temp = rnorm(31, coeffs[6], params_se[6]),
                         beta_chla = rnorm(31, coeffs[7], params_se[7])
  )
  
  
  
  ####get process uncertainty
  #find residuals
  fit_df_noNA <- na.omit(fit_df)
  mod <- predict(fdom_model, data = fit_df_noNA)
  residuals <- mod - fit_df_noNA$fDOM_QSU_mean
  sigma <- sd(residuals, na.rm = TRUE) # Process Uncertainty Noise Std Dev.; this is your sigma
  
  
  ####look at set up for IC uncert 
  ic_sd <- 0.1 #adpating from HLWs temp 4cast using 0.1 and detection limit on fDOM sensor being 0.07 QSU
  # ic_uc <- rnorm(n = 30, mean = mean(fcr_fdom_2023$observation, na.rm = T), sd = ic_sd)
  # hist(ic_uc)
  
  # param_df$sigma <- sigma
  # param_df$ic_sd <- ic_sd
  
  # return(param_df)
  
  #-------------------------------------
  
  # Set up forecast data frame
  
  message('Make forecast dataframe')
  
  #establish forecasted dates
  forecasted_dates <- seq(from = ymd(forecast_date), to = ymd(forecast_date) + forecast_horizon, by = "day")
  
  #get current fdom value
  curr_fdom <- current_value(dataframe = targets,variable = var, start_date = forecast_date)
  
  #set up df of different initial conditions for IC uncert
  ic_df <- tibble(date = rep(as.Date(forecast_date), times = n_members),
                  ensemble_member = c(1:n_members),
                  forecast_variable = var,
                  value = get_IC_uncert(curr_fdom, n_members, ic_sd = 0.1),
                  uc_type = "total")
  
  #set up table to hold forecast output 
  forecast_full_unc <- tibble(date = rep(forecasted_dates, times = n_members),
                              ensemble_member = rep(1:n_members, each = length(forecasted_dates)),
                              reference_datetime = forecast_date,
                              Horizon = date - reference_datetime,
                              forecast_variable = var,
                              value = as.double(NA),
                              uc_type = "total") |> 
    rows_update(ic_df, by = c("date","ensemble_member","forecast_variable", "uc_type")) # adding IC uncert
  
  
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
    
    flare_driv <- forecast_watertemp %>%
      filter(as.Date(reference_datetime) == forecast_date) |> 
      filter(ymd(datetime_date) == forecasted_dates[i])
    
    #pull lagged fdom values
    fdom_lag <- forecast_full_unc %>%
      filter(date == forecasted_dates[i-1])
    
    #pull lagged chla values
    chla_lag <- targets |>
      filter(variable == "Chla_ugL_mean") |> 
      filter(ymd(datetime) == forecasted_dates[i-1]) 
    
    #run model
    fdom_pred$value <- param_df$beta_int + (fdom_lag$value * param_df$beta_fdomLag)  +
      (met_sw_driv$prediction * param_df$beta_SW) + (met_precip_driv$prediction * param_df$beta_rain) + 
      (met_precip_lag_driv$prediction * param_df$beta_rainLag) + (flare_driv$prediction * param_df$beta_temp) +
      (chla_lag$value * param_df$beta_chla) + ##################BREAKS SINCE LAG IS ONLY GOOD FOR 1st FORECAST DAY
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
           parameter, family, prediction, variable, depth_m,
           duration, project_id)
  
  return(write.csv(forecast_df, file = output_folder, row.names = F))
  # return(write.csv(forecast_df, file = paste0("C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/ASLO_talk_forecast_output/", output_folder, "/forecast_full_unc_", forecast_date, '.csv'), row.names = F))
  
  
}  ##### end function


########### TEst function #######
forecast_date <- ymd("2023-04-24")
model_id <- "example_fDOM_AR_dwh"
targets_url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
var <- "fDOM_QSU_mean"
site <- "fcre"
forecast_depths <- 1.6
project_id <- "vera4cast"
calibration_start_date <- ymd("2022-11-11")

water_temp_4cast_data <- fcr_flare
noaa_4cast <- noaa_daily

n_members <- 31
forecast_horizon <- 16
# output_folder <- "z"
output_folder <- paste0("C:/Users/dwh18/Downloads/", model_id, "_", forecast_date, ".csv")



##run function
generate_fDOM_forecast(forecast_date = forecast_date, forecast_horizon = forecast_horizon, n_members = n_members,
                       output_folder = output_folder, model_id = model_id, targets_url = targets_url,
                       water_temp_4cast_data = water_temp_4cast_data, noaa_4cast = noaa_4cast, var = var,
                      site = site, forecast_depths = forecast_depths, project_id = project_id,
                      calibration_start_date = calibration_start_date )


read.csv("C:/Users/dwh18/Downloads/example_fDOM_AR_dwh_2023-04-24.csv")|>
  mutate(date = as.Date(datetime)) |>
    # filter(forecast_date > ymd("2023-01-03")) |>
  ggplot(aes(x = date, y = prediction, color = as.character(parameter)))+
  geom_line()




