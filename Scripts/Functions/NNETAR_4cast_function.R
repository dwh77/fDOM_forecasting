####Generate NNETAR fDOM forecasts
#adapting MEL and ADD github code
#Code heavily adapted from MEL github, but copied locally to avoid changes when pulling from github
#https://github.com/addelany/vera4casts/blob/main/code/combined_workflow/nnetar_workflow.R

# #fableNNETAR
# devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/predict/fableNNETAR.R")
# #format_data_NNETAR
# devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/format_data/format_data_NNETAR.R")
# #interpolate
# devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/format_data/interpolate.R")


#### helper functions ----

library(tidyverse)
library(lubridate)
library(zoo)
library(fable)
library(feasts)
library(urca)


#### interpolate ----
interpolate <- function(vec){
  
  #replace missing values at beginning of timeseries
  vec[cumall(is.na(vec))] <- as.double(vec[min(which(!is.na(vec)))])
  
  #create interpolated timeseries
  interp <- na.approx(vec)
  
  #fill in missing values at end of timeseries
  if(length(interp) < length(vec)){
    num_NA = length(vec) - length(interp)
    nas <- rep(NA, times = num_NA)
    interp2 = c(interp, nas)
    interp3 <- na.locf(interp2)
    out <- interp3
  } else {
    out <- interp
  }
  
  return(out)
  
}

#### format data ----
format_data_NNETAR <- function(targets, end_date){
  
  #read in targets 
  dat <- targets %>%
    filter(variable == "fDOM_QSU_mean") %>%
    arrange(site_id, datetime)
  
  sites <- unique(dat$site_id)
  
  start_dates <- dat %>%
    group_by(site_id) %>%
    filter(!is.na(observation)) %>%
    slice(1) %>%
    pull(datetime)
  
  #get list of dates
  end_date = as.Date(end_date)
  daily_dates_df = tibble(datetime = Date(length = 0L), 
                          site_id = character(length = 0L))
  for(i in 1:length(start_dates)){
    temp_dates <- seq.Date(from = as.Date(start_dates[i]), to = as.Date(end_date), by = "day")
    temp_sites <- rep(sites[i], times = length(temp_dates))
    temp_df <- tibble(datetime = temp_dates,
                      site_id = temp_sites)
    daily_dates_df <- bind_rows(daily_dates_df, temp_df)
  }
  
  #join to dates and interpolate
  dat1 <- left_join(daily_dates_df, dat, by = c("datetime","site_id")) %>%
    group_by(site_id) %>%
    mutate(observation = interpolate(observation))
  
  return(dat1)
}


####fable ----

fableNNETAR <- function(data, reference_datetime, forecast_horizon){
  
  #assign target and predictors
  df <- data %>%
    select(-depth_m, -variable) %>%
    mutate(datetime = as.Date(datetime)) |> 
    as_tsibble(key = site_id, index = datetime)
  
  #fit NNETAR from fable package
  my.nnar <- df %>%
    model(nnar = fable::NNETAR(log(observation + 0.001))) 
  fitted_values <- fitted(my.nnar)
  
  #build output df
  df.out <- data.frame(site_id = df$site_id,
                       model_id = "NNETAR",
                       datetime = df$datetime,
                       variable = "fDOM_QSU_mean",
                       depth_m = 1.5,
                       observation = df$observation,
                       prediction = fitted_values$.fitted)
  
  #get process error
  sd_resid <- sd(df.out$prediction - df.out$observation)
  
  #create "new data" dataframe
  fc_dates <- seq.Date(from = reference_datetime, to = reference_datetime + forecast_horizon, by = "day")
  new_data <- tibble(datetime = rep(fc_dates,times = 3),
                     site_id = rep(unique(df.out$site_id), each = length(fc_dates)),
                     observation = NA) %>%
    as_tsibble(key = site_id, index = datetime)
  
  #make forecast
  fc <- forecast(my.nnar, new_data = new_data, bootstrap = TRUE, times = 31)
  
  ensemble <- matrix(data = NA, nrow = length(fc$observation), ncol = 31)
  for(i in 1:length(fc$observation)){
    ensemble[i,] <- unlist(fc$observation[i])
  }
  
  ensemble_df <- data.frame(ensemble) %>%
    add_column(site_id = rep(unique(df.out$site_id), each = length(fc_dates)),
               datetime = rep(fc_dates,times = 3),
               reference_datetime = reference_datetime,
               family = "ensemble",
               variable = "fDOM_QSU_mean",
               model_id = "fableNNETAR",
               duration = "P1D",
               project_id = "vera4cast",
               depth_m = ifelse(site_id == "fcre",1.6,1.5)) %>%
    pivot_longer(X1:X31, names_to = "parameter", values_to = "prediction") %>%
    mutate(across(parameter, substr, 2, nchar(parameter)))  
  
  #return ensemble output in EFI format
  return(ensemble_df)
}



#### main function ----
fdom_nnetar <- function(forecast_date,
                        targets_df,
                        var,
                        forecast_horizon,
                        output_folder){
  
  #Define targets filepath
  targets <- targets_df
  
  target_variables <- var
  
  for (t in target_variables){
    
    print(t)
    
    #Define start and end dates (needed for interpolation)
    end_date = forecast_date
    
    #Format data
    dat_NNETAR <- format_data_NNETAR(targets = targets,
                                     end_date = end_date)
    
    #Set prediction window and forecast horizon
    reference_datetime <- forecast_date
    
    #Predict variable
    prediction_df <- fableNNETAR(data = dat_NNETAR,
                                 reference_datetime = reference_datetime,
                                 forecast_horizon = forecast_horizon) |> 
      select(-project_id, -duration) ###to save space in files for now, bring back at some point
    
    
  } # close variable iteration loop
  
  #save forecast csv
  
  return(write.csv(prediction_df, file = paste0(output_folder, forecast_date, ".csv"), row.names = F))
  
} # end fucntion 


##### Test function #####
##uncomment lines below to run function for one day 


# ## Set up target data 
# targets_fdom <- read_csv("./Data/GeneratedData/Targets_fDOM_allReservoirs.csv")
# targets_df <- targets_fdom |>
#   #filter(site_id == "fcre") |>
#   filter(datetime >= ymd("2023-03-13")) #filter to same starting point as AR and met regression
# 
# 
# ###function inputs
# forecast_date <- ymd("2023-05-06")
# 
# var <- "fDOM_QSU_mean"
# forecast_horizon <- 35
# 
# #update for local folder
# output_folder <- paste0("C:/Users/dwh18/Downloads/", "NNETAR_")
# 
# 
# #run function
# fdom_nnetar(forecast_date = forecast_date, targets_df = targets_df, var = var,
#               forecast_horizon = forecast_horizon, output_folder = output_folder)
# 
# 
# #plot example forecast
# read.csv("C:/Users/dwh18/Downloads/NNETAR_2023-05-06.csv")|>
#   mutate(date = as.Date(datetime)) |>
#   ggplot(aes(x = date, y = prediction, color = as.character(parameter)))+
#   geom_line()+
#   facet_wrap(~site_id) + guides(color = "none")






