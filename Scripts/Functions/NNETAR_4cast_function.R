####Generate NNETAR fDOM forecasts
#adapting ADD github code
#https://github.com/addelany/vera4casts/blob/main/code/combined_workflow/nnetar_workflow.R

##get source scripts from githbub 
#fableNNETAR
devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/nettar_functions/fableNNETAR.R")
#format_data_NNETAR
devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/nettar_functions/format_data_NNETAR.R")
#interpolate
devtools::source_url("https://raw.githubusercontent.com/addelany/vera4casts/main/code/function_library/nettar_functions/interpolate.R")


## function 
fdom_nnetar <- function(forecast_date,
                        targets_url,
                        var,
                        forecast_horizon,
                        output_folder){
  
  #Define targets filepath
  targets <- targets_url
  
  target_variables <- var
  
  for (t in target_variables){
    
    print(t)
    
    #Define start and end dates (needed for interpolation)
    end_date = forecast_date
    
    #Format data
    dat_NNETAR <- format_data_NNETAR(targets = targets,
                                     target_var = t,
                                     end_date = end_date)
    
    #Set prediction window and forecast horizon
    reference_datetime <- forecast_date
    
    #Predict variable
    prediction_df <- fableNNETAR(data = dat_NNETAR,
                                 target_var = t,
                                 reference_datetime = reference_datetime,
                                 forecast_horizon = forecast_horizon) |> 
      select(-project_id, -duration) ###to save space in files for now ############bring back at some point
    
    
  } # close variable iteration loop
  
  #save forecast csv
  
  return(write.csv(prediction_df, file = paste0("C:/Users/dwh18/OneDrive/Desktop/R_Projects/fDOM_forecasting/Data/ASLO_talk_forecast_output/", output_folder, "/forecast_nnetar_", forecast_date, '.csv'), row.names = F))
  
} # end fucntion 

