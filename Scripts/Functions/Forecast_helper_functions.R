#### Script to hold functions for running different forecast models and helper functions


#### Helper functions ####

#identify current value
current_value <- function(dataframe, variable, start_date){
  
  value <- dataframe |> 
    mutate(datetime = as.Date(datetime)) |> 
    filter(datetime == start_date,
           variable == variable) |> 
    pull(observation)
  
  return(value)
}

#function to generate 30 ensembles of fDOM IC based on standard deviation around current observation
get_IC_uncert <- function(curr_fdom, n_members, ic_sd = 0.1){
  rnorm(n = n_members, mean = curr_fdom, sd = ic_sd)
}






