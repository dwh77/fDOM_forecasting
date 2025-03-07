#check out trophic statuses of reservoirs


library(tidyverse)

chla <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/555/4/4d92727343445ad28046d57ac852b45f" )


chla_TSI <- chla |> 
  mutate(year = year(DateTime)) |> 
  filter(year >= 2022) |> 
  filter(Depth_m <= 2,
         Site == 50) |> 
  group_by(Reservoir) |> 
  summarise(meanChla = mean(Chla_ugL, na.rm = T))


secchi <- read_csv( "https://pasta.lternet.edu/package/data/eml/edi/198/13/3ee0ddb9f2183ad4d8c955d50d1b8fba"  )


secchi_TSI <- secchi |> 
  mutate(year = year(DateTime)) |> 
  filter(year >= 2022) |> 
  filter(Site == 50) |> 
  group_by(Reservoir) |> 
  summarise(meanSecchi = mean(Secchi_m , na.rm = T))



chem <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/199/12/a33a5283120c56e90ea414e76d5b7ddb" )


chem_TSI <- chem |> 
  mutate(year = year(DateTime)) |> 
  filter(year >= 2022) |> 
  filter(Site == 50,
         Depth_m <= 2) |> 
  group_by(Reservoir) |> 
  summarise(meanTP = mean(TP_ugL , na.rm = T))


## calc TSI values 
TSI <- left_join(secchi_TSI, chla_TSI, by = "Reservoir") |> 
  left_join(chem_TSI, by = "Reservoir") |> 
  filter(Reservoir %in% c("CCR", "BVR", "FCR")) |> 
  mutate(TSI_SD = ( 60- (14.41 * log(meanSecchi) ) ),
         TSI_CHLA = ( (9.81 * log(meanChla) ) + 30.6  ),
         TSI_TP = ( (14.42 * log(meanTP) ) + 4.15 )
         )

#print TSI
TSI

#average across variables 
TSI |> 
  select(1, 5,7) |> #dropping chla since CCr doesn't have a chla value
  pivot_longer(-1) |> 
  group_by(Reservoir) |> 
  summarise(TSI_mean = mean(value, na.rm = T))








