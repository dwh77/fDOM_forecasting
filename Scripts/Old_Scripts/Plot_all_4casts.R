nnetar <- read.csv("./Data/Forecast_Outputs/NNETAR_4casts/allReservoir_NNETAR_4cast.csv")




#facet for all forecasts; HAVE TO RUN AS CHUNK
pdf("./Data/Forecast_Outputs/NNETAR_4casts/all_nnetar_forecasts.pdf", height = 500)

nnetar |>
  # filter(reference_datetime >= ymd("2024-01-05"),
  #        reference_datetime <= ymd("2024-01-15")) |>
  mutate(Horizon = as.Date(datetime) - as.Date(reference_datetime)) |>
  ggplot(aes(x = Horizon, y = prediction, color = as.character(parameter)))+
  geom_line()+
  facet_grid(reference_datetime~site_id, scales = "free_y")+
  guides(color = "none")+
  theme_bw()

dev.off()