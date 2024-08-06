#### plotting timeseries of all three reservoirs 
library(tidyverse)


#### Get data ----

#VERA
targets_url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

targets <- read_csv(targets_url)


#CCR 
ccr_L1 <- read_csv("https://raw.githubusercontent.com/FLARE-forecast/CCRE-data/ccre-dam-data-qaqc/ccre-waterquality_L1.csv")

ccr <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1069/2/ea78dd541e089687af1f4c4b550bc9ca" )

ccrfull <- rbind(ccr, ccr_L1)


#### plot of 3 reservoir timeseries ---- 

targets_fdom <- targets |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(variable %in% c("fDOM_QSU_mean")) |> 
  select(Date, site_id, observation) |> 
  rename(fdom = observation)

ccr_daily <- ccrfull |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fdom = mean(EXOfDOM_QSU_1, na.rm = T)) |> 
  mutate(site_id = "ccre")


fdom <- rbind(targets_fdom, ccr_daily)

fdom |> 
  filter(Date > ymd("2022-12-01")) |> 
  #        Date < ymd("2024-02-01")) |> 
ggplot(aes(x = Date, y = fdom, color = site_id))+
  geom_point()+
  geom_vline(aes(xintercept = ymd("2022-12-12")))+
  geom_vline(aes(xintercept = ymd("2024-01-31")))+
  labs(y = "fDOM (QSU)")+
  theme_classic() + theme(legend.position = "top", text = element_text(size = 18))


#### fdom ~ chla correlation ----

fdom_chla_vera <- targets |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(variable %in% c("fDOM_QSU_mean", "Chla_ugL_mean")) |> 
  select(Date, site_id, variable, observation) |> 
  pivot_wider(names_from = variable, values_from = observation)

#fcr
fcr_fdom_chla <- fdom_chla_vera |> 
  filter(Date > ymd("2023-01-01")) |> 
  filter(site_id == "fcre") |> 
  mutate(chla_lag1 = lag(Chla_ugL_mean, 1)) |> 
  filter(!is.na(fDOM_QSU_mean),
         !is.na(chla_lag1)) 

fcr_fdom_chla |> 
  ggplot()+
  geom_point(aes(x = Date, y = fDOM_QSU_mean, color = "fdom"), color = "maroon")+
  geom_point(aes(x = Date, y = Chla_ugL_mean, color = "chla"), color = "green3")

cor(fcr_fdom_chla$fDOM_QSU_mean, fcr_fdom_chla$Chla_ugL_mean)
cor(fcr_fdom_chla$fDOM_QSU_mean, fcr_fdom_chla$chla_lag1)


#bvr
bvr_fdom_chla <- fdom_chla_vera |> 
  filter(Date > ymd("2023-01-01")) |> 
  filter(site_id == "bvre") |> 
  mutate(chla_lag1 = lag(Chla_ugL_mean, 1)) |> 
  filter(!is.na(fDOM_QSU_mean),
         !is.na(chla_lag1)) 

bvr_fdom_chla |> 
  ggplot()+
  geom_point(aes(x = Date, y = fDOM_QSU_mean, color = "fdom"), color = "maroon")+
  geom_point(aes(x = Date, y = Chla_ugL_mean, color = "chla"), color = "green3")

cor(bvr_fdom_chla$fDOM_QSU_mean, bvr_fdom_chla$Chla_ugL_mean)
cor(bvr_fdom_chla$fDOM_QSU_mean, bvr_fdom_chla$chla_lag1)


#ccr
ccr_fdom_chla <- ccrfull |> 
  mutate(Date = as.Date(DateTime)) |> 
  select(Date, EXOfDOM_QSU_1, EXOChla_ugL_1) |> 
  group_by(Date) |> 
  summarise(fDOM_QSU_mean = mean(EXOfDOM_QSU_1, na.rm = T),
            Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = T)) |> 
  #filter(Date > ymd("2023-01-01")) |> 
  mutate(chla_lag1 = lag(Chla_ugL_mean, 1)) |> 
  filter(!is.na(fDOM_QSU_mean),
         !is.na(chla_lag1)) 

ccr_fdom_chla |> 
  ggplot()+
  geom_point(aes(x = Date, y = fDOM_QSU_mean, color = "fdom"), color = "maroon")+
  geom_point(aes(x = Date, y = Chla_ugL_mean, color = "chla"), color = "green3")

cor(ccr_fdom_chla$fDOM_QSU_mean, ccr_fdom_chla$Chla_ugL_mean)
cor(ccr_fdom_chla$fDOM_QSU_mean, ccr_fdom_chla$chla_lag1)




#### looking at temp correction ----

##CCR
temp <- ccr |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fdom = mean(EXOfDOM_QSU_1, na.rm = T),
            temp = mean(EXOTemp_C_1, na.rm = T)) 

temp |> ggplot(aes(x = Date))+
  geom_point(aes(y = fdom, color = "fdom"))+
  geom_point(aes(y = temp, color = "temp"))

p <- -0.027

temp <- temp |> 
  mutate(fdom_TC = fdom/(1 + (p*(temp - 20)) 
                         )
         )

temp |> ggplot(aes(x = Date))+
  geom_point(aes(y = fdom, color = "fdom_raw"))+
  geom_point(aes(y = fdom_TC, color = "fdom_TC"))+
  geom_point(aes(y = temp, color = "temp"))

##FCR 
fcrTC <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce" 
) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fdom = mean(EXOfDOM_QSU_1, na.rm = T),
            temp = mean(EXOTemp_C_1, na.rm = T)) 



fcrTC |> ggplot(aes(x = Date))+
  geom_point(aes(y = fdom, color = "fdom"))+
  geom_point(aes(y = temp, color = "temp"))

p <- -0.015

fcrTC <- fcrTC |> 
  mutate(fdom_TC = fdom/(1 + (p*(temp - 20)) 
  )
  )

fcrTC |> ggplot(aes(x = Date))+
  geom_point(aes(y = fdom, color = "fdom_raw"))+
  geom_point(aes(y = fdom_TC, color = "fdom_TC"))+
  geom_point(aes(y = temp, color = "temp"))










#### a ----

