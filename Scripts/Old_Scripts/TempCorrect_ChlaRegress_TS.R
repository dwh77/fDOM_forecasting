#### plotting timeseries of all three reservoirs 
library(tidyverse)
library(ggpmisc) #stat poly line


## EXO tests w/ long file
# exotest2 <- read_csv("C:/Users/dwh18/Downloads/EXOlong_tempTest.csv")
# 
# plot(exotest2$Temp)
# points(exotest2$fdom_QSU)
# 
# exotest2 |> 
#   #filter(Temp > 10, Temp < 16) |> 
#   ggplot(aes(x = Temp, y = fdom_QSU))+
#   geom_point()+
#   geom_smooth()
# 
# lm(fdom_QSU~Temp, data = exotest2)


## EXO tests w/ shorter file
# exotest <- read_csv("C:/Users/dwh18/Downloads/EXO_tempTest_v2.csv")
exotest <- read_csv("C:/Users/dwh18/Downloads/EXOtemptest.csv")

plot(exotest$Temp)
points(exotest$fdom_QSU)
points(exotest$fdom_RFU+6)
points(exotest$cond/10)
points(exotest$chlarfu+15)
points(exotest$dosat/10)

exotest |> 
  slice(20:200) |> 
  #filter(Temp > 10, Temp < 16) |> 
  ggplot(aes(x = Temp, y = fdom_QSU))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  geom_smooth()

lm(fdom_QSU~Temp, data = exotest)

-0.1784/14.99

exotest_2 <- exotest |> 
  filter(Temp > 10, Temp < 16)

lm(fdom_QSU~Temp, data = exotest_2)

-0.2285/15.6691

exotest_3 <- exotest |> 
  slice(20:200) 

lm(fdom_QSU~Temp, data = exotest_3)

-0.1844 / 15.1136

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
  filter(depth_m %in% c(1.6, 1.5),
         variable %in% c("fDOM_QSU_mean", "Temp_C_mean")) |> 
  select(Date, site_id, variable, observation) |> 
  pivot_wider(names_from = variable, values_from = observation)

targets |> 
  mutate(Date = as.Date(datetime)) |> 
  filter(depth_m %in% c(1.6, 1.5),
         variable %in% c("fDOM_QSU_mean", "Temp_C_mean", "Turbidity_FNU_mean")) |> 
  select(Date, site_id, variable, observation) |> 
  ggplot(aes(x = Date, y = observation))+
  geom_point()+
  facet_grid(variable~site_id, scales = "free_y")


ccr_daily <- ccrfull |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(fDOM_QSU_mean = mean(EXOfDOM_QSU_1, na.rm = T),
            fDOM_RFU_mean = mean(EXOfDOM_RFU_1, na.rm = T),
            Temp_C_mean = mean(EXOTemp_C_1, na.rm = T)) |> 
  mutate(site_id = "ccre")

ccr_daily |> 
  pivot_longer(-c(Date, site_id)) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, ncol = 1, scales = "free_y")


fdom <- rbind(targets_fdom, ccr_daily)

#Raw fDOM
fdom |> 
  #filter(Date > ymd("2022-12-01")) |> 
ggplot(aes(x = Date, y = fDOM_QSU_mean, color = site_id))+
  geom_point()+
  ylim(0,30)+ ggtitle("Raw fDOM")+
  theme_bw() + theme(legend.position = "top", text = element_text(size = 18))

#Temp Corr
p <- -0.01

fdom |> 
  #filter(Date > ymd("2022-12-01")) |> 
  mutate(fdom_TC = fDOM_QSU_mean/(1 + (p*(Temp_C_mean - 20)) )   ) |> 
ggplot(aes(x = Date, y = fdom_TC, color = site_id))+
  geom_point()+
  ylim(0,30)+ ggtitle("fDOM TC; p = -0.01")+
  theme_bw() + theme(legend.position = "top", text = element_text(size = 18))


p <- -0.03

fdom |> 
  #filter(Date > ymd("2022-12-01")) |> 
  mutate(fdom_TC = fDOM_QSU_mean/(1 + (p*(Temp_C_mean - 20)) )   ) |> 
  ggplot(aes(x = Date, y = fdom_TC, color = site_id))+
  geom_point()+
  ylim(0,30)+ ggtitle("fDOM TC; p = -0.03")+
  theme_bw() + theme(legend.position = "top", text = element_text(size = 18))


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

p <- -0.02

fcrTC <- fcrTC |> 
  mutate(fdom_TC = fdom/(1 + (p*(temp - 20))   ),
         fdom_TCundo = fdom*(1 + (p*(temp - 20))   )
  )

fcrTC  |> 
  ggplot(aes(x = Date))+
  geom_point(aes(y = fdom, color = "fdom_raw"))+
  geom_point(aes(y = fdom_TC, color = "fdom_TC"))
  #geom_point(aes(y = fdom_TCundo, color = "fdom_TCundo"))+
  #geom_point(aes(y = temp, color = "temp"))



fcrTC |> mutate(year = year(Date), doy = yday(Date)) |> 
  ggplot(aes(x = doy))+
  geom_point(aes(y = fdom, color = "fdom_raw"))+
  geom_point(aes(y = fdom_TC, color = "fdom_TC"))+
  facet_wrap(~year, ncol = 1)

fcrTC |> mutate(year = year(Date), doy = yday(Date)) |>
  mutate(resid = fdom_TC - fdom) |> 
  ggplot(aes(x = doy, y = resid))+
  geom_point()+
  facet_wrap(~year, ncol = 1)


summary(fcrTC$fdom)

summary(fcrTC$fdom_TC)

#DOC 
doc <- read.csv("https://pasta.lternet.edu/package/data/eml/edi/199/12/a33a5283120c56e90ea414e76d5b7ddb" )

fcrdoc <- doc |> 
  filter(Reservoir == "FCR",
         Site == 50, Depth_m == 1.6, !is.na(DOC_mgL)) |> #filter(DOC_mgL <10) |> 
  #mutate(Year = year(DateTime)) |> filter(Year == 2023) |> 
  select(DateTime, DOC_mgL) |> mutate(DateTime = as.Date(DateTime))

fcrTC |> 
  ggplot(aes(x = temp, y = fdom ))+
  geom_point()+ ggtitle("Temp ~fdom")+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = temp, y = fdom, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()

left_join(fcrdoc, fcrTC, by = c("DateTime" = "Date")) |> 
  ggplot(aes(x = fdom_TC, y = DOC_mgL ))+
  geom_point()+ ggtitle("TempCorr")+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = fdom_TC, y = DOC_mgL, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()

left_join(fcrdoc, fcrTC, by = c("DateTime" = "Date")) |> 
  ggplot(aes(x = fdom, y = DOC_mgL ))+
  geom_point()+ ggtitle("Raw")+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = fdom, y = DOC_mgL, label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()


#### a ----

