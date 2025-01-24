#### plotting timeseries of all three reservoirs 
library(tidyverse)
library(ggpmisc) #stat poly line



## EXO tests w/ shorter file
exotest <- read_csv("./Scripts/Old_Scripts/Temp_correct/exo_temp_correct_data.csv")

plot(exotest$Temp)
points(exotest$fdom_QSU)


exotest |> 
  ggplot(aes(x = Temp, y = fdom_QSU))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  geom_smooth()

lm(fdom_QSU~Temp, data = exotest)

-0.1784/14.99

exotest |> 
  filter(Temp > 10, Temp < 16) |> 
  ggplot(aes(x = Temp, y = fdom_QSU))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  geom_smooth()

exotest_2 <- exotest |> 
  filter(Temp > 10, Temp < 16)

lm(fdom_QSU~Temp, data = exotest_2)

-0.2285/15.6691

exotest |> 
  slice(20:200) |> 
  ggplot(aes(x = Temp, y = fdom_QSU))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  geom_smooth()

exotest_3 <- exotest |> 
  slice(20:200) 

lm(fdom_QSU~Temp, data = exotest_3)

-0.1844 / 15.1136



