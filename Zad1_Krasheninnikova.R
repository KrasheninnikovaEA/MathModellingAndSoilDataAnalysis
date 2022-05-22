library(tidyverse)
library(rnoaa)
library(lubridate)
#station_data = ghcnd_stations() 
#Может занять несколько минут лучше выполнить один раз в месте с хорошим интернетом и сохранить результат
#write.csv(station_data, "C:/Users/vasil/Desktop/Zad1/stations.csv", row.names = FALSE)
station_data = read.csv("stations.csv")
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
orel = data.frame(id = "OREL", latitude = 52.9650800,  longitude = 36.0784900)
orel_around = meteo_nearby_stations(lat_lon_df = orel, station_data = station_data,
                                    limit = 20, var = c("PRCP", "TAVG"),
                                    year_min = 1995, year_max = 2005)
#tula_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Тулы, очевидно что первым элементом таблицы будет идентификатор метеостанции Тулы, его то мы и попытаемся получить
orel_id = orel_around[["OREL"]][["id"]][1:3]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_orel_data = meteo_tidy_ghcnd(stationid = orel_id, var = "TAVG", date_min = "1995-01-01", date_max = "2005-12-31")

Orel_data_clean = all_orel_data %>% 
  mutate(year = year(date), month = month(date)) %>%
  group_by(month) %>%
  summarise(tavg = sum(tavg[tavg>50])/10)

af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,0.00,0.00,0.00)
Kf = 300
Qj = 1600
Lj = 2.2 
Ej = 25

Orel_res = Orel_data_clean %>%
  group_by(month) %>%
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  mutate(a = af, b = bf) %>%
  mutate(fert = ((a + b * 1.0 * s) * di * Kf) / (Qj * Lj * (100 - Ej)))

Yield = sum(Orel_res$fert); Yield