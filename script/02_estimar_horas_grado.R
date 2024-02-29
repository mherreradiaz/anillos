
library(chillR)
library(tidyquant)
library(plotly)
library(ggplot2)
library(chron)
library(lubridate)
library(tidyr)  
library(tidyverse)
library(imputeTS)

data_temp <- read_rds('data/temperatura.rds') |>
  select(-t_min,-t_max) |>
  rename(Temp = t_avg)

latitud_la_esperanza <- -34.32844
latitud_rio_claro <- -34.69997

data_fill <- data_temp |>
  mutate(Year = year(fecha),
         Month = month(fecha),
         Day = day(fecha),
         .before = hora) |>
  rename(Hour = hora) |>
  mutate(latitud = ifelse(sitio == 'la_esperanza',latitud_la_esperanza,latitud_rio_claro)) |>
  group_by(temporada,sitio) |>
  reframe(interpolate_gaps_hourly(hourtemps = data.frame(Year,Month,Day,Hour,Temp), 
                                  latitude = unique(latitud))$weather) |>
  mutate(fecha = as.Date(paste(Year,Month,Day,sep = '-')),
         hora = Hour,
         .before = Year) |>
  select(-(Year:Tmax_source)) |>
  rename(temp = Temp)

data_cp <- data_fill |>
  filter(between(month(fecha),5,9)) |>
  group_by(temporada,sitio) |>
  mutate(cp = round(Dynamic_Model(temp),3))

data_gdh <- data_fill |>
  filter(between(month(fecha),8,9)) |>
  group_by(temporada,sitio) |>
  mutate(gdh = round(GDH(temp,summ = T),3))

data_diagrados <- data_fill |>
  left_join(data_cp, by = c('temporada','sitio','fecha','hora','temp')) |>
  left_join(data_gdh, by = c('temporada','sitio','fecha','hora','temp'))

write_rds(data_diagrado,'data/diagrado.rds')
