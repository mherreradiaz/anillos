
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(agvAPI)

# Temporada 2022-2023

periodo <- c('2022-05-01','2023-01-01')

le_temp <- getDataAGV_clima(station_id ='00205018', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)


rc_temp <- getDataAGV_clima(station_id ='00203E6E', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)


temp <- rbind(le_temp,rc_temp) |>
  rename(tavg = `avg (°C)`,
         tmin = `min (°C)`,
         tmax = `max (°C)`)

data_temp_2022 <- temp |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |>
  mutate(fecha = format(datetime, "%Y-%m-%d"),
         hora = hour(floor_date(datetime,'1 hour')),
         .before = datetime) |>
  select(-datetime) |>
  group_by(sitio,fecha,hora) |>
  summarise(tavg = mean(t_avg,na.rm=T),
            tmin = min(t_min,na.rm=T),
            tmax = max(t_max,na.rm=T)) |>
  ungroup() |>
  mutate(temporada = '2022-2023', .before = fecha)

# Temporada 2023-2024

periodo <- c('2023-05-01','2024-01-01')

le_temp <- getDataAGV_clima(station_id ='00205018', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'la_esperanza', .before = datetime)


rc_temp <- getDataAGV_clima(station_id ='00203E6E', var = 'Temperature',
                            time_span = periodo) |>
  mutate(sitio = 'rio_claro', .before = datetime)


temp <- rbind(le_temp,rc_temp) |>
  rename(tavg = `avg (°C)`,
         tmin = `min (°C)`,
         tmax = `max (°C)`)

data_temp_2023 <- temp |>
  mutate(datetime = as_datetime(datetime,tz = 'America/Santiago')) |>
  mutate(fecha = format(datetime, "%Y-%m-%d"),
         hora = hour(floor_date(datetime,'1 hour')),
         .before = datetime) |>
  select(-datetime) |>
  group_by(sitio,fecha,hora) |>
  summarise(tavg = mean(t_avg,na.rm=T),
            tmin = min(t_min,na.rm=T),
            tmax = max(t_max,na.rm=T)) |>
  ungroup() |>
  mutate(temporada = '2023-2024', .before = fecha)

# Unir temporadas

data_temp <- rbind(data_temp_2022,data_temp_2023)

write_rds(data_temp, 'data/data_processed/temperatura.rds')

