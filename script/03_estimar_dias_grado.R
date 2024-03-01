
library(chillR)
library(tidyquant)
library(plotly)
library(ggplot2)
library(chron)
library(lubridate)
library(tidyr)  
library(tidyverse)
library(imputeTS)
gdd_function <- function(tmin, tmax, tb = 10, tu = 35) {
  
  tmean <- (tmin + tmax)/2
  a <- (tmax-tmin)/2
  theta1 = suppressWarnings(asin((tb-tmean)/a))
  theta2 = suppressWarnings(asin((tu-tmean)/a))
  
  if (tmax < tb) {
    GDD <- 0
  } else if (tmin < tb & tmax < tu) {
    GDD <- (1 / pi) * ((tmean - tb) * ((pi / 2) - theta1) + a * cos(theta1))
  } else if (tmin > tb & tmax < tu) {
    GDD <- tmean - tb
  } else if (tmin > tb & tmax > tu) {
    GDD <- (tu - tb) / 2
  } else if (tmin < tb & tmax > tu) {
    GDD <- (1 / pi) * (tmean * (theta2 - theta1) + a * (cos(theta1) - cos(theta2)) + (tu - tb) * ((pi / 2) - theta2))
  } else {
    GDD <- NA
  }
  return(GDD)
}

# Rellenar data temperatura

data_temp <- read_rds('data/data_processed/temperatura.rds') |>
  mutate(fecha = as.Date(fecha)) |>
  group_by(sitio,temporada,fecha) |>
  summarise(tmin = min(tmin,na.rm=F),
            tmax = max(tmax,na.rm=F),
            tavg = mean(tavg,na.rm=F)) |>
  ungroup() |>
  mutate(tmin = ifelse(tmin == 10,10.01,tmin),
         tmax = ifelse(tmax == 35,35.01,tmax))

fechas_2022 <- seq(as.Date("2022-06-01"), as.Date("2023-02-28"), by = "day")
fechas_2023 <- seq(as.Date("2023-06-01"), as.Date("2024-02-29"), by = "day")

data_gaps <- tibble(
  sitio = c(rep(c("la_esperanza", "rio_claro"), each = length(fechas_2022)),
            rep(c("la_esperanza", "rio_claro"), each = length(fechas_2023))),
  fecha = c(rep(fechas_2022,2),rep(fechas_2023,2))) |>
  mutate(temporada = ifelse(fecha < '2023-04-01','2022-2023','2023-2024'),
                            .before = fecha) |>
  left_join(data_temp, by = c('sitio','temporada','fecha')) |>
  mutate(Year = year(fecha),
         Month = month(fecha),
         Day = day(fecha),
         Tmin = tmin,
         Tmax = tmax,
         Tavg = tavg) |>
  select(sitio,fecha,Year:Tavg)

gaps_le <- data_gaps |>
  filter(sitio == 'la_esperanza') |>
  select(-sitio)

gaps_rc <- data_gaps |>
  filter(sitio == 'rio_claro') |>
  select(-sitio)

gaps_le$Tmin <- fix_weather(gaps_le)$weather |>
  filter(!is.na(fecha)) |> pull(Tmin)
gaps_le$Tmax <- fix_weather(gaps_le)$weather |>
  filter(!is.na(fecha)) |> pull(Tmax)
gaps_rc$Tmin <- fix_weather(gaps_rc)$weather |>
  filter(!is.na(fecha)) |> pull(Tmin)
gaps_rc$Tmax <- fix_weather(gaps_rc)$weather |>
  filter(!is.na(fecha)) |> pull(Tmax)

gaps_le$sitio = 'la_esperanza'
gaps_rc$sitio = 'rio_claro'

data_fill <- rbind(gaps_le,gaps_rc) |>
  select(sitio,fecha,-Year,-Month,-Day,Tmin,Tmax,Tavg) |>
  rename(tmin = Tmin,
         tmax = Tmax,
         tavg = Tavg) |>
  mutate(temporada = ifelse(fecha < '2023-05-01','2022-2023','2023-2024'),
         .before = fecha) |>
  arrange(temporada,sitio,fecha) |>
  mutate(tavg = ifelse(is.na(tavg),(tmin+tmax)/2,tavg))

write_rds(data_fill,'data/data_processed/temperatura_fill.rds')

# Calcular GDD

data_temp <- read_rds('data/data_processed/temperatura_fill.rds')

data_gdd <- data_temp |>
  filter(!between(month(fecha),3,7)) |>
  rowwise() |>
  mutate(gdd = gdd_function(tmin,tmax)) |>
  group_by(sitio,temporada) |>
  mutate(gdd_acum = cumsum(gdd)) |>
  ungroup()

write_rds(data_gdd,'data/data_processed/gdd.rds')

