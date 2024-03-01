
library(fs)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(tidyverse)
library(dbscan)
library(purrr)
library(grid)
library(cowplot)
library(ggpubr)
library(scales)
library(readxl)

bbch <- read_xlsx('data/data_raw/bbch.xlsx') |>
  mutate(fecha_inicial = as.Date(fecha_inicial),
         fecha_final = as.Date(fecha_final)) |>
  pivot_longer(cols = c('fecha_inicial','fecha_final'), values_to = 'fecha', names_to = 'fecha_tipo')

fechas <- bbch |>
  group_by(sitio) |>
  summarise(fecha_min = min(fecha),
            fecha_max = max(fecha)) |>
  pivot_longer(cols = c('fecha_min','fecha_max'),values_to = 'fecha',names_to = 'tipo') |>
  pull(fecha)

fechas <- list(la_esperanza = seq.Date(fechas[1],fechas[2], by = 'day'),
            rio_claro = seq.Date(fechas[3],fechas[4], by = 'day'))

data_gdd <- read_rds('data/data_processed/gdd.rds')

datos_cor <- tibble(sitio = rep(c('la_esperanza','rio_claro'),times = c(length(fechas$la_esperanza),
                                                                     length(fechas$rio_claro))),
                 fecha = as.Date(unlist(fechas))) |>
  left_join(bbch, by = c('sitio','fecha')) |>
  fill(etapa, bbch) |>
  select(-fecha_tipo) |>
  left_join(data_gdd, by = c('sitio','fecha')) |>
  select(sitio,gdd_acum,bbch) |>
  rename(gdd = gdd_acum) |>
  na.omit() |>
  mutate(gdd_log = log(gdd))

datos_le <- datos_cor |> filter(sitio == 'la_esperanza')
datos_rc <- datos_cor |> filter(sitio == 'rio_claro')

# Relación logarítmica

modelo_log <- nls(bbch ~ SSlogis(gdd, Asym, xmid, scal), data = datos_le)
parametros <- coef(modelo_log)
predicciones <- data.frame(gdd = seq(min(datos_le$gdd), max(datos_le$gdd), length.out = 100))
predicciones$bbch_pred <- predict(modelo_log, newdata = predicciones)

r2 <- cor(datos_le$bbch,predict(modelo_log))^2

plot_le <- ggplot(datos_le, aes(gdd,bbch)) +
  geom_point() +
  geom_line(data = predicciones, aes(gdd,bbch_pred), color = "blue", linewidth = 1) +
  annotate("text", x = 1300, y = 84, label = paste("R² =", round(r2, 2)), size = 5) +
  labs(title = "La Esperanza",
       x = "GDD",
       y = "Escala BBCH") +
  ylim(60,100) +
  theme_light()

modelo_log <- nls(bbch ~ SSlogis(gdd, Asym, xmid, scal), data = datos_rc)
parametros <- coef(modelo_log)
predicciones <- data.frame(gdd = seq(min(datos_rc$gdd), max(datos_rc$gdd), length.out = 100))
predicciones$bbch_pred <- predict(modelo_log, newdata = predicciones)

r2 <- cor(datos_rc$bbch,predict(modelo_log))^2

plot_rc <- ggplot(datos_rc, aes(gdd,bbch)) +
  geom_point() +
  geom_line(data = predicciones, aes(gdd,bbch_pred), color = "blue", linewidth = 1) +
  annotate("text", x = 1000, y = 84, label = paste("R² =", round(r2, 2)), size = 5) +
  labs(title = "Río Claro",
       x = "GDD",
       y = "Escala BBCH") +
  ylim(60,100) +
  theme_light()

log_combined <- plot_grid(ggdraw() + draw_label('Ajuste logarítmico BBCH vs. GDD', size = 14),
                          plot_grid(plot_le,plot_rc, ncol = 2),
                          ncol = 1, rel_heights = c(.1,1))

# Relación lineal

plot_le <- ggplot(datos_le, aes(x = gdd_log, y = bbch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = 'pearson', label.x = 6.2, label.y = 77, color = 'black',geom = 'label') +
  labs(title = "La Esperanza",
       x = "GDD (log)",
       y = "Escala BBCH") +
  theme_light()

plot_rc <- ggplot(datos_rc, aes(x = gdd_log, y = bbch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = 'pearson', label.x = 6.2, label.y = 71, color = 'black',geom = 'label') +
  labs(title = "Rio Claro",
       x = "GDD (log)",
       y = "Escala BBCH") +
  theme_light()

lm_combined <- plot_grid(ggdraw() + draw_label('Ajuste lineal BBCH vs. GDD', size = 14),
                          plot_grid(plot_le,plot_rc, ncol = 2),
                          ncol = 1, rel_heights = c(.1,1))

# Boxplot

data_gdd <- read_rds('data/data_processed/gdd.rds')

data_gdd |>
  left_join(bbch, by = c('sitio','fecha')) |>
  filter(!is.na(etapa)) |>
  ggplot(aes(as.factor(bbch),gdd_acum)) +
  geom_boxplot() +
  facet_wrap(~sitio) +
  coord_flip()
