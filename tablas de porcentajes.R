rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(openxlsx)

out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido"

# Municipios --------------------------------------------------------------

mun <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQYNRmqrZVq5rctj6BEj_aM_f9oQRY_G-xnDMIgyjsiMnpsiy_9HHlbbpzFwj7pEV-u4Rgy3LaUw55P/pub?gid=266728315&single=true&output=csv", stringsAsFactors = F)

mun <- mun %>% 
  select(1:3, 24:43) %>% 
  pivot_longer(
    X2000:X2019, 
    names_to = "Anio", 
    values_to = "Ganador", 
    names_prefix = "X") %>% 
  mutate(Ganador = ifelse(Ganador == "", NA, Ganador))

try.mun <- mun %>% 
  mutate(
    cont = ifelse(!is.na(Ganador), 1, NA),
    first = sapply(strsplit(Ganador, "_"), "[", 1),
    first = ifelse(first == "nx", NA, first)
    ) %>% 
  group_by(Anio) %>% 
  mutate(tot = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Anio, first, tot) %>% 
  summarise(prop = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(porc = prop * 100 /tot) %>% 
  select(-c(prop, tot))

mun2 <- try.mun %>% 
  pivot_wider(names_from = Anio, 
              values_from = porc)

mun <- mun %>% 
  mutate(
    cont = ifelse(!is.na(Ganador), 1, NA),
    Ganador = ifelse(Ganador == "nx", NA, Ganador)
    ) %>% 
  group_by(Anio) %>% 
  mutate(tot = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Anio, Ganador, tot) %>% 
  summarise(prop = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(porc = prop * 100 /tot) %>% 
  select(-c(prop, tot)) %>% 
  pivot_wider(names_from = Anio, 
              values_from = porc)

# Diputaciones ------------------------------------------------------------

dip <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRA-4N91VihIrGzX5fEvErf3OvmsVwYKvD-6CF4Eq0Cmr4Xnb9I53ZaPPu07Vvk6YSbnK6l_bB-vchU/pub?gid=173991573&single=true&output=csv", stringsAsFactors = F)

dip <- dip %>% 
  mutate(GP = str_replace_all(GP, c("Conv" = "MC", "INDEP" = "Independiente"))) %>% 
  group_by(year) %>% 
  mutate(num = sum(Total)) %>% 
  ungroup() %>% 
  mutate(prop = Total * 100/num) %>% 
  select(Legislatura, year, GP, prop)

dip2 <- dip %>% 
  select(-Legislatura) %>% 
  pivot_wider(names_from = year,
              values_from = prop)

# Senadurías --------------------------------------------------------------

sen <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJkpFRyYSaghOiCEmBPRidWKd0EPlkyV26-91ZpV_UbVldUEMQY4IA5eKo-CvzrWgHYKgW9UU3BQ8z/pub?output=csv", stringsAsFactors = F)

sen <- sen %>% 
  mutate(GP = str_replace_all(GP,"Convergencia", "MC")) %>% 
  group_by(year) %>% 
  mutate(num = sum(Total)) %>% 
  ungroup() %>% 
  mutate(prop = Total * 100 /num) %>% 
  select(Legislatura, year, GP, prop)

sen2 <- sen %>% 
  select(-Legislatura) %>% 
  pivot_wider(names_from = year,
              values_from = prop)

# Exportar ----------------------------------------------------------------

wb <- createWorkbook()
sh <- addWorksheet(wb, "Municipios_coalición")
writeData(wb, sh, mun)
sh <- addWorksheet(wb, "Municipios_primer")
writeData(wb, sh, mun2)
sh <- addWorksheet(wb, "Diputaciones")
writeData(wb, sh, dip2)
sh <- addWorksheet(wb, "Senadurías")
writeData(wb, sh, sen2)
saveWorkbook(wb, paste(out, "Tablas de porcentajes.xlsx", sep = "/"), overwrite = T) 
