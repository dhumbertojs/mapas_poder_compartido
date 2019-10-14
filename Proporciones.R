rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido"

data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQYNRmqrZVq5rctj6BEj_aM_f9oQRY_G-xnDMIgyjsiMnpsiy_9HHlbbpzFwj7pEV-u4Rgy3LaUw55P/pub?gid=266728315&single=true&output=csv", stringsAsFactors = F)

data <- data %>% 
  select(1:3, 24:43) %>% 
  pivot_longer(
    X2000:X2019, 
    names_to = "Anio", 
    values_to = "Ganador", 
    names_prefix = "X") %>% 
  filter(!is.na(Ganador))

data <- data %>% 
  mutate(
    first = sapply(strsplit(Ganador, "_"), "[", 1),
    first2 = ifelse(first == "PAN", "PAN",
                    ifelse(first == "PRI", "PRI",
                           ifelse(first == "PRD", "PRD", 
                                  ifelse(first == "Independiente", "Independiente", 
                                         ifelse(first == "Usos y costumbres", "Usos y costumbres", 
                                                ifelse(first == "MORENA", "Morena",
                                                       ifelse(first == "PT", "PT",
                                                              ifelse(first == "PVEM", "PVEM", "Otros"))))))))
  )

ggplot(data %>% filter(!is.na(first)), aes(x = Anio, y = first)) +
  geom_jitter()

try <- data %>% 
 mutate(
   cont = 1
 ) %>% 
  group_by(Anio) %>% 
  mutate(tot = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Anio, first2, tot) %>% 
  summarise(prop = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(porc = prop * 100/tot)

try <- try %>% 
  filter(!is.na(first2))

party <- c("INDEP" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7",    "PNA" = "#01F7CA",
           "PT" = "#DE1830", "Otros" = "#fc03c6", "Independiente" = "#c48900")

ggplot(try %>% filter(first2 != "Usos y costumbres"), aes(x = Anio, y = porc)) +
  geom_bar(aes(fill = first2), stat = "identity") + scale_fill_manual(values = party)

ggplot(try %>% filter(first2 != "Usos y costumbres"), aes(x = Anio, y = porc, group = first2, col = first2)) +
  geom_line() +
  scale_color_manual(values = party, name = "") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  labs(title = "Proporción de municipios gobernados por partidos nacionales", x = "", y = "")
ggsave(paste(out, "Proporción.png", sep = "/"), dpi = 300, width = 20, height = 40, units = "cm")