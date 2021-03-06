rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido"

# Municipios --------------------------------------------------------------

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
                                                              ifelse(first == "PVEM", "PVEM", 
                                                                     ifelse(first == "nx", NA, 
                                                                            ifelse(first == "PNA", "PNA",
                                                                                   ifelse(first == "MC", "MC", "Otros")))))))))))
  )

try <- data %>% 
 mutate(cont = ifelse(!is.na(Ganador), 1, NA)) %>% 
  group_by(Anio) %>% 
  mutate(tot = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(Anio, first2, tot) %>% 
  summarise(prop = sum(cont, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(porc = round(prop/tot, 2))

# try <- try %>% 
#   filter(!is.na(first2))

party <- c("INDEP" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7",    "PNA" = "#01F7CA",
           "PT" = "#DE1830", "Otros" = "#fc03c6", "Independiente" = "#c48900",
           "Sin_Grupo" = "#c48900")

ggplot(try %>% filter(first2 != "Usos y costumbres"), aes(x = Anio, y = porc)) +
  geom_bar(aes(fill = first2), stat = "identity") + 
  scale_fill_manual(values = party) +
  scale_y_continuous(labels = scales::percent_format())

ggplot(try %>% filter(first2 != "Usos y costumbres" & first2 != "nx"), aes(x = Anio, y = porc, group = first2, col = first2)) +
  geom_line(size = 2) +
  scale_color_manual(values = party, name = "") +
  #geom_text(aes(label = max(porc))) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, .60, by = .05), labels = scales::percent_format()) +
  labs(title = "Proporción de municipios gobernados por partidos nacionales", subtitle = "2000 -2019", x = "", y = "", caption = "Sin incluir usos y costumbres")
ggsave(paste(out, "Proporción.png", sep = "/"), dpi = 300, width = 40, height = 20, units = "cm")

# Diputaciones ------------------------------------------------------------

dip <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRA-4N91VihIrGzX5fEvErf3OvmsVwYKvD-6CF4Eq0Cmr4Xnb9I53ZaPPu07Vvk6YSbnK6l_bB-vchU/pub?gid=173991573&single=true&output=csv")

dip <- dip %>% 
  mutate(GP = str_replace_all(GP, c("Conv" = "MC", "INDEP" = "Independiente")))

try.dip <- dip %>% 
  mutate(
    GP2 = ifelse(GP == "MC", GP,
                 ifelse(GP == "Morena", GP, 
                        ifelse(GP == "PAN", GP,
                               ifelse(GP == "PNA", GP, 
                                      ifelse(GP == "PRD", GP, 
                                             ifelse(GP == "PRI", GP, 
                                                    ifelse(GP == "PT", GP,
                                                           ifelse(GP == "PVEM", GP, "Otros"))))))))
  ) %>% 
  group_by(year, Legislatura, GP2) %>% 
  summarise(Tot = sum(Total)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(num = sum(Tot)) %>% 
  ungroup() %>% 
  mutate(prop = Tot/num)

ggplot(try.dip, aes(x = as.factor(year), y = prop, group = GP2, col = GP2))+
  geom_line(size = 1.5) +
  scale_color_manual(values = party, name = "") +
  scale_y_continuous(breaks = seq(0, .55, by = .05), labels = scales::percent_format()) +
  labs(title = "Porcentaje de curules por partidos nacionales", subtitle = "Cámara de Diputados 2000-2018",
       x = "", y = "")
ggsave(paste(out, "Proporción_Dips.png", sep = "/"), dpi = 300, width = 40, height = 20, units = "cm")
# Senadurías --------------------------------------------------------------

sen <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJkpFRyYSaghOiCEmBPRidWKd0EPlkyV26-91ZpV_UbVldUEMQY4IA5eKo-CvzrWgHYKgW9UU3BQ8z/pub?output=csv")

sen <- sen %>% 
  mutate(GP = str_replace_all(GP,"Convergencia", "MC")) %>% 
  group_by(year) %>% 
  mutate(num = sum(Total)) %>% 
  ungroup() %>% 
  mutate(prop = Total/num)

ggplot(sen, aes(x = as.factor(year), y = prop, group = GP, col = GP))+
  geom_line(size = 1.5) +
  scale_color_manual(values = party, name = "") +
  scale_y_continuous(breaks = seq(0, .55, by = .05), labels = scales::percent_format()) +
  labs(title = "Porcentaje de curules por partidos nacionales", subtitle = "Cámara de Senadores 2000-2018",
       x = "", y = "")
ggsave(paste(out, "Proporción_Sen.png", sep = "/"), dpi = 300, width = 40, height = 20, units = "cm")