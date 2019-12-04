library(dplyr)
library(stringr)
library(tidyr)

inp <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido/Congresos locales"

bc <- read.csv(paste(inp, "baja california.csv", sep = "/"), stringsAsFactors = F)

bc <- bc %>% 
  mutate(cont = 1,
         partido = str_replace_all(partido, "PANAL", "PNA"),
         year = year - 3) %>% 
  group_by(partido, principio, legis, year) %>% 
  summarise_all(sum)

bc <- bc %>% 
  pivot_wider(names_from = principio,
              values_from = cont) %>% 
  select(partido, MR, RP, legis, year)

write.csv(bc, paste(inp, "integracion BC.csv", sep = "/"), row.names = F)

data <- read.csv(paste(inp, "fuente C diputados.csv", sep = "/"), stringsAsFactors = F)

data <- data %>% 
  mutate(
    cont = 1,
    principio = ifelse(prin == "", "RP", "MR"),
    partido = str_replace_all(partido, c("Comp con Puebla" = "Comp. con Puebla",
                                         "CONVERGENCIA" = "Convergencia",
                                         "ENC SOC" = "PES",
                                         "MoCi" = "MC",
                                         "Nueva Alianza" = "PNA",
                                         "PANAL" = "PNA",
                                         "PaDu" = "PD",
                                         "Partido por Durango" = "PD",
                                         "PUP" = "UP",
                                         "Unidad Popular" = "UP",
                                         "PMC" = "MC",
                                         "INDE" = "IND",
                                         "S/P" = "IND",
                                         "MORENA" = "Morena",
                                         "Coalicion" = "Coalición"
                                         )),
    partido = str_replace(partido, "CONV", "Convergencia"),
    partido = str_replace(partido, "Alianza", "PNA")
  ) %>% 
  group_by(partido, year,legis,estado, principio) %>% 
  summarise(cont = sum(cont))

data <- data %>% 
  pivot_wider(names_from = principio,
              values_from = cont) %>% 
  select(estado, partido, MR, RP, legis, year)

write.csv(data, paste(inp, "integracion congresos fuente dipus.csv", sep = "/"), row.names = F)