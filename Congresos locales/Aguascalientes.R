library(dplyr)
library(stringr)
library(tidyr)

inp <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido/Congresos locales"

ags <- read.csv(paste(inp, "ags.csv", sep = "/"), stringsAsFactors = F)

ags <- ags %>% 
  mutate(
    cont = 1,
    PRINCIPIO = str_replace_all(PRINCIPIO, 
                                c("Mayoría" = "MR", 
                                  "Representación Proporcional" = "RP"))
         ) %>% 
  group_by(PARTIDO, PRINCIPIO, legis, year) %>% 
  summarise_all(sum)

ags <- ags %>% 
  pivot_wider(names_from = PRINCIPIO,
              values_from = cont)

write.csv(ags, paste(inp, "ags 2010-2018.csv", sep = "/"), row.names = F)

ag1 <- read.csv(paste(inp, "ags 2001-2007.csv", sep = "/"), stringsAsFactors = F)

ag1 <- ag1 %>% 
  mutate(cont = 1) %>% 
  group_by(partido, principio, legis, year) %>% 
  summarise_all(sum)

ag1 <- ag1 %>% 
  pivot_wider(names_from = principio,
              values_from = cont) %>% 
  select(partido, MR, RP, legis, year)

write.csv(ag1, paste(inp, "integracion 2001-2007.csv", sep = "/"), row.names = F)