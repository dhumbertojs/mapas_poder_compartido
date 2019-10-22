rm(list = ls())
setwd("~")

library(dplyr)
library(tidyr)

out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido"

data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQYNRmqrZVq5rctj6BEj_aM_f9oQRY_G-xnDMIgyjsiMnpsiy_9HHlbbpzFwj7pEV-u4Rgy3LaUw55P/pub?gid=266728315&single=true&output=csv", stringsAsFactors = F)

tidy <- data %>% 
  select(1:3, 24:43) %>% 
  pivot_longer(
    X2000:X2019, 
    names_to = "Anio", 
    values_to = "Ganador", 
    names_prefix = "X")

#Candelaria, Camp; no tuvo elecciones? 
#Igual y siempre ganó el PRI

try <- tidy %>% 
  filter(Ganador == "")
table(try$Estado)

#filter(!str_detect(filtro, "Sección") & !str_detect(filtro, "Básica") 
##Filtro para renglones con texto!!
table(tidy$Estado)

table(try$Estado)