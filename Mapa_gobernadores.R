rm(list=ls())
setwd("~")

####Directorios y paquetes####
inp <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAJ22OR9JCNfRdQFeHUO1szhw-tje4sjtMCMf3m9DA5mKjoWMNCWi4Pr3SPeO1TEb5wvvVZArw6ecq/pub?gid=1686758032&single=true&output=csv"
out <- "/R projects/mapas_poder_compartido"#/Gubernaturas"
maps <- "/R projects/Mapas (INEGI,shp)/conjunto de datos"

library(dplyr)
library(rgdal)
library(stringr)
library(ggplot2)
library(magick)
library(purrr)
####Llamar y limpiar data####
gob <- read.csv(url(inp), stringsAsFactors = F)
gob$INEGI <- formatC(gob$INEGI, width = 2, format = "d", flag = "0")

elec <- 1973:2019
gober <- data.frame()
pb = txtProgressBar(min=1, max=length(elec), style=3)
for (x in 1:length(elec)) {
  tempo <- gob %>% select(INEGI, ends_with(as.character(elec[x]))) %>% mutate(year = elec[x])
  names(tempo) <- c("INEGI", "win", "year")
  gober <- bind_rows(gober, tempo)
  setTxtProgressBar(pb, x)
  rm(tempo)
}

gober$win = sapply(strsplit(gober$win, "_"), "[", 1)
gob_first <- select(gob, INEGI)

for(x in 1:length(elec)) {
  tempo = filter(gober, year == elec[x]) %>% select(-year)
  gob_first = full_join(gob_first, tempo, by = "INEGI")
  rm(tempo)
  setTxtProgressBar(pb, x)
}

elec <- 1972:2019
for(x in 2:length(elec)) {
  colnames(gob_first)[x] <- elec[x]
}
####Llamar shp####
estados <- 1:31
x <- 1
y <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
       "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32")
nombre <- paste(y, "ent.shp", sep = "")
rep <- readOGR(paste(maps, "01ent.shp", sep = "/")) ##No supe poner un SP vacio, 
pb = txtProgressBar(min=1, max=length(nombre), style=3)

for (x in 1:length(estados)){
  tempo <- readOGR(paste(maps, nombre[x], sep = "/"))
  rep <- rbind(rep, tempo)
  setTxtProgressBar(pb, x)
  rm(tempo)
}
####Unir data y mapa####
rep@data$id <- rownames(rep@data)
mapa <- merge(rep, gob_first, by.x = "CVE_ENT", by.y = "INEGI")
map <- fortify(mapa, region = "id")
fin <- merge(map, mapa@data, by = "id")
####Plotear####
party <- c("INDEP" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000", "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44")
pb = txtProgressBar(min=1, max=length(fin), style=3)
elec <- 1973:2019
dada <- as.character(1963:2019)
for (x in 11:length(fin)) {
  try <- ggplot() + 
    geom_polygon(data = fin, aes(long, lat, group = group, fill = unlist(fin[x])), color = "#FFFFFF") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    scale_fill_manual(values = party, limit = levels(as.factor(unlist(fin[x]))), name = "Partido") +
    labs(title = "Gobernadores", subtitle = as.character(colnames(fin[x])), col = "Partido", x = "", y = "") +
    coord_fixed(1.4)
  ggsave(paste(dada[x], ".png", sep = ""), path = out, dpi = 300)
  setTxtProgressBar(pb, x)
  rm(try)
}
####Animacion/GIF####
list.files(path = out, pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"gubernaturas_1973-2019.gif", sep = "/"))