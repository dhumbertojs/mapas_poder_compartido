rm(list=ls())
setwd("~")

####Directorios y paquetes####
inp <- "/Users/Emmanuel RB/Documents/El Poder Compartido II"
out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido/Ayuntamientos/SLP"
maps <- "/home/dhjs/Documentos/R_projects/Mapas (INEGI,shp)"

library(rgdal)
library(stringr)
library(dplyr)
library(ggplot2)
library(purrr)
library(magick)
####Llamar y limpiar datos####
slp <- read.csv(paste(inp, "EPC2 - Municipios (1980-2019).csv", sep = "/"), fileEncoding = "UTF-8", stringsAsFactors = F)
slp <- slp %>% filter(Estado == "San_Luis_Potosi") %>% select(-c(4:23))

elec <- 2000:2019
san <- data.frame()
x <- 1
pb = txtProgressBar(min=1, max=length(elec), style=3)

for (x in 1:length(elec)) {
  tempo <- slp %>% select(INEGI, ends_with(as.character(elec[x]))) %>% mutate(year = elec[x])
  names(tempo) <- c("INEGI", "win", "year")
  san <- bind_rows(san, tempo)
  setTxtProgressBar(pb, x)
  rm(tempo)
}

san$win = sapply(strsplit(san$win, "_"), "[", 1)
slp <- slp %>% select(Municipio, INEGI)

for(x in 1:length(elec)) {
  tempo = filter(san, year == elec[x]) %>% select(-year)
  slp = full_join(slp, tempo, by = "INEGI")
  rm(tempo)
  setTxtProgressBar(pb, x)
}

elec <- 1998:2019
for(x in 3:length(elec)) {
  colnames(slp)[x] <- elec[x]
}

slp <- slp %>% mutate(
  CVE_MUN = substr(INEGI, 3, 5)
) %>% select(-c("Municipio", "INEGI"))
####Abrir shp####
map <- readOGR(paste(maps, "24mun.shp", sep = "/"))
map@data$id <- rownames(map@data)
mapa <- merge(map, slp, by.x = "CVE_MUN", by.y = "CVE_MUN")
map_slp <- fortify(mapa, region = "id")
fin <- merge(map_slp, mapa@data, by = "id")
####Plotear####
party <- c("INDEP" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7",    "PNA" = "#01F7CA",
           "PT" = "#DD0FB4")
pb = txtProgressBar(min=1, max=length(fin), style=3)
elec <- 1989:2019
dada <- as.character(1989:2019)
for (x in 12:length(fin)) {
  try <- ggplot() + 
    geom_polygon(data = fin, aes(long, lat, group = group, fill = unlist(fin[x])), color = "#FFFFFF") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()) +
    scale_fill_manual(values = party, limit = levels(as.factor(unlist(fin[x]))), name = "Partido") +
    labs(title = "Alcaldías de San Luis Potosí", subtitle = as.character(colnames(fin[x])), col = "Partido", x = "", y = "") +
    coord_fixed(1.1)
  ggsave(paste(dada[x], ".png", sep = ""), path = out, dpi = 300)
  setTxtProgressBar(pb, x)
  rm(try)
}
####Animacion/GIF####
list.files(path = out, pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"SLP_2000-2019.gif", sep = "/"))
