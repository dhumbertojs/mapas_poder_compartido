rm(list=ls())
setwd("~")

####Directorios y paquetes####
inp <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRA-4N91VihIrGzX5fEvErf3OvmsVwYKvD-6CF4Eq0Cmr4Xnb9I53ZaPPu07Vvk6YSbnK6l_bB-vchU/pub?gid=173991573&single=true&output=csv"
out <- "/Users/Emmanuel RB/Documents/El Poder Compartido II/Output/mapas_poder_compartido/Diputaciones"

library(ggplot2)
library(ggparliament)
library(dplyr)
library(purrr)
library(magick)
####Graficas####
dip <- read.csv(inp, stringsAsFactors = F, fileEncoding = "UTF-8")
dip <- arrange(dip, by = year)
#levels(as.factor(dip$Legislatura))
party <- c("Sin_Grupo" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7", "PNA" = "#01F7CA",
           "PT" = "#DD0FB4", "Conv" = "#ff8000", "PAS" = "#800000", "PSD" = "#ff6666")

##a esto le hace falta un for##
x <- 1
legis <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018")
orden <- as.character(1:7)
pb = txtProgressBar(min=1, max=length(legis), style=3)

for (x in 1:length(legis)) {
  tempo <- dip %>% filter(year == legis[x])
  dipus <- parliament_data(tempo, 8, tempo$Total, type = "semicircle", tempo$GP)
  plot <- ggplot(dipus, aes(x, y, colour = GP, size = 3L)) +
    geom_parliament_seats() + 
    theme_ggparliament() +
    scale_color_manual(values = party) +
    labs(title = paste(levels(as.factor(dipus$Legislatura)), "Legislatura", sep = " "), 
         subtitle = levels(as.factor(dipus$year)))
  ggsave(paste(orden[x], "legislatura_", legis[x], ".png", sep = ""), path = out, dpi = 300)
  rm(tempo,dipus, plot)
  setTxtProgressBar(pb, x)
}
####Animacion####
list.files(path = out, pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"camara_baja.gif", sep = "/"))