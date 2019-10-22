setwd("~")

####Directorios y paquetes####
inp <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSJkpFRyYSaghOiCEmBPRidWKd0EPlkyV26-91ZpV_UbVldUEMQY4IA5eKo-CvzrWgHYKgW9UU3BQ8z/pub?output=csv"
out <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido/Senadurías"

library(ggplot2)
library(ggparliament)
library(dplyr)
library(purrr)
library(magick)
####Graficas####
sen <- read.csv(inp, stringsAsFactors = F, fileEncoding = "UTF-8")
sen <- arrange(sen, by = year)
#levels(as.factor(sen$Legislatura))
party <- c("Sin_Grupo" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7", "PNA" = "#01F7CA",
           "PT" = "#DD0FB4", "Conv" = "#ff8000", "PAS" = "#800000", 
           "PSD" = "#ff6666", "PSN" = "#a64dff", "INDEP" = "#666699",
           "Convergencia" = "#ff8000")

##a esto le hace falta un for##
x <- 1
legis <- c("2000", "2003", "2006", "2009", "2012", "2015", "2018")
orden <- as.character(1:7)
pb = txtProgressBar(min=1, max=length(legis), style=3)

for (x in 1:length(legis)) {
  tempo <- sen %>% filter(year == legis[x])
  sena <- parliament_data(tempo, 4, tempo$Total, type = "semicircle", tempo$GP)
  plot <- ggplot(sena, aes(x, y, colour = GP, size = 2L)) +
    geom_parliament_seats() + 
    theme_ggparliament() +
    scale_color_manual(values = party) +
    labs(title = paste(levels(as.factor(sena$Legislatura)), "Legislatura", sep = " "), 
         subtitle = levels(as.factor(sena$year)))
  ggsave(paste(orden[x], "legislatura_", legis[x], ".png", sep = ""), path = out, dpi = 300)
  rm(tempo,sena, plot)
  setTxtProgressBar(pb, x)
}
####Animacion####
list.files(path = out, pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 0.5) %>% 
  image_write(paste(out,"camara_alta.gif", sep = "/"))
