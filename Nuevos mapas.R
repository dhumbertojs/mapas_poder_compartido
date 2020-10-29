###Mapas y gifs

library(tidyverse)
library(sf)
library(magick)

inp <- "/Users/dhjs/Documents/projects/mapas_poder_compartido"
mapas <- "/Users/dhjs/Documents/projects/mapas_poder_compartido/mapas" ##Archivos shapefile
out <- "/Users/dhjs/Documents/projects/mapas_poder_compartido/visualizaciones" ##Directorio principal para guardar los mapas

origen <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQYNRmqrZVq5rctj6BEj_aM_f9oQRY_G-xnDMIgyjsiMnpsiy_9HHlbbpzFwj7pEV-u4Rgy3LaUw55P/pub?gid=266728315&single=true&output=csv"  ##Base de datos del poder compartido a nivel municipal

mex <- st_read(paste(mapas, "01_32_mun.shp", sep = "/")) ##Archivos para los mapas

datos <- read.csv(origen, fileEncoding = "UTF-8") %>% 
  mutate(
    INEGI = formatC(INEGI, width = 5, flag = "0")
  ) %>% 
  select(1:3, 24:43) %>% 
  mutate(
    Estado = str_replace_all(Estado, "_", " "),
    X2000 = sapply(strsplit(X2000, "_"), "[", 1),
    X2001 = sapply(strsplit(X2001, "_"), "[", 1),
    X2002 = sapply(strsplit(X2002, "_"), "[", 1),
    X2003 = sapply(strsplit(X2003, "_"), "[", 1),
    X2004 = sapply(strsplit(X2004, "_"), "[", 1),
    X2005 = sapply(strsplit(X2005, "_"), "[", 1),
    X2006 = sapply(strsplit(X2006, "_"), "[", 1),
    X2007 = sapply(strsplit(X2007, "_"), "[", 1),
    X2008 = sapply(strsplit(X2008, "_"), "[", 1),
    X2009 = sapply(strsplit(X2009, "_"), "[", 1),
    X2010 = sapply(strsplit(X2010, "_"), "[", 1),
    X2011 = sapply(strsplit(X2011, "_"), "[", 1),
    X2012 = sapply(strsplit(X2012, "_"), "[", 1),
    X2013 = sapply(strsplit(X2013, "_"), "[", 1),
    X2014 = sapply(strsplit(X2014, "_"), "[", 1),
    X2015 = sapply(strsplit(X2015, "_"), "[", 1),
    X2016 = sapply(strsplit(X2016, "_"), "[", 1),
    X2017 = sapply(strsplit(X2017, "_"), "[", 1),
    X2018 = sapply(strsplit(X2018, "_"), "[", 1),
    X2019 = sapply(strsplit(X2019, "_"), "[", 1)
  ) ##transformacion de los datos

d_mapa <- mex %>% 
  left_join(datos, by = c("CVEGEO" = "INEGI"))
##Unir datos de mapa con datos de municipios

tab <- tibble()
i <- 1
for (i in 1:32) {
  dada <- data.frame("edos" = i,
                    "anios" = c("X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006",
                                "X2007", "X2008", "X2009", "X2010", "X2011", "X2012", "X2013",
                                "X2014", "X2015", "X2016", "X2017", "X2018", "X2019"))
  tab <- rbind(tab, dada)
} ##Loop para crear una base de datos con los estados y los anios
#Esto es muy importante para que el purrr::map funcione

tab <- tab %>% 
  mutate(
    edos = formatC(edos, width = 2, flag = "0"),
    name = paste0(edos, anios, ".png")
  ) ##Transformacion de datos para el map

map(levels(as.factor(tab$edos)), 
    ~ dir.create(paste(out, .x, sep = "/"))) #rear los directorios

party <- c("INDEP" = "#ff3399", "MC" = "#ff8000", "Morena" = "#b30000",
           "PAN" = "#005ce6", "PRD" = "#ffff00", "PRI" = "#ff0000", 
           "PVEM" = "#00cc44","NPP" = "#8C0F6D", "PCP" = "#DC551E",    
           "PDM" = "#CE0536", "PES" = "#8701F7",    "PNA" = "#01F7CA",
           "PT" = "#DD0FB4")
##paleta de colores para los partidos politicos

plots <- map2(tab$edos, tab$anios, 
              ~ d_mapa %>% 
                filter(CVE_ENT == .x) %>% 
                select(names(mex), Estado, .y) %>% 
                ggplot() +
                geom_sf(aes_string(fill = .y), color = "black") +
                scale_fill_manual(values = party, 
                                  name = "Partidos") +
                theme_classic() +
                labs(title = d_mapa$Estado,
                     subtitle = levels(as.factor(str_remove_all(.y, "X")))
                     )
              )
##Lista con las mapas por estado y anio

walk2(plots, tab$name, 
      ~ ggsave(filename = .y, 
               plot = .x, 
               path = paste(out, str_sub(.y,1,2), sep = "/"),
               dpi = 300)
      )
##guardar cada mapa en su carpeta con su nombre corresponiente

# Hacer los gifs!!! -------------------------------------------------------

map(tab$edos, 
     ~ list.files(
       path = paste(out, .x, sep = "/"),
       pattern = "*.png",
       full.names = T
       ) %>% 
       image_read() %>% 
       image_join() %>% 
       image_animate(fps = 1) %>% 
       image_write(
         path = paste(out, .x, paste0(.x, ".gif"), sep = "/")
         )
     )











