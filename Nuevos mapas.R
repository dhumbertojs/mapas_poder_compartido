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


n_edos <- levels(as.factor(d_mapa$Estado))

tab <- cbind(tab, n_edos)

map(levels(as.factor(tab$edos)), 
    ~ dir.create(paste(out, .x, sep = "/"))) #rear los directorios

party <- c(
  "AVE" =   "#50007d",             
    "CEDEM" = "#feca00",
    "Consejo Municipal" = "#e83c54",
    "Convergencia" = "#ff8000",
    "CU" = "#028cb3",
    "FC" = "#008458",
    "Humanista" = "#9f3b77",
    "Independiente" = "#8f1e64",
    "MAS" = "#A32A0A",      
    "MC" = "#ff8300",
    "Morena" = "#b30000",
    "Mover" = "#763a92",
    "MP" = "#00B4A1",
    "NPP" = "#8C0F6D",
    ##NX = #0f2b8e estos mejor que se queden como NA
    "PAC" = "#3e3564",
    "PAN" = "#005ce6",
    "PAS" = "#da3927",
    "PASDC" = "#ff0000",
    "PAZ" = "#dbe231",
    "PCM" = "#fff200",
    "PCP" = "#DC551E",
    "PD" = "#00a3e6",
    "PDM" = "#CE0536",
    "PDUR" = "#213e80",
    "PEC" = "#07a9b5",
    "PES" = "#8701F7",
    "PFCRN" = "#cf0000",
    "PJS" = "#0876bd",
    "PMC" = "#2d5238",
    "PMR" = "#e0117e",
    "PNA" = "#01F7CA",
    "POCH" = "#825496",
    "PPG" = "#9e000e",
    "PPS" = "#e6488b",
    "PPT" = "#f5e6b8",
    "PRD" = "#ffff00",
    "PRI" = "#ff0000",
    "PRS" = "#d44191",
    "PRV" = "#4db135",
    "PS" = "#0d4592",
    "PSD" = "#e02128",
    "PSI" = "#fcc604",
    "PSN" = "#6d0e74",
    "PT" = "#DD0FB4",
    "PUP" = "#a1d29a",
    "PVEM" = "#00cc44",
    "UDC" = "#f18634",
    "UDEMOR" = "#e34144",
    "Usos y costumbres" = "#1687bc",
    "VR" = "#df1b79"
  )
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
                labs(#title = "",
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
prueba <- levels(as.factor(tab$edos)) %>% 
  as.data.frame()

# map(prueba,
#      ~ list.files(
#        path = paste(out, .x, sep = "/"),
#        pattern = "*.png",
#        full.names = T
#        ) %>%
#        image_read() %>%
#        image_join() %>%
#        image_animate(fps = 1) %>%
#        image_write(
#          path = paste(out, .x, paste0(.x, ".gif"), sep = "/")
#          )
#      )

list.files(path = paste(out, "01", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"01", "01.gif", sep = "/"))

list.files(path = paste(out, "02", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"02", "01.gif", sep = "/"))

list.files(path = paste(out, "03", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"03", "01.gif", sep = "/"))

list.files(path = paste(out, "04", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"04", "01.gif", sep = "/"))

list.files(path = paste(out, "05", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"05", "01.gif", sep = "/"))

list.files(path = paste(out, "06", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"06", "01.gif", sep = "/"))

list.files(path = paste(out, "07", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"07", "01.gif", sep = "/"))

list.files(path = paste(out, "08", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"08", "01.gif", sep = "/"))

list.files(path = paste(out, "09", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"09", "01.gif", sep = "/"))

list.files(path = paste(out, "10", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"10", "01.gif", sep = "/"))

list.files(path = paste(out, "11", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"11", "01.gif", sep = "/"))

list.files(path = paste(out, "12", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"12", "01.gif", sep = "/"))

list.files(path = paste(out, "13", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"13", "01.gif", sep = "/"))

list.files(path = paste(out, "01", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"01", "01.gif", sep = "/"))

list.files(path = paste(out, "15", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"15", "01.gif", sep = "/"))

list.files(path = paste(out, "16", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"16", "01.gif", sep = "/"))

list.files(path = paste(out, "17", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"17", "01.gif", sep = "/"))

list.files(path = paste(out, "18", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"18", "01.gif", sep = "/"))

list.files(path = paste(out, "19", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"19", "01.gif", sep = "/"))

list.files(path = paste(out, "20", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"20", "01.gif", sep = "/"))

list.files(path = paste(out, "21", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"21", "01.gif", sep = "/"))

list.files(path = paste(out, "22", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"22", "01.gif", sep = "/"))

list.files(path = paste(out, "23", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"23", "01.gif", sep = "/"))

list.files(path = paste(out, "24", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"24", "01.gif", sep = "/"))

list.files(path = paste(out, "25", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"25", "01.gif", sep = "/"))

list.files(path = paste(out, "26", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"26", "01.gif", sep = "/"))

list.files(path = paste(out, "27", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"27", "01.gif", sep = "/"))

list.files(path = paste(out, "28", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"28", "01.gif", sep = "/"))

list.files(path = paste(out, "29", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"29", "01.gif", sep = "/"))

list.files(path = paste(out, "30", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"30", "01.gif", sep = "/"))

list.files(path = paste(out, "31", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"31", "01.gif", sep = "/"))

list.files(path = paste(out, "32", sep = "/"), pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% #función de Purrr(map), las otras son funciones de magick
  image_join() %>% 
  image_animate(fps = 1) %>% 
  image_write(paste(out,"32", "01.gif", sep = "/"))