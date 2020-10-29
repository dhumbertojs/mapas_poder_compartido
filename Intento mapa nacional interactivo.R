library(dplyr)
library(ggplot2)
library(stringr)
library(sf)

inp <- "/Users/dhjs/Documents/projects/mapas_poder_compartido"
mapas <- "/Users/dhjs/Documents/projects/mapas_poder_compartido/mapas"

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
  "VR" = "#df1b79",
  "NA" = "black")


x2000 <- ggplot(d_mapa) +
  geom_sf(aes(fill = X2000), color = "black") +
  scale_fill_manual(values = party, 
                    name = "Partidos") +
  theme_classic()
