rm(list = ls())
setwd("~")

library(dplyr)
library(stringr)
library(tidyr)

inp <- "/home/dhjs/Documentos/R_projects/mapas_poder_compartido"

uso <- read.csv(paste(inp, "oax_usos.csv", sep = "/"))

uso <- uso %>% 
  mutate(
    edo = 20,
    cve = str_pad(CLAVE.INEGI, width = 3, side = "left", pad = "0"),
    INEGI = paste0(edo, cve)
  ) %>% 
  select(INEGI, Org)

data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQYNRmqrZVq5rctj6BEj_aM_f9oQRY_G-xnDMIgyjsiMnpsiy_9HHlbbpzFwj7pEV-u4Rgy3LaUw55P/pub?gid=266728315&single=true&output=csv", stringsAsFactors = F)

data <- data %>% 
  select(1:3, 24:43) %>% 
  filter(Estado == "Oaxaca") %>% 
  mutate(INEGI = as.character(INEGI))

try <- full_join(data, uso, by = "INEGI")

try <- try %>% 
  mutate(
    X2000 = ifelse(X2000 == "" & !is.na(Org), "Usos y costumbres", X2000),
    X2001 = ifelse(X2001 == "" & !is.na(Org), "Usos y costumbres", X2001),
    X2002 = ifelse(X2002 == "" & !is.na(Org), "Usos y costumbres", X2002),
    X2003 = ifelse(X2003 == "" & !is.na(Org), "Usos y costumbres", X2003),
    X2004 = ifelse(X2004 == "" & !is.na(Org), "Usos y costumbres", X2004),
    X2005 = ifelse(X2005 == "" & !is.na(Org), "Usos y costumbres", X2005),
    
    X2006 = ifelse(X2006 == "" & !is.na(Org), "Usos y costumbres", X2006),
    X2007 = ifelse(X2007 == "" & !is.na(Org), "Usos y costumbres", X2007),
    X2008 = ifelse(X2008 == "" & !is.na(Org), "Usos y costumbres", X2008),
    X2009 = ifelse(X2009 == "" & !is.na(Org), "Usos y costumbres", X2009),
    X2010 = ifelse(X2010 == "" & !is.na(Org), "Usos y costumbres", X2010),
    
    X2011 = ifelse(X2011 == "" & !is.na(Org), "Usos y costumbres", X2011),
    X2012 = ifelse(X2012 == "" & !is.na(Org), "Usos y costumbres", X2012),
    X2013 = ifelse(X2013 == "" & !is.na(Org), "Usos y costumbres", X2013),
    X2014 = ifelse(X2014 == "" & !is.na(Org), "Usos y costumbres", X2014),
    X2015 = ifelse(X2015 == "" & !is.na(Org), "Usos y costumbres", X2015),
  
    X2016 = ifelse(X2016 == "" & !is.na(Org), "Usos y costumbres", X2016),
    X2017 = ifelse(X2017 == "" & !is.na(Org), "Usos y costumbres", X2017),
    X2018 = ifelse(X2018 == "" & !is.na(Org), "Usos y costumbres", X2018),
    X2019 = ifelse(X2019 == "" & !is.na(Org), "Usos y costumbres", X2019)
  ) %>% 
  select(-Org)

write.csv(try, paste(inp, "oax_correct.csv", sep = "/"), row.names = F)
  