library(dplyr)
library(ggplot2)
library(stringr)

inp <- paste(getwd(), "Congresos locales", sep = "/")

data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRvdghVEmPfLMmo7LKcw2z9FbnL8MjIlJ1LPaNqqN1gtgwPXpDq2rkWLcFYPMaO4qivzgFNx_0u0KGN/pub?gid=1423196126&single=true&output=csv", 
                 stringsAsFactors = F, 
                 encoding = "UTF-8")

levels(as.factor(data$Partido))


try <- data %>%
  mutate(
    total = rowSums(cbind(MR, RP), na.rm = T)
  ) %>% 
  group_by(Estado, year, Legislatura) %>% 
  summarise(
    legis = sum(total, na.rm = T)
  ) %>% 
  write.csv(paste(inp, "total legis.csv", sep = "/"), row.names = F)
