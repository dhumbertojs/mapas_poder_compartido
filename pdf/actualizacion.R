library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)

inp <- "/Users/dhjs/Documents/projects/mapas_poder_compartido/pdf"
list.files(inp)

lista <- c("X-LEGISLATURA.csv", "XII-LEGISLATURA.csv", "XIII-LEGISLATURA.csv",
           "XIV-LEGISLATURA.csv", "XV-LEGISLATURA.csv")

data <- paste(inp, "bcs", lista, sep ="/")

bcs <- map(
  data,
           ~ read.csv(.x) %>% 
             mutate(legis = as.character(.x))
  )

d1 <- bcs[1]
d2 <- bcs[2]
d3 <- bcs[3]
pre <- bind_rows(d1,d2,d3)
pre <- pre %>% 
  filter(CARÁCTER == "PROPIETARIO") %>% 
  mutate(legis = str_remove_all(legis, inp))

d4 <- bcs[4]
d5 <- bcs[5]
raw <- bind_rows(d4,d5)
names(raw) <- names(pre)
raw <- raw %>% 
  filter(DIPUTADO != "DIPUTADO" & CARÁCTER == "PROPIETARIO") %>% 
  mutate(legis = str_remove_all(legis, inp))

fin <- bind_rows(pre, raw) %>% 
  mutate(
    legis = str_remove_all(legis, "/"),
    legis = str_remove_all(legis, "-LEGISLATURA.csv"),
    SISTEMA = str_replace_all(SISTEMA, "Mayoria Relativa", "Mayoría Relativa"),
    SISTEMA = ifelse(SISTEMA == "Mayoría Relativa", "MR", "RP")
    ) %>% 
  group_by(PARTIDO, SISTEMA, legis) %>% 
  count() %>% 
  pivot_wider(
    names_from = SISTEMA, 
    values_from = n
  ) %>% 
  select(PARTIDO, MR, RP, legis) %>% 
  write.csv(paste(inp, "bcs", "bcs.csv", sep = "/"), row.names = F)

camp <- read.csv(paste(inp, "camp", "campeche.csv", sep = "/"))

camp <- camp %>% 
  select(-Diputado.Suplente) %>% 
  mutate(
    principio = ifelse(Distrito == "", "RP", "MR")
  ) %>% 
  group_by(Partido, principio, legis) %>% 
  count() %>% 
  pivot_wider(
    names_from = principio,
    values_from = n
  ) %>% 
  select(Partido, MR, RP, legis) %>% 
  write.csv(paste(inp, "camp", "camp_final.csv", sep = "/"), row.names = F)

chis <- read.csv(paste(inp, "chis", "tabula-RITLBC145884.csv", sep = "/"))

chis <- chis %>% 
  mutate(
    year = 2012,
    legis = "LXV"
  ) %>% 
  group_by(X, PRINCIPIO, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = PRINCIPIO,
    values_from = n
  ) %>% 
  select(X, MR, RP, legis, year) %>% 
  write.csv(paste(inp, "chis", "chiapas_final.csv", sep = "/"), row.names = F)

cLXVII <- read.csv(paste(inp, "chis", "LXVII.csv", sep = "/"))

cLXVII <- cLXVII %>% 
  mutate(
    year = 2018,
    legis = "LXVII",
    partido = substring(partido, 3), #este no necesita tener parametro de terminar
    prin = ifelse(muni == "Plurinominal", "RP", "MR")
  ) %>% 
  group_by(partido, year, legis, prin) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, RP, legis, year) %>% 
  write.csv(paste(inp, "chis", "legis2018.csv", sep = "/"), row.names = F)

ch1 <- read.csv(paste(inp, "chih", "diputadosLeg65.csv", sep = "/"))
ch2 <- read.csv(paste(inp, "chih", "diputadosLeg66.csv", sep = "/"))
chih <- bind_rows(ch1, ch2) %>% 
  mutate(
    prin = ifelse(Distrito == "Representación Proporcional", "RP", "MR")
  ) %>% 
  group_by(Partido, prin, Legislatura) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(Partido, MR, RP, Legislatura) %>% 
  write.csv(paste(inp, "chih", "chihuahua_fin.csv", sep = "/"), row.names = F)

dcol <- c("54.csv", "55.csv", "56.csv")

listcol <- paste(inp, "col", dcol, sep = "/")

mapcol <- map(
  listcol,
  ~ read.csv(.x)
)

c1 <- bind_rows(mapcol[1]) %>% 
  mutate(
    year = "2003",
    legis = "LIV"
  ) %>% 
  select(-SUPLENTE)
c2 <- bind_rows(mapcol[2]) %>% 
  mutate(
    year = "2006",
    legis = "LV"
  ) 
c3 <- bind_rows(mapcol[3])
c3 <- c3 %>% 
  select(FRA, DIST, NOMBRE.DIPUTADO.A.) %>% 
  mutate(
    year = "2009",
    legis = "LVI"
  )
names(c3) <- names(c2)

col <- bind_rows(c1, c2, c3) %>% 
  mutate(
    prin = ifelse(DISTRITO == "PLURI", "RP", "MR")
  ) %>% 
  group_by(PARTIDO, legis, year, prin) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(PARTIDO, MR, RP, legis, year) %>% 
  write.csv(paste(inp, "col", "colima_primero.csv", sep = "/"), row.names = F)

col15 <- read.csv(paste(inp, "col", "2015.csv", sep = "/"))
col15 <- col15 %>% 
  mutate(dto = str_replace_all(dto, "Plurinomnal", "Plurinominal"),
         prin = ifelse(dto == "Plurinominal", "RP", "MR")) %>% 
  group_by(ï..partido, prin) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  write.csv(paste(inp, "col", "col2015_fin.csv", sep = "/"), row.names = F)

edomexlvi <- read_excel(paste(inp, "lvi edomex.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(!is.na(Distrito), "MR", "RP")
  ) %>% 
  group_by(Partido, prin) %>% 
    count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  write.csv(paste(inp, "lvi_edomex_summ.csv", sep = "/"), row.names = F)

gto <- read_excel(paste(inp, "GUANAJUATO.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "RP", "MR"),
    partido = str_replace_all(partido, c(
      "Partido del Trabajo" =  "PT", "Movimiento Ciudadano" = "MC", 
      "MoCi" = "MC", "Nueva Alianza" = "PNA", "PANAL" = "PNA"))
    ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  arrange(year) %>% 
  select(
    partido, MR, RP, legis, year
  ) %>% 
  write.csv(paste(inp, "Guanajuato_final.csv", sep = "/"), row.names = F)

jal <- read_excel(paste(inp, "JALISCO.xlsx", sep = "/")) %>% 
  mutate(prin = ifelse(is.na(distrito), "RP", "MR")) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin, 
    values_from = n
  ) %>% 
  select(partido, MR, RP, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "Jalisco_final.csv", sep = "/"), row.names = F)

mich <- read_excel(paste(inp, "MICH.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "Michocan_final.csv", sep = "/"), row.names = F)

nay <- read_excel(paste(inp, "NAYARIT.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "Nayarit_final.csv", sep = "/"), row.names = F)

nl <- read_excel(paste(inp, "NL.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "nuevo leon_final.csv", sep = "/"), row.names = F)


oax <- read_excel(paste(inp, "OAXACA.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "oaxaca_final.csv", sep = "/"), row.names = F)


qroo <- read_excel(paste(inp, "QROO.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "quintana_final.csv", sep = "/"), row.names = F)

slp <- read_excel(paste(inp, "SLP.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "slp_final.csv", sep = "/"), row.names = F)

son <- read_excel(paste(inp, "SONORA.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "sonora_final.csv", sep = "/"), row.names = F)

tab <- read_excel(paste(inp, "TAB.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "tabasco_final.csv", sep = "/"), row.names = F)

tamps <- read_excel(paste(inp, "TAMAULIPAS.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "tamaulipas_final.csv", sep = "/"), row.names = F)

tlax <- read_excel(paste(inp, "TLAXCALA.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "tlaxcala_final.csv", sep = "/"), row.names = F)

ver <- read_excel(paste(inp, "VERACRUZ.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "veracruz_final.csv", sep = "/"), row.names = F)

yuc <- read_excel(paste(inp, "YUCATAN.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "yucatan_final.csv", sep = "/"), row.names = F)


zac <- read_excel(paste(inp, "ZACATECAS.xlsx", sep = "/")) %>% 
  mutate(
    prin = ifelse(is.na(distrito), "PR", "MR")
  ) %>% 
  group_by(partido, prin, legis, year) %>% 
  count() %>% 
  pivot_wider(
    names_from = prin,
    values_from = n
  ) %>% 
  select(partido, MR, PR, legis, year) %>% 
  arrange(year) %>% 
  write.csv(paste(inp, "zacatecas_final.csv", sep = "/"), row.names = F)
