library(here)
library(tidyverse)
library(readxl)
library(stringi)

# cargar datos
convocatoria <- read_xlsx(here("in", "Postulantes-y-ganadores-FONDOS.xlsx"))
names(convocatoria) <- c("postulante", "entidad", "proyecto",
                         "investigacion", "region", "modalidad",
                         "area", "ano", "tipo", "presupuesto")

perfil <- read_csv(here("in","infoPerfiles.csv"))

# unir
load("out/codigos-excel-raw.rds")
ncod <- unlist(lapply(codigos, length))

convocatoria[which(ncod == 1), "cod_dina"] <- unlist(codigos[which(ncod == 1)])
convocatoria[which(ncod > 1), "cod_dina"] <- c("34844", "82007", "34844")
convocatoria[which(ncod == 0), "cod_dina"] <-  c("11914", "1980", "39802", "33982", "23205", "3499", "28604",  "12557", "77797", "12160", "451", "2261", "47842", "10871", "202", "46398", "12655", "73389", "10064", "12261", "13089", "38375","10108", "1863", "1203", "16975", "16821", "1449", "53272", "10029", "38316", "3266", "47780", "14722", "59762", "52576", "61480", "4618", "47780", "63856", "59762", "16821", "38375", "10029", "38316", "3266", "14722", "61480", "52576", "47780", "59762")
convocatoria$cod_dina <- as.numeric(convocatoria$cod_dina)

full <- convocatoria %>% left_join(perfil, by = c("cod_dina" = "id"))

# limpiar
sin_posgrado <- which(full$maestria+full$doctorado == 0)
full[sin_posgrado, "maestria"] <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
full[sin_posgrado, "doctorado"] <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)

full <- full %>%
  mutate(presupuesto =
           stri_replace_all_regex(presupuesto, "[^[0-9]\\,\\.]", "") %>%
           stri_replace_all_regex("^\\.", "") %>%
           stri_replace_all_regex("\\,", "") %>%
           as.numeric)

full <- full %>%
  select(cod_dina, nombre, genero, maestria, doctorado, desde, ano, region, entidad, investigacion, modalidad, area, presupuesto, tipo)

write_csv(full, here("out", "tabla_final.csv"))