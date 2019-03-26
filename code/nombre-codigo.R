# Buscar nombre de investigadorx en DINA y obtener su codigo

library(lubridate)
library(readxl)
library(dplyr)
library(stringi)
library(rvest)
library(here)

inpu <- here("in","Postulantes-y-ganadores-FONDOS.xlsx") %>% read_xlsx
rutaBase <- "https://dina.concytec.gob.pe/appDirectorioCTI/BuscarInvestigadores.do?tipo=investigadores&origen=cabBusqueda&apellidos="

dameTabla <- function(x){
  print(x)
  start.time <- Sys.time()
  
  x %>%
    read_html %>%
    html_nodes("td:nth-child(2) a") %>%
    html_attr("href") %>%
    stri_replace_all_regex("^.*\\?id\\_investigador\\=", "") -> done
  
  end.time <- Sys.time()
  print(end.time - start.time)
  
  return(done)
}

linkList <- inpu$POSTULANTE %>%
  # una funcion
  stri_replace_all_regex("[^[:alnum:] ]", "") %>%
  stri_replace_all_fixed(" ", "%20") %>%
  paste0(rutaBase, .)

#codigos <- lapply(linkList, dameTabla)
#here("out", "codigos-excel-raw.rds") %>% save(codigos, file = .)
load(here("out","codigos-excel-raw.rds"))

ncod <- unlist(lapply(codigos, length))

# Lista para mity a mano
#inpu$POSTULANTE[which(ncod == 0)]
# Lista de multiples
#inpu$POSTULANTE[which(ncod > 1)]

inpu[which(ncod == 1), "cod_dina"] <- unlist(codigos[which(ncod == 1)])
inpu[which(ncod > 1), "cod_dina"] <- c("34844", "82007", "34844")
inpu[which(ncod == 0), "cod_dina"] <-  c("11914", "1980", "39802", "33982", "23205", "3499", "28604",  "12557", "77797", "12160", "451", "2261", "47842", "10871", "202", "46398", "12655", "73389", "10064", "12261", "13089", "38375","10108", "1863", "1203", "16975", "16821", "1449", "53272", "10029", "38316", "3266", "47780", "14722", "59762", "52576", "61480", "4618", "47780", "63856", "59762", "16821", "38375", "10029", "38316", "3266", "14722", "61480", "52576", "47780", "59762")

# copiar todos los perfiles DINA
personaId <- inpu$cod_dina %>%
  unique

unicos <- personaId %>%
  paste0("http://directorio.concytec.gob.pe/appDirectorioCTI/VerDatosInvestigador.do?id_investigador=", .)

perfiles200 <- unicos[1:200] %>% lapply(function(x){print(x);read_html(x)})
perfiles400 <- unicos[201:400] %>% lapply(function(x){print(x);read_html(x)})
perfiles465 <- unicos[400:465] %>% lapply(function(x){print(x);read_html(x)})
perfiles <- c(perfiles200, perfiles400, perfiles465[-1])
# NO PUEDES GUARDAR LOS ARCHIVOS O___O

dameSecciones <- function(x){
  x %>%
    html_nodes(xpath = '//*[contains(@class,"titulo_fichas_dos")]') %>%
    html_text()
}

secciones <- unique(unlist(lapply(perfiles, dameSecciones)))
secciones <- tibble(id = 1:length(secciones), seccion = secciones)

verificaSecciones <- function(x){
  (secciones %>% pull(seccion)) %in% (x %>% dameSecciones)
}

perfilSeccion <- t(sapply(lapply(perfiles, verificaSecciones), rbind))
colnames(perfilSeccion) <- paste0("sec_",1:nrow(secciones))
perfilSeccion <- as_tibble(perfilSeccion)

tablaPersona <- tibble(id = personaId) %>%
  cbind(perfilSeccion)

dameGenero <- function(x) {
  a <- x %>% html_nodes(xpath = '//*[@id="datitos"]') %>% html_table(fill = TRUE)
  a[[1]][4, 2]
}

tablaPersona[tablaPersona$sec_1, "genero"] <- perfiles[tablaPersona %>% pull(sec_1)] %>% sapply(dameGenero)

tablaPersona %>%
  mutate()

dameNombre <- function(x) {
  x %>% html_nodes(xpath = '//*[contains(@class,"tituloNombreFicha")]') %>% html_text()
}

tablaPersona[, "nombre"] <- perfiles %>% sapply(dameNombre)

dameGrados <- function(x) {
  ntabaca <- which(dameSecciones(x) == (secciones[6,]$seccion))
  if(length(ntabaca) == 0){
    return(NA)
  } else {
    a <- x %>% html_nodes(xpath = '//*[@id="datitos"]') %>% html_table(fill = TRUE)
    return(a[[ntabaca]][, 1])
  }
}

b <- perfiles %>% lapply(dameGrados)

tablaPersona[, "maestria"] <- b %>% sapply(function(x){"MAGISTER" %in% x})
tablaPersona[, "doctorado"] <- b %>% sapply(function(x){"DOCTORADO" %in% x})

dameExperiencia <- function(x) {
  ntabaca <- which(dameSecciones(x) == (secciones[2,]$seccion))
  if(length(ntabaca) == 0){
    return(NA)
  } else {
    a <- x %>% html_nodes(xpath = '//*[@id="datitos"]') %>% html_table(fill = TRUE)
    return(a[[ntabaca]][-1, 4] %>% lubridate::ymd() %>% lubridate::year() %>% min)
  }
}

tablaPersona[, "desde"] <- perfiles %>% sapply(dameExperiencia)

tablaPersona %>%
  select(id, nombre, genero, desde, maestria, doctorado) %>%
  write.csv("infoPerfiles.csv", row.names = FALSE)