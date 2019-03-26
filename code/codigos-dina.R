library(rvest)
library(stringi)

rutaBase <- "https://dina.concytec.gob.pe/appDirectorioCTI/BuscarInvestigadores.do?tipo=investigadores&origen=cabBusqueda&apellidos="

rutas <- paste0(rutaBase, letters)

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

codigos <- lapply(rutas, dameTabla)

codigos <- unique(unlist(codigos))

save(codigos, file = "codigos.rds")