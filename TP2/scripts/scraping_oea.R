library(tidyverse)  # Manipulación de datos
library(rvest)      # Web scraping
library(tidytext)   # Análisis de texto
library(udpipe)     # Lematización
library(here)       # Manejo de rutas de archivos
library(xml2)       # Manejo de HTML 

#descargamos las librerías necesarias

link_oea <-  "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=4&nAnio=2026" 

oea <- read_html(link_oea)
data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  dir.create(data_dir)
} else {
  cat(
    "'data' ya existe"
  )
}
attr(oea, "fecha_descarga") <- Sys.time()

write_html(
  oea,
  file = file.path(data_dir, "oea_abril_2026.html"))

saveRDS(oea, file = "oea_abril_2026.rds")

meses <- 1:4
nombres_meses <- c("enero", "febrero", "marzo", "abril")
titulos <- list()


for (i in seq_along(meses)) {
  link_oea <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    meses[i],
    "&nAnio=2026"
  )
  
  oea <- read_html(link_oea)
  
  titulos[[i]] <- oea |>
    html_elements(".itemmenulink") |>
    html_text2() |>
    str_trim()
    Sys.sleep(3)
}



links_totales <- c()
cuerpos <- list()

for (mes in meses) {
  url_mes <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    mes,
    "&nAnio=2026"
  )
  
  mes_lectura <- read_html(url_mes)
  
  links_mes <- mes_lectura |>
    html_elements("a.itemmenulink") |>
    html_attr("href")
  
  links_mes <- links_mes[str_detect(
    links_mes,
    "^(comunicado_prensa\\.asp|fotonoticia\\.asp)"
  )]
  
  links_mes <- paste0(
    "https://www.oas.org/es/centro_noticias/",
    links_mes
  )
  
  links_totales <- c(links_totales, links_mes)
  Sys.sleep(3)
}
for (i in seq_along(links_totales)) {
    link_body <- read_html(links_totales[[i]]) 
    
    cuerpo <- link_body |>
      html_elements("div p") |>
      html_text2() |>
      str_trim()
    
    cuerpo <- cuerpo[
      cuerpo != "" &
        !str_detect(cuerpo, "^Contáctenos:") &
        !str_detect(cuerpo, "^Referencia:")
    ]
    
    cuerpo <- paste(cuerpo, collapse = " ")
    
    cuerpos[[i]] <- cuerpo
    
   
    Sys.sleep(3)
}


id <- str_remove(links_totales, ".*sCodigo=")

length(titulos)
length(cuerpos)

titulos_todos <- unlist(titulos)
cuerpos_todos <- unlist(cuerpos)

length(titulos_todos)
length(cuerpos_todos)
length(id)
tabla_final <- tibble(
  id = id,
  titulo = titulos_todos,
  cuerpo = cuerpos_todos
)


saveRDS(
  tabla_final,
  file = file.path(data_dir, "comunicados_oea_crudo.rds")
)

tabla_final
