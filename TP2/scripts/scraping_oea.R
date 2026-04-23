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

# abrimos el link de la página de OEA y lo leemos, y luego pedimos que cree 
# la carpeta data si es que esta no existe

write_html(
  oea,
  file = file.path(data_dir, "oea_abril_2026.html"))



#guardamos el archivo html en la carpeta data

meses <- 1:4
nombres_meses <- c("enero", "febrero", "marzo", "abril")
titulos <- list()

# creamos un vector llamado meses para luego recorrerlo con un for, y una lista
# vacía en donde irán los links de títulos de las noticias de cada mes. También la lista de links totales y 
# de cuerpos, explicadas más adelante
links_totales <- c()
cuerpos <- list()

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

# pedimos que por cada mes se genere su respectivo link de "entrada",
# y posteriormente recorremos ese link de "x" mes para extraer el conjunto de
# títulos de ese mes y agregar ese conjunto como elemento de la lista títulos





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

# luego, recorriendoleyendo nuevamente "meses" para tener cada link de su respectivo mes, 
# se usa selector gadget para identificar el selector CSS correspondiente a los títulos de las noticias
# y se usa el atributo href para obtener los links de cada noticia. Después, se
# arman los links de las noticias de cada mes. Primero se pide que detecte a 
# los links que poseen el fragmento de "comunicado de prensa" o "fotonoticia" 
# ya que sabemos que se tratan de noticias que poseen texto analizable. Posteriormente
# mediante la concanetación de este filtro armamos los links de cada notica y los
# almacenamos (todos) en la variable links totales


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

# en este bloque for, recorremos todos los links de las noticias y los leemos
# para extraer el selector CSS del cuerpo (mediante Selector gadget), y así
# leer los cuerpos de las noticias. Previamente hacemos una breve limpieza 
# de "contactenos" y "referencia" para eliminarlos ya que quedaban dentro
# del selector. En la variable cuerpo almacenamos el cuerpo de cada noticia
# y lo incluimos como elemento dentro de la lista vacía llamada cuerpos. 

id <- str_remove(links_totales, ".*sCodigo=")

# creamos la variable id para tener el id de cada noticia. Removemos 
# la mayor parte del link de cada noticia hasta sCódigo que es en donde
# se encuentra el id 

length(titulos)
length(cuerpos)

# verificamos la longitudo de las listas. Titulos tiene una longitud de 4
# ya que cada elemento de la lista son los títulos de cada mes, y cuerpos tiene
# como elementos a los cuerpos de las noticias

titulos_todos <- unlist(titulos)
cuerpos_todos <- unlist(cuerpos)

# "derrumbamos" las listas para equiparar correctamente la cantidad de títulos
# con la cantidad de cuerpos. Deberían ser iguales, ya que se cuenta un cuerpo
# por cada título, lo mismo con el id (hay uno por noticia)

length(titulos_todos)
length(cuerpos_todos)
length(id)
tabla_final <- tibble(
  id = id,
  titulo = titulos_todos,
  cuerpo = cuerpos_todos
  
)

# creamos la tabla final con las variables id, título, cuerpo


saveRDS(
  tabla_final,
  file = file.path(data_dir, "comunicados_oea_crudo.rds")
)

# guardamos el rds de la tabla final en la carpeta data.

tabla_final

length(cuerpos)
sum(is.na(unlist(cuerpos)))

