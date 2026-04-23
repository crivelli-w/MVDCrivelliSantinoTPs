output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  cat(
    "'output' ya existe"
  )
}

# creamos output si no existe

lemat <- udpipe_download_model(language = "spanish", overwrite = FALSE)
lematizador <- udpipe_load_model(lemat$file_model)

# modelos de lematizador

oea_tokens <- tabla_final |>
  select(id, titulo, cuerpo) |>
  unnest_tokens(
    output = palabra,
    input = cuerpo,
    token = "words" 
  )

head(oea_tokens)

# tokenizamos con unnest tokens que se encarga de la limpieza básica y normalizacion
# del cuerpo

lema_oea <- udpipe_annotate(
  lematizador, 
  x = tabla_final$cuerpo, 
  doc_id = tabla_final$id) 
# lematización del texto ( x el cuerpo, doc_id el id) que pasa
# a llamarse lema oea
  
lema_oea_df <- as.data.frame(lema_oea) |> 
  select(doc_id, lemma, upos) 

lema_oea_df$upos |> unique() 
lema_oea_df <- lema_oea_df |> 
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(
    lemma = tolower(lemma),
    lemma = str_replace_all(lemma, "[^[:alpha:]áéíóúüñ]", "")
  ) |>
  filter(lemma != "")

# creamos el dframe lema_oea_df en el que cada observación consiste en una palabra
# de todas las noticias analizadas. Cada palabra está identificada a la noticia
# a la que pertenece (id) y el tipo de palabra que es (upos). Nos quedamos con 
# aquellas que no sean articulos, las pasamos a minúscula y sacamos caractéres 
# especiales y espacios
  

data("stopwords", package = "stopwords")

swords_1 <- stopwords::stopwords("es")
swords_2 <- stopwords::stopwords("en")
stopwords <- tibble(lemma = c(swords_1, swords_2))

head (stopwords, 10)

#cargamos las stopwords en español y en inglés (por si las hay)

cant_palabras <- nrow(lema_oea_df)

lema_oea_df <- lema_oea_df |>
  anti_join(stopwords, by = "lemma") |>
  filter(
    !str_detect(lemma, "^\\d+$"), 
    nchar(lemma) > 2            
  )

# quitamos cadenas de números y palabras menores a 2 caractéres. Vemos cuanto varía 
# la eliminación de stopwords con este filtro "extra"

cat("Nro de tokens previo a la eliminación de stop words:", cant_palabras, "\n")
cat("Nro de tokens luego de eliminar stop words:", nrow(lema_oea_df), "\n")
cat(
  "Reducción relativa porcentual:", 
  round(
    100 * ((cant_palabras - nrow(lema_oea_df)) / cant_palabras),
    1
  ), 
  "%\n"
)

# guardamos la tabla de cada palabra, con su tipo y a la noticia que 
# pertenece, como rds

saveRDS(
  lema_oea_df,
  file = file.path(output_dir, "comunicados_oea_procesado.rds")
)

oea_procesado <- readRDS(file.path(output_dir, "comunicados_oea_procesado.rds"))

#visualizacion de los comunicados procesados