output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  cat(
    "'output' ya existe"
  )
}


lemat <- udpipe_download_model(language = "spanish", overwrite = FALSE)
lematizador <- udpipe_load_model(lemat$file_model)

oea_tokens <- tabla_final |>
  select(id, titulo, cuerpo) |>
  unnest_tokens(
    output = palabra,
    input = cuerpo,
    token = "words" 
  )

head(oea_tokens)

lema_oea <- udpipe_annotate(
  lematizador, 
  x = tabla_final$cuerpo, 
  doc_id = tabla_final$id) 
  
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
  

data("stopwords", package = "stopwords")

swords_1 <- stopwords::stopwords("es")
swords_2 <- stopwords::stopwords("en")
stopwords <- tibble(lemma = c(swords_1, swords_2))

head (stopwords, 10)

cant_palabras <- nrow(lema_oea_df)

lema_oea_df <- lema_oea_df |>
  anti_join(stopwords, by = "lemma") |>
  filter(
    !str_detect(lemma, "^\\d+$"), 
    nchar(lemma) > 2            
  )

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

names(lema_oea_df)

saveRDS(
  lema_oea_df,
  file = file.path(output_dir, "comunicados_oea_procesado.rds")
)

oea_procesado <- readRDS(file.path(output_dir, "comunicados_oea_procesado.rds"))

#visualizacion de los comunicados procesados