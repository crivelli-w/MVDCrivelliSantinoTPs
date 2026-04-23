oea_tfidf <- lema_oea_df |>
  count(doc_id, lemma) |>
  bind_tf_idf(doc_id, lemma, n)

# comenzamos creando una tabla de tf idf de las palabras que quedaron de la 
# iteración anterior
n_docs <- 6

oea_destacados <- oea_tfidf |>
  group_by(lemma) |>
  summarise(
    tfidf_promedio = mean(tf_idf),
    nro_documentos = n_distinct(doc_id),
    promedio_tf = mean(tf),
    promedio_idf = mean(idf),
    frecuencia = sum(n)
  ) |>
  filter(nro_documentos >= n_docs)|>
  arrange(desc(tfidf_promedio)) 

print(oea_destacados, n = 100)

# se visualiza el tfidf de aquellas palabras que aparecen en al menos 6 documentos
# para asegurar un grado de variedad (aunque se reconoce que es un umbral bajo)
# se filtran los términos para ver cuantas veces se mencionan en su total

terminos <- c("democracia", "educación", "empleo", "indígena", "elección")

n_terminos <- lema_oea_df |>
  filter(lemma %in% terminos) |>
  group_by(lemma) |>
  summarise(
    n = n(),
    nro_documentos = n_distinct(id)
  ) |>
  arrange(desc(n))

# previo al gráfico se puede hacer una visualización de la cantidad de documentos
# en la que aparece cada término (reemplazar elección por alguna de las palabras
# de interés) para comprobar que no hayan desbalances entre cantidad/variedad, aunque
# un tf idf moderado es el mejor indicador para asegurarlo

lema_oea_df |>
  filter(lemma == "elección") |>
  summarise(nro_documentos = n_distinct(doc_id))

n_terminos |>
  select(lemma, n) |>
  head()

# generamos el plot y lo guardamos como png

freq_terminos <- ggplot(
  n_terminos, 
  aes(x = reorder (lemma, n), y = n)) +
  geom_col(fill = "darkred" , alpha = 0.8) +
  labs(
    title = "Comunicados de la OEA: términos elegidos",
    subtitle = "Comparación de los términos seleccionados",
    x = "Términos seleccionados",
    y = "Frecuencia total",
    caption = "Fuente: Organización de los Estados Americanos (OEA)"
  ) +
  theme_minimal(base_size = 12) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, max(80), by = 10)) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "black", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
freq_terminos

ggsave(
   file = file.path(output_dir, "frecuencia_terminos.png"),
   plot = freq_terminos
 )
