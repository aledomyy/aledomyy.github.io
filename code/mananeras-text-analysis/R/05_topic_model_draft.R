#rm(list=ls())
library(readr)
library(stringr)
library(tm)
library(tidyverse)
library(magrittr)
library(tidytext)
library(progress)
library(stopwords)
library(topicmodels)
library(seededlda)

#descargar csv
oraciones = read.csv("oraciones.csv",fileEncoding="UTF-8")

# ASIGNAR STOPWORDS
stop_words = tibble(stop_words = stopwords("es"))
#palabras <- unnest_tokens(tablaLopez, palabras,texto,"words")

oraciones$document <- seq(1:213051)
# BAG OF WORDS
oraciones_tokens = oraciones %>%
  # Tokenizamos
  unnest_tokens(
    output = "palabra", 
    token = "words", 
    input = oraciones
  ) %>%
  count(palabra,document) %>%
  anti_join(stop_words,by=c("palabra"="stop_words")) %>%
  filter(
    !str_detect(palabra,'[^\\w_]|\\d'),
  )

library(beepr)
beep()

oraciones_matriz = oraciones_tokens %>%
  # Creamos la dtm
  cast_dtm(
    # Identificador de cada documento
    document = document, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n, 
    # Ponderadores
    weighting = tm::weightTf
  )
beep()

# Ajustamos nuestro modelo
lda_model = LDA(oraciones_matriz, k = 15, method = 'Gibbs',
                control = list(seed = 1111))
beep()

gammas = tidy(lda_model, matrix="gamma")

betas = tidy(lda_model,matrix="beta")

#se bbuscan los terminos principales de cada topico
top_terms <- betas%>%
  group_by(topic) %>%
  top_n(20,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)


#se organizan en un gráfico
topicos <- top_terms%>%
  mutate(term = reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend=F)+
  facet_wrap(~topic,scales="free")+
  coord_flip()


