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

# CORRER DESDE LA LINEA 45



# CARGAR TEXTO DE MA?ANERAS
# Run from the project root (mananeras-text-analysis/)
mananeras <- read.csv("Mananeras.csv",fileEncoding="windows-1252")

# DISCURSO SOLO DE AMLO
tablaLopez <- c()
pb <- progress_bar$new(total = 1269)

for (a in 1:1269){
  texto_limpio = stringi::stri_replace_all(mananeras$texto_raw[a], regex = '(\n)([A-Z??????]{2})','---$2')
  texto_limpio = str_split(texto_limpio,'---', simplify = TRUE)
  lopez = paste0(texto_limpio[str_detect(texto_limpio,'L?PEZ OBRADOR:')], collapse = ' ')
  fecha = mananeras$fecha[a]
  fila =  tibble(
    X = a,
    fecha = fecha,
    texto = lopez
  )
  tablaLopez <- bind_rows(tablaLopez,fila)
  
}

tibb
# TOKENIZACI?N DE ORACIONES
oraciones <- unnest_tokens(tablaLopez, oraciones,texto,"sentences")

# ELIMINAR NOMBRE DE AMLO Y ASIGNAR ID A ORACIONES
oraciones = oraciones %>% group_by(fecha) %>% 
  mutate(id = seq(1,n()), id_uni = paste(X,id,sep = "_"), 
         oraciones = str_remove_all(oraciones,'.*l?pez obrador:\\s')) %>%
  ungroup()

oraciones = read.csv("data/oraciones.csv",fileEncoding="UTF-8")

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

totales = apply(matriz_menos_rala, 1, sum)
matriz_menos_rala = matriz_menos_rala[totales>0,]

matriz_menos_rala = oraciones_matriz %>%
  removeSparseTerms(sparse = .9)

sample_size = floor(0.90 * nrow(matriz_menos_rala))
set.seed(1111)
train_ind = sample(nrow(matriz_menos_rala), size = sample_size)
train = matriz_menos_rala[train_ind, ]
test = matriz_menos_rala[-train_ind, ]


#borrar desp

i <- rep(1:5, each=3)

seedWords <- c("homicidios", "asesinatos",
               "desaparecidos","cuerpos",
               "masacres","personas",
               "narcotráfico","drogas",
               "feminicidios","mujeres")

j <- NULL

for (z in seq_along(seedWords)) { 
  j[z] <- which(oraciones_matriz$dimnames$Terms %in% seedWords[z]) 
}
SeedWeight <- 500-0.1

require(quanteda)



library(slam)
deltaS <- simple_triplet_matrix(i, j, v = rep(SeedWeight, 5),
                                nrow = 6, ncol = ncol(oraciones_matriz))




# Ajustamos nuestro modelo
lda_model = LDA(oraciones_matriz, k = 15, method = 'Gibbs',
                control = list(seed = 1111))

gammas = tidy(lda_model, matrix="gamma")

betas = tidy(lda_model,matrix="beta")


top_terms <- betas%>%
  group_by(topic) %>%
  top_n(10,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)

top_terms%>%
  mutate(term = reorder(term,beta))%>%
  ggplot(aes(term,beta,fill=factor(topic)))+
  geom_col(show.legend=F)+
  facet_wrap(~topic,scales="free")+
  coord_flip()

violencia <- betas %>%
  filter(topic==8)

docsviolencia <- gammas %>%
  filter(topic==8,gamma>0.20)%>%
  arrange(desc(gamma))

docsviolencia$document <- as.integer(docsviolencia$document)
violenciacsv <- docsviolencia %>%
  inner_join(oraciones,docsviolencia, by= "document")
violenciacsv$X <- NULL
violenciacsv$id <- NULL
violenciacsv$documento <- NULL

write.csv(violenciacsv,"violencias.csv")



#seeded para violencia

#creo el diccionario de la violencia que buscaremos
dic <- dictionary(list(hom=c("homicidios","muertos","homicidio","asesinatos"),
                       desa =c("desaparecidos","cuerpos","desapareci?","b?squeda","secuestro","secuestros","desaparici?n"),
                       mas = c("masacres","masacre","tortura"),
                       narc = c("narcotr?fico","drogas","droga","asociaci?n","guerra","organizado","contrabando","fentanilo","marihuana"),
                       fem = c("feminicidios","feminicidio","muchachas","acoso")))


violencia_tokens = violenciacsv %>%
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



#creo el LDA para hablar de violencia
oraciones_matriz2 = violencia_tokens %>%
  # Creamos la dtm
  cast_dfm(
    # Identificador de cada documento
    document = document, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n
  )

#corro el LDA, con base en el diccionario
lda2 <- textmodel_seededlda(oraciones_matriz2,dictionary=dic,valuetype="regex",verbose = T)

#creo las probabilidades de cada documento de ingresar a cada 
probtopico = as.tibble(lda2$theta)

probpalabra = as.tibble(lda2$phi)

prob <- t(probpalabra)

probtopico$documento <- seq(1:599)

violenciacsv$oraciones[222]

         