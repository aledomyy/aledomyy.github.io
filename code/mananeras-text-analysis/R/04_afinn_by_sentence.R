#rm(list=ls())
library(readr)
library(stringr)
library(tm)
library(tidyverse)
library(magrittr)
library(tidytext)
library(corpus)
library(syuzhet)
library(ggplot2)
library(beepr)
library(corpus)
#cargo los csv necesarios
afinn <- read.csv("lexico_afinn.csv",fileEncoding = "UTF-8")
oraciones <- read.csv("oraciones.csv",fileEncoding="UTF-8")
manas <- read.csv("mananeras.csv",fileEncoding = "Windows-1252")

oraciones$document <- seq(1:213051)
#guardo las stop words
custom_stop_words <- bind_rows(stop_words,
                               tibble(palabra = tm::stopwords("spanish"),
                                      lexicon = "custom"))

#hago el tidy de las mañaneras, quito stop words, hago stems
or_tidy <- oraciones %>%
  unnest_tokens(palabra,oraciones) %>%
  anti_join(custom_stop_words) %>%
  mutate(
    stems = stem_snowball(palabra, algorithm = "es")
  )
beep()

#calculo el animo de AMLO en ciertas fechas. 
animoOraciones <- or_tidy %>%
  inner_join(afinn)%>%
  group_by(document) %>%
  dplyr::summarise(animo = sum(puntuacion))
beep()
animoOraciones$animo <- scale(animoOraciones$animo)
compara<-read.csv("AnimoOraciones.csv")
write.csv(animoOraciones,"AnimoOracionesSUMA.csv")
