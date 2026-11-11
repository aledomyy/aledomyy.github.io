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
library(RColorBrewer)
#cargo los csv necesarios
afinn <- read.csv("lexico_afinn.csv",fileEncoding = "UTF-8")
mananeras <- read.csv("oraciones.csv",fileEncoding="UTF-8")
manas <- read.csv("mananeras.csv",fileEncoding = "Windows-1252")

aa<-read.csv("oraciones.csv")
#guardo las stop words
custom_stop_words <- bind_rows(stop_words,
                               tibble(palabra = tm::stopwords("spanish"),
                                      lexicon = "custom"))

#hago el tidy de las mañaneras, quito stop words, hago stems
man_tidy <- mananeras %>%
  unnest_tokens(palabra,oraciones) %>%
  anti_join(custom_stop_words) %>%
  mutate(
    stems = stem_snowball(palabra, algorithm = "es")
  )
beep()
#calculo el animo de AMLO en ciertas fechas. 
animofecha <- man_tidy %>%
  inner_join(afinn)%>%
  group_by(X) %>%
  dplyr::summarise(animo = sum(puntuacion))

animofecha$animo <- scale(animofecha$animo)
fechas <- cbind(manas$X,manas$fecha)

colnames(fechas) <- c("X","fecha")

fechas <- as_tibble(fechas)
fechas$X <- as.integer(fechas$X)
#creo el tidy alrededor de las fechas y el animo
animo <- inner_join(animofecha,fechas,by="X")

write.csv(animo,"animoSUMA.csv")

animo <- read.csv("animoSUMA.csv")
#agrego el a?o al tibble
animo$año <- str_extract(animo$fecha, "[0-9]{4}")

#checo animo promedio por año
anaño <- animo %>%
  group_by(año)%>%
  summarise(prom=mean(animo))
ggplot(aes(año,prom,group=1),data=anaño)+
  geom_point(size=2)+
  geom_line(size=2,aes(color="red"))+
  coord_cartesian(ylim=c(-1,1))+
  scale_color_brewer(palette="Paired")+
  theme(legend.position = "none")+
  labs(x="Año",y="Promedio de Ánimo",title = "Ánimo de AMLO a \n través de los años")

#junto violencia a animo
colnames(cant_violencia_x) <- c("X","Cant_Violencia")
animo <- left_join(animo,cant_violencia_x,by="X")
animo$Cant_Violencia <- scale(animo$Cant_Violencia)

#creo el gráfico del animo de AMLO
  ggplot(animo)+
    geom_col(aes(x=desc(X),y=animo,fill=año),show.legend = T)+
    labs(title = "Estado de Ánimo de AMLO", x= "Mañanera", y = "Ánimo promedio en escala AFINN")+
    facet_wrap(~año,scales="free_x")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x = element_blank())
    

#sacar textos, remplazar agosto-08
# %d%m%a clase dates

ggplot(animo,aes(x=desc(X),y=animo, fill = año))+
  geom_col(show.legend = T)+
  labs(title = "Estado de ánimo de AMLO", x= "Mañanera", y = "Ánimo promedio en escala AFINN")+
  facet_wrap(~año,scales="free_x")+
  theme(axis.text.x = element_blank(),legend.position = "none")

#bajo datos violencia
cant_violencia_x <- read.csv("x_violencia.csv")
colnames(cant_violencia_x) <- c("A","X","cant_violencia")
animo <- left_join(animo,cant_violencia_x,by="X")
animo$cant_violencia<- scale(animo$cant_violencia)

#grafico violencia vs animo
violAnimo <- ggplot(animo)+
  geom_point(aes(x=cant_violencia,y=animo,color=animo,alpha=.2))+
  geom_smooth(aes(x=cant_violencia,y=animo),method="lm",se=F,formula=y~poly(x,2))+
  labs(x="Cantidad de oraciones relacionadas a violencia (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a la violencia")+
  theme(axis.text.x = element_blank())+
  scale_alpha(guide=F)
violAnimo + scale_color_brewer(palette = "YlGnBu")

#grafico neoliberal vs animo
cant_neoliberal_x <- read.csv("x_neoliberal.csv")
colnames(cant_neoliberal_x) <- c("Y","X","cant_neoliberal")
animo <- left_join(animo,cant_neoliberal_x,by="X")
animo$cant_neoliberal<- scale(animo$cant_neoliberal)

neolibAnimo <- ggplot(animo)+
  geom_point(aes(x=cant_neoliberal,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_neoliberal,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas al Neoliberalismo (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación al neoliberalismo")+
  scale_alpha(guide=F)
neolibAnimo + scale_color_brewer(palette = "Paired")


#grafico campaña vs animo
cant_campaña_x <- read.csv("x_campaña.csv")
colnames(cant_campaña_x) <- c("Y","X","cant_campaña")
animo <- left_join(animo,cant_campaña_x,by="X")
animo$cant_campaña<- scale(animo$cant_campaña)

camp_animo <- ggplot(animo)+
  geom_point(aes(x=cant_campaña,y=animo,color=año,alpha=.2))+
  geom_smooth(aes(x=cant_campaña,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a la campaña (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a la campaña")+
  scale_alpha(guide=F)
camp_animo + scale_color_brewer(palette = "Paired")


#grafico 4t vs animo
cant_4t_x <- read.csv("x_4t.csv")
colnames(cant_4t_x) <- c("Y","X","cant_4t")
animo <- left_join(animo,cant_4t_x,by="X")
animo$cant_4t<- scale(animo$cant_4t)

t4_animo <-ggplot(animo)+
  geom_point(aes(x=cant_4t,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_4t,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a la 4T (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a la 4T")
t4_animo + scale_color_brewer(palette = "Paired")


#grafico apoyos vs animo
cant_apoyos_x <- read.csv("x_apoyos.csv")
colnames(cant_apoyos_x) <- c("Y","X","cant_apoyos")
animo <- left_join(animo,cant_apoyos_x,by="X")
animo$cant_apoyos<- scale(animo$cant_apoyos)

apoyo_animo <- ggplot(animo)+
  geom_point(aes(x=cant_apoyos,y=animo,color=año, alpha=0.2))+
  geom_smooth(aes(x=cant_apoyos,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a los apoyos (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a los apoyos")
apoyo_animo + scale_color_brewer(palette = "YlGnBu")

#grafico apoyos vs historia
cant_histo_x <- read.csv("x_histo.csv")
colnames(cant_histo_x) <- c("Y","X","cant_histo")
animo <- left_join(animo,cant_histo_x,by="X")
animo$cant_histo<- scale(animo$cant_histo)

histo_animo <- ggplot(animo)+
  geom_point(aes(x=cant_histo,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_histo,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Historia (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Historia")
histo_animo + scale_color_brewer(palette = "YlGnBu")



#grafico Legalidad vs animo
cant_leg_x <- read.csv("x_leg.csv")
colnames(cant_leg_x) <- c("Y","X","cant_leg")
animo <- left_join(animo,cant_leg_x,by="X")
animo$cant_leg<- scale(animo$cant_leg)

leg_animo <- ggplot(animo)+
  geom_point(aes(x=cant_leg,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_leg,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Legalidad (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Legalidad")

leg_animo + scale_color_brewer(palette = "YlGnBu")


#grafico apoyos vs Energia
cant_en_x <- read.csv("x_en.csv")
colnames(cant_en_x) <- c("Y","X","cant_en")
animo <- left_join(animo,cant_en_x,by="X")
animo$cant_en<- scale(animo$cant_en)

en_animo <- ggplot(animo)+
  geom_point(aes(x=cant_en,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_en,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Energía (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Energía")
en_animo + scale_color_brewer(palette = "YlGnBu")

#grafico mañananeras vs mañaneras
cant_man_x <- read.csv("x_man.csv")
colnames(cant_man_x) <- c("Y","X","cant_man")
animo <- left_join(animo,cant_man_x,by="X")
animo$cant_man<- scale(animo$cant_man)

man_animo <- ggplot(animo)+
  geom_point(aes(x=cant_man,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_man,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Mañaneras (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Mañaneras")
man_animo + scale_color_brewer(palette = "YlGnBu")


#grafico mañananeras vs Presupuesto
cant_pres_x <- read.csv("x_pres.csv")
colnames(cant_pres_x) <- c("Y","X","cant_pres")
animo <- left_join(animo,cant_pres_x,by="X")
animo$cant_pres<- scale(animo$cant_pres)

pres_animo <- ggplot(animo)+
  geom_point(aes(x=cant_pres,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_pres,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Presupuesto (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Presupuesto")
pres_animo + scale_color_brewer(palette = "YlGnBu")


#grafico mañananeras vs Desarrollo
cant_des_x <- read.csv("x_des.csv")
colnames(cant_des_x) <- c("Y","X","cant_des")
animo <- left_join(animo,cant_des_x,by="X")
animo$cant_des<- scale(animo$cant_des)

des_animo <- ggplot(animo)+
  geom_point(aes(x=cant_des,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_des,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Desarrollo (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Desarrollo")
des_animo + scale_color_brewer(palette = "YlGnBu")



#grafico mañananeras vs Vulnerable
cant_vul_x <- read.csv("x_vul.csv")
colnames(cant_vul_x) <- c("Y","X","cant_vul")
animo <- left_join(animo,cant_vul_x,by="X")
animo$cant_vul<- scale(animo$cant_vul)

vul_animo <- ggplot(animo)+
  geom_point(aes(x=cant_vul,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_vul,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Personas Vulnerables (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Personas Vulnerables")
vul_animo + scale_color_brewer(palette = "YlGnBu")


#grafico mañananeras vs Infraestructura
cant_inf_x <- read.csv("x_inf.csv")
colnames(cant_inf_x) <- c("Y","X","cant_inf")
animo <- left_join(animo,cant_inf_x,by="X")
animo$cant_inf<- scale(animo$cant_inf)

inf_animo <- ggplot(animo)+
  geom_point(aes(x=cant_inf,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_inf,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Infraestructura (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Infraestructura")
inf_animo + scale_color_brewer(palette = "YlGnBu")



#grafico mañananeras vs Infraestructura
cant_fin_x <- read.csv("x_fin.csv")
colnames(cant_fin_x) <- c("Y","X","cant_fin")
animo <- left_join(animo,cant_fin_x,by="X")
animo$cant_fin<- scale(animo$cant_fin)

fin_animo <- ggplot(animo)+
  geom_point(aes(x=cant_fin,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_fin,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Finanzas (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Finanzas")
fin_animo + scale_color_brewer(palette = "YlGnBu")


#grafico mañananeras vs Salud
cant_ss_x <- read.csv("x_ss.csv")
colnames(cant_ss_x) <- c("Y","X","cant_ss")
animo <- left_join(animo,cant_ss_x,by="X")
animo$cant_ss<- scale(animo$cant_ss)

ss_animo <- ggplot(animo)+
  geom_point(aes(x=cant_ss,y=animo,color=año,alpha=0.2))+
  geom_smooth(aes(x=cant_ss,y=animo),method="lm",se=F)+
  labs(x="Cantidad de oraciones relacionadas a Salud (estandarizadas)",
       y="Ánimo de acuerdo a análisis AFINN",
       title = "Ánimo del presidente en sus conferencias con relación a Salud")
ss_animo + scale_color_brewer(palette = "YlGnBu")



#Corro una regresión para predecir animo
write.csv(animo,"AnimoCant.csv")
install.packages("broom")
library(broom)
library(dplyr)
library(RColorBrewer)
regan <- lm(animo ~ cant_ss+cant_fin+cant_inf+cant_vul+cant_des+cant_pres+
              cant_man+cant_en+cant_4t+cant_leg+cant_histo+cant_apoyos+
              cant_neoliberal+cant_violencia+cant_campaña,data=animo)
summary(regan)
library(regclass)
VIF(regan)
cov(regan)
r <- tidy(regan)
r<- r[-1,]
r <- arrange(r, estimate)
r$term
r$terminos <- c("Campaña", "Violencia", "Neoliberal","Historia", "Energía",
                "Personas Vulnerables", "Legalidad", "4T","Salud","Finanzas",
                "Mañaneras", "Desarrollo", "Infraestructura", "Apoyos", 
                "Presupuesto")
VID()
ggplot(r)+
  geom_col(aes(x=reorder(terminos,estimate),y=estimate,fill=terminos))+
  labs(x="Término",y="Coeficiente en la regresión",
       title = "Efecto de la cantidad hablada sobre cada tópico para predecir
       el ánimo de AMLO")+
  scale_fill_manual(values=rep(brewer.pal(n=12,"YlGnBu"),2))+
  theme(legend.position = "none")
  


r$estimate <- sort(r$estimate)
library(GGally)
estimados <- ggcoef(regan,color="blue",size=5,shape=18)
estimados + scale_color_brewer(palette = "YlGnBu")


#ver las peores mañaneras
negativas <- animo %>%
  filter(animo<0)

peores <- negativas %>%
  slice_min(order_by = animo, n=5)


#ver las mejores
positivas <- animo %>%
  filter(animo > 0)

mejores <- positivas %>%
  slice_max(order_by = animo,n=5)

oraciones <- read.csv("oraciones.csv")
#cuanto habla de cada tema
cant_total_oraciones <- oraciones %>%
  inner_join(promgam, by="document")%>%
  group_by(topic)%>%
  summarise(n())



#comienza búsqueda de palabras negativas más comunes
palanimo <- man_tidy %>%
  inner_join(afinn)

palneg <- palanimo %>%
  filter(puntuacion < 0) %>%
  dplyr::count(palabra, sort = T)


cuentas <- man_tidy %>%
  group_by(palabra) %>%
  summarise(n()) 
