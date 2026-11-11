#rm(list=ls())


# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}                                                                               

# Instalar - Cargar httr                                                       
if(require(httr) == FALSE){                                                
  install.packages('httr')                                                 
  library(httr)                                                            
}else{                                                                          
  library(httr)                                                            
}                                                                               

# Instalar - Cargar progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress) 
}    

if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress) 
}     

if(require(rapport) == FALSE){                                                
  install.packages('rapport')                                                 
  library(rapport)                                                            
}else{                                                                          
  library(rapport) 
} 

if(require(writexl) == FALSE){                                                
  install.packages('writexl')                                                 
  library(writexl)                                                            
}else{                                                                          
  library(writexl) 
} 

#genero las tablas para guardar los datos
tabla <- c()
errores <- c()

pb <- progress_bar$new(total = 234)


#comienza el loop de las 212 paginas de ma?aneras
for (i in 1:212) {
  url <-
    paste0("https://lopezobrador.org.mx/transcripciones/page/",
           i,
           "/")
  url_get <- GET(url)
  url_html <- read_html(url_get)
  pb$tick()
  
  
  #comienza el if loop en caso de tener buena respuesta
  if (url_get$status_code == 200){
    links <- url_html %>%
      html_elements(xpath = '//h2[@class="entry-title"]/a/@href') %>%
      html_text()
    
    #entendiendo que presenta seis ma?aneras por pagina,
    #se comienza el loop
    for (a in 1:6){
      path <- links[a]
      dom_get <- GET(path)
      dom_html <- read_html(dom_get)
      
      if (dom_get$status_code == 200){
        xpath_titulo_link <- dom_html %>%
          html_elements(xpath = '//*[@class="entry-title"]') %>%
          html_text()
        
        xpath_fecha_link <- dom_html %>%
          html_elements(xpath = '//*[@class = "entry-date"]/a') %>%
          html_text()
        
        xpath_texto_link <- dom_html %>%
          html_elements(xpath = '//*[@class = "entry-content"]') %>%
          html_text()
        
        fila =  tibble(
          url = path,
          fecha = xpath_fecha_link,
          titulo = xpath_titulo_link,
          texto_raw = xpath_texto_link
        )
        
        tabla <- bind_rows(tabla, fila)
        
      }
      
    }
    
  }
  
  
}

fix(tabla)

write.csv(tabla,file = "Mananeras.csv")
  

  
  
  
  
