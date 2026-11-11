library(tidyverse)
library(readxl)
library(shiny)
library(scales)
library(janitor)
library(sf)
library(leaflet)
library(fuzzyjoin)
library(colorRamps)
library(bslib)
library(bsicons)
library(thematic)
library(sass)
library(bootstrap)
#rm(list=ls())



# bs_theme_preview(
#   bs_theme(version = 4,
#            bg = "white", fg = "Black", primary = "#ffb703"
#   )
# )

 
## Para la repartición en el 2023

coeficientes <- read_excel("data/coeficientes_municipios.xlsx", sheet = "part_23") %>% 
  filter(!Municipio %in% c("total", "total_con_transitorio")) %>% 
  select(!part_22) 


part_anuales <- read_csv("data/participaciones_pmunicipio_anual.csv")

municipio <- st_read("data/municipio_corregido.shp") %>% 
  st_transform("+init=epsg:4326")






ponderacion_original <- c(0.50,0.25,0.25)



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   bg = "white", fg = "Black", primary = "#ffb703"
                   ),
  thematic::thematic_shiny(),
  
  # Application title
  titlePanel("Simulador de Ganadores y Perdedores"),
  HTML("Por Alejandro Domínguez"),
  tabsetPanel(
    tabPanel("Inicio",
             mainPanel(HTML(
               "
                 <head>
                 <title>Simulador de Proyecto de Simulación Fiscal del Estado de Nuevo León</title>
                
                 </head>
                 <body>
                 <header>
                 <h1>Bienvenido al Simulador de Proyecto de Simulación Fiscal del Estado de Nuevo León</h1>
                 </header>
                 
                 <main>
                 <section>
                 <p>Esta herramienta fue diseñada para proporcionar una aproximación visual y numérica de la repartición de participaciones basada en la Ley de Coordinación Hacendaria del Estado de Nuevo León. El simulador toma en consideración los coeficientes de pobreza, población y predial estipulados en la mencionada ley.</p> 
                 </section>
                 
                 <section>
                 <h2>Funciones principales del simulador</h2>
                 <ul>
                 <li>Estimación de la repartición de participaciones a partir de los valores de los coeficientes de pobreza, población y predial establecidos por la ley.</li>
                 <li>Simulación de la repartición de participaciones con o sin un transitorio en la ley.</li>
                 <li>Representación de la repartición de participaciones con o sin la compensación en los estatutos de la ley.</li>
                 <li>Identificación de los municipios que ganan o pierden participaciones como resultado de cualquier cambio simulado en la ley.</li>
                 <li>Cálculo de las ganancias o pérdidas de los partidos políticos como resultado de los diferentes cambios simulados en la ley.</li>
                 </ul>
                 </section>
                 
                 <section>
                 <p>Nuestro objetivo es facilitar la toma de decisiones en relación con políticas fiscales y la planificación de presupuestos. Nos esforzamos por representar distintos escenarios y mostrar cómo diferentes cambios pueden afectar a diversas partes interesadas.</p>
                 </section>
                 </main>
                 
                 <footer>
                 Hecho por Alejandro Domínguez
                 </footer>
                 </body>
                 </html>"
                 
             ))
             
             ),
    tabPanel("Explicación de la ley", 
             mainPanel(HTML(
               "
                 <head>
                 <title>Descripción de la Ley de Coordinación Hacendaria de Nuevo León</title>
                 </head>
                 <body>
                 <header>
                 <h1>Descripción de la Ley de Coordinación Hacendaria de Nuevo León</h1>
                 </header>
                 
                 <main>
                 <section>
                 <p>La Ley de Coordinación Hacendaria de Nuevo León estipula la fórmula para la repartición del 20% de las participaciones federales entre los 51 municipios del estado. La fórmula, descrita en el artículo 14, está determinada por la ponderación de los coeficientes de eficiencia recaudatoria del predial (CER), población y territorio (CEPT) y de carencias de la población (CIMP).</p>
                 <pre>
                 PART_(i,t)=(CER_(i,t)×0.50+ CEPT_(i,t)×0.25+ CIMP_(i,t)×0.25)× PARTD
               </pre>
                 </section>
                 
                 <section>
                 <h2>Coeficientes</h2>
                 <article>
                 <h3>Coeficiente de eficiencia recaudatoria (CER)</h3>
                 <p>Este coeficiente, con una ponderación de 50%, beneficia a los municipios con mejores índices de recaudación. Su cálculo involucra valores de recaudación de impuesto predial, entre otros factores, dentro de la siguiente fórmula:</p>
                 <pre>
                 CER_(i,t)=  ER_(i,t-1)/(Σ ER_(i,t-1) )
               </pre>
                 </article>
                 
                 <article>
                 <h3>Coeficiente de población y territorio (CEPT)</h3>
                 <p>Este coeficiente, con una ponderación de 25%, tiene en cuenta tanto la población como el territorio de los municipios:</p>
                 <pre>
                 CEPT_(i,t)=0.85×(PO_i/(ΣPO_i)) + 0.15×(TE_i/(ΣTE_i))
               </pre>
                 </article>
                 
                 <article>
                 <h3>Coeficiente de carencias de la población (CIMP)</h3>
                 <p>Este coeficiente, también con una ponderación de 25%, se basa en las carencias sociales del municipio y se calcula de la siguiente manera:</p>
                 <pre>
                 CIMP_(i,t) = 0.85×(CS2_i) + 0.15×(MS_i/(ΣMS_i))
               </pre>
                 </article>
                 </section>
                 
                 <section>
                 <h2>Compensación</h2>
                 <p>La compensación es una herramienta estipulada en el artículo 19 de la Ley de coordinación hacendaria de NL. Su función radica en intentar garantizar que ningún municipio reciba menos ingresos que el año anterior (tomando en cuenta a la inflación). Esto se hace siguiendo los siguientes pasos: <br> <br>
1)	Se hace un cálculo preliminar utilizando la fórmula del artículo 14, designando las cantidades a recibir por parte de cada municipio <br>
2)	Se definen los municipios ganadores (quienes recibieron más que el año pasado más la inflación) y los municipios perdedores (quienes recibieron menos que el año pasado más la inflación). <br>
3)	Se estiman las cantidades extras y faltantes en total.<br>
4)	Se obtiene la proporción del dinero faltante en total entre el dinero extra en total. <br>
5)	Esta proporción se le quita a cada municipio ganador de sus ganancias para formar una bolsa compensatoria <br>
6)	Esta bolsa compensatoria se utiliza para devolver a cada municipio perdedor sus pérdidas. <br> <br>

Utilizando este procedimiento, se garantiza que cada municipio reciba por lo menos la misma cantidad que el año pasado más la inflación. La excepción a esto se da cuando la cantidad de dinero faltante es mayor a la cantidad de dinero extra. Esto sucedió en el ejercicio fiscal del 2021, lo que se hizo fue ignorar la fórmula del artículo 14 y entregar a todos los municipios lo mismo que el año pasado menos el decrecimiento de la bolsa de participaciones. 
</p>
                 </section>
                 
                 
                 <section>
                 <h2>Transitorio</h2>
                 <p>A partir del 2022, entra en vigor un transitorio a la ley impulsado por la Tesorería del Estado. Este puede observarse en los acuerdos de repartición de las participaciones publicados en el periódico oficial del estado. Este transitorio modifica por completo la ley, cambiando la fórmula del articulo 14 por la siguiente: 

<pre>
〖PART〗_(i,t)=〖PART〗_2021   + (〖PART〗_(t )- 〖PART〗_2021 )  (〖CER〗_(i,t)  × 0.50+〖CEPT〗_(i,t)×0.25+〖CIMP〗_(i,t)× 0.25)
</pre>


Como se debe observar, la participación en el año actual depende en gran parte de la participación que se recibió en el 2021. Los coeficientes ya sólo tienen efecto en cuanto a la cantidad en la que incrementó la bolsa entre el período actual y el 2021. 
 <p>
                 </section>
                 </main>
                 
                 <footer>
                Hecho por Alejandro Domínguez<br><br><br><br><br><br><br><br><br>
                 </footer>
                 </body>
                 </html>
               "
                 
             ))
             ),
    
    tabPanel("Parámetros", 
             sidebarPanel(
                 textInput("primerc", "¿Cuál es el peso para el coeficiente de predial?",
                           value = "0.50"),
                 textInput("segundoc", "¿Cuál es el peso para el coeficiente de población y territorio?",
                           value = "0.25"),
                 textInput("tercerc", "¿Cuál es el peso para el coeficiente de pobreza?",
                           value = "0.25"),
                 selectInput("transitorio", "¿Está el transitorio en vigencia?", c("Sí" = 1,
                                                                                   "No" = 0)),
                 selectInput("compensacion", "¿Está la compensación en vigencia?", 
                             c("No",
                               "Sí")),
                 sliderInput("monto", "¿Qué porcentaje de las participaciones se reparte a los municipios?", 
                             min = 0, max = 1, step = 0.05, value = 0.2)
               ),
             mainPanel(
               HTML(
                 "
<html>

<body>
    <p>Nuestra herramienta de simulación te permite interactuar con los coeficientes que influyen en la distribución de participaciones federales a los municipios en Nuevo León. Puedes ajustar los coeficientes de pobreza, población y territorio, y eficiencia recaudatoria, y ver directamente cómo afectan estos cambios en la asignación de fondos. Descubre algunas funciones clave de nuestro simulador:</p>

    <ol>
        <li>Estima la repartición de participaciones basado en los coeficientes propuestos por la ley.</li>
        <li>Simula la repartición de participaciones con o sin un transitorio en la ley.</li>
        <li>Representa la repartición de participaciones incluyendo la compensación.</li>
        <li>Identifica los municipios que ganan o pierden participaciones como resultado de cualquier cambio simulado en la ley.</li>
        <li>Calcula las ganancias o pérdidas de los partidos políticos como resultado de los diferentes cambios simulados en la ley.</li>
    </ol>

    <p>Descubre cómo tu selección afectará a los municipios de Nuevo León y encuéntrate inmerso en la política fiscal a un nivel que nunca antes habías experimentado. Invitamos a todos a probar nuestro simulador y obtener una visión profunda de las implicaciones de la ley y cómo se pueden eseñar estrategias efectivas para la distribución de recursos. ¡Comienza hoy mismo tu viaje hacia una mejor comprensión de la política fiscal de Nuevo León!</p>
</body>
</html>"
               )
             )
  ),
  tabPanel("Situación tras los cambios",
           mainPanel(
             HTML(
               "
<head>
    <title>Graficación de Montos Municipales y Partidos Políticos</title>
</head>
<body>
    <h1>Graficación de Montos Municipales y Partidos Políticos</h1>
    <p>Para ayudarte a visualizar mejor los efectos de tus decisiones, proporcionamos una gráfica que muestra los montos que cada municipio recibe con los parámetros que has ingresado en el simulador. Esta gráfica de barras ofrece una comparativa clara de cómo la asignación de fondos se distribuye entre los municipios según tus cambios propuestos.</p>
    <p>Además, las barras en la gráfica están codificadas por colores para indicar a qué partido político pertenece cada municipio. De esta manera, no solo puedes ver cuánto financiamiento recibe cada municipio, sino también qué partido se beneficia de tu distribución propuesta.</p>
    <p>Esta combinación de información financiera y política en una sola visualización te brindará una visión más completa del panorama en Nuevo León tras los cambios que propones. Permite evaluar de manera integral los impactos socioeconómicos y políticos de tus decisiones.</p>
</body>
</html>
"
             ), 
             plotOutput("graf_municipios_inicial", height = "1000px", width = "150%"))
           ),
  tabPanel("Diferencias geográficas",
           mainPanel(
             HTML("
<head>
    <title>Observaciones del Simulador de Coordinación Hacendaria</title>
</head>
<body>
    <h1>Observaciones del Simulador de Coordinación Hacendaria</h1>
    <p>Nuestro simulador presenta un mapa detallado con los municipios de Nuevo León. Al aumentar los coeficientes de población y territorio, así como de pobreza, el simulador demuestra un hecho notable: la periferia de la ciudad se beneficia de estos ajustes. Estos cambios en la ponderación de los coeficientes desvían los recursos hacia las áreas menos densamente pobladas de la región metropolitana.</p>
    <p>En contraste, el status quo tiende a privilegiar a las zonas urbanas, dejando atrás a los municipios periféricos. Esta observación subraya la importancia de las decisiones políticas en la distribución de los recursos fiscales. A través de nuestro simulador, puedes experimentar y observar cuán significativo puede ser el impacto de cambios sutiles en la asignación de la ayuda federal.</p>
</body>
</html>
"),
             leafletOutput("mapa"),
             HTML(
               "
<head>
    <title>Análisis de los Montos Per Cápita de Participaciones Federales</title>
</head>
<body>
    <h1>Análisis de los Montos Per Cápita de Participaciones Federales</h1>
    <p>Las siguientes gráficas muestran los montos per cápita que los diferentes municipios, sean urbanos, periféricos o rurales, reciben de las participaciones federales. El monto per cápita implica la cantidad de fondos que cada municipio recibe por cada uno de sus habitantes.</p>
    <p>En el status quo, es evidente que hay una distribución muy desigual de los recursos: por ejemplo, mientras municipios como San Pedro reciben más de 11,000 pesos por persona, municipios como Juárez obtienen menos de 1,000 pesos por persona. Esta disparidad resalta la importancia de considerar las necesidades individuales de cada municipio al hacer las asignaciones de fondos.</p>
    <p>Te invitamos a experimentar con los coeficientes en nuestro simulador y observar cómo estos cambios pueden lograr una distribución de la ayuda federal más equitativa entre los municipios. A través de tus decisiones, puedes explorar formas de reducir las brechas existentes y crear una distribución de recursos que considera de mejor manera las particularidades de cada municipio.</p>
</body>
</html>
"
             ),
             plotOutput("graf_municipios_inicial_poblacion_urbano", height = "1000px", width = "150%"),
             plotOutput("graf_municipios_inicial_poblacion_periferico", height = "1000px", width = "150%"),
             plotOutput("graf_municipios_inicial_poblacion_rural", height = "1000px", width = "150%")
           )
           ),
  tabPanel("Diferencias con los parámetros",
           mainPanel(
             HTML(
               "
    <title>Comparación de Asignaciones de Participaciones Federales</title>
</head>
<body>
    <h1>Comparación de Asignaciones de Participaciones Federales</h1>
    <p>A continuación, exploraremos la diferencia entre el status quo actual y una hipotética nueva situación basada en los cambios de coeficientes que tú, como usuario de la aplicación, hayas decidido implementar. Esta comparativa te permitirá visualizar directamente, municipio por municipio, el impacto de tus decisiones.</p>
    <p>Los municipios con una diferencia positiva están ganando dinero tras los cambios imputados por el usuario. En otras palabras, estos municipios recibirían más fondos con tu nueva configuración de coeficientes en comparación con la asignación actual.</p>
    <p>Por otro lado, los municipios con una diferencia negativa están perdiendo dinero tras los cambios imputados por el usuario. Esto indica que la cantidad de participaciones federales que recibirían con tu nueva configuración de coeficientes sería menor que la que actualmente reciben.</p>
    <p>La evaluación de estas diferencias es clave para comprender el efecto real que tendrían tus decisiones sobre las asignaciones de recursos entre los municipios y puede ayudarte a tomar decisiones más equitativas e informadas.</p>
</body>
</html>
"
             ),
             plotOutput("graf_municipios", height = "1000px", width = "150%")
  )),
  tabPanel("Tablas informativas",
           mainPanel(
             
             HTML("
<head>
    <title>Tablas Informativas de Cambios en Participaciones Federales</title>
</head>
<body>
    <h1>Tablas Informativas de Cambios en Participaciones Federales</h1>
    <p>Finalizamos nuestra exploración con un par de tablas informativas que resumen los efectos de los cambios imputados por el usuario en el simulador. Estas tablas proporcionan una representación detallada de cómo se redistribuyen las participaciones federales bajo tu nueva configuración de coeficientes.</p>
    <p>La primera columna incluye el nombre del municipio afectado. La segunda columna muestra la diferencia entre el status quo y lo que el municipio recibiría con los cambios que el usuario ha implementado en el simulador. Es decir, esta cantidad representa el aumento o la disminución en los fondos de participación federal que resultarían de tus cambios propuestos.</p>
    <p>Continuando, la tercera columna presenta el nuevo monto total que recibiría el municipio bajo tu configuración de coeficientes. Y finalmente, la última columna muestra el nuevo monto per cápita que se derivaría tras los cambios realizados por el usuario. Este valor representa la cantidad de fondos que cada municipio recibiría por cada uno de sus habitantes bajo tu propuesta de distribución.</p>
    <p>Estas tablas son una valiosa herramienta para entender con detalle el alcance de los posibles cambios en la distribución de las participaciones federales y te ayudarán a tomar decisiones más informadas y equitativas.</p>
</body>
</html>
"),tableOutput("tabla"),downloadButton("downloadData", "Descargar esta tabla"), 
             HTML("

<body>
    <h1>Análisis Político de los Cambios en Participaciones Federales</h1>
    <p>Además de las tablas que describen los cambios financieros en cada municipio, también se agrega una tabla que examina la situación política tras los cambios imputados por el usuario. La política fiscal no sólo tiene implicaciones económicas, sino que también cambia el panorama político de una región dada, y es importante entender estos efectos interrelacionados.</p>
    <p>En esta nueva tabla puedes observar cuánto dinero ganan o pierden los partidos políticos tras las modificaciones que has realizado. Cada partido político tiene diferentes municipios bajo su administración, y los cambios que propones en la distribución de las participaciones federales pueden beneficiar a algunos partidos mientras perjudican a otros.</p>
    <p>Esta visión politizada de las implicaciones fiscales te proporciona una comprensión más completa de las ramificaciones de tus decisiones. Así, puedes tomar decisiones no solo basadas en criterios económicos, sino también considerando los cambios en el equilibrio del poder político entre los diferentes partidos.</p>
</body>
</html>
"), 
             tableOutput("tabla_partidos"),
             tableOutput("tabla_zonas"),
             textOutput("transit"))
           )
           )
    
    )
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  cant_distribuir <- reactive({
    part_total <- 54992150000
    dist <- input$monto * part_total
    
    cant_distribuir <- dist
    cant_distribuir
    
    
  })
  
  nueva_cant_distribuir <- reactive({
    nueva_cant_distribuir <- cant_distribuir() - sum(coeficientes$part_21)
    nueva_cant_distribuir
  })
    
  
  
  
  
  
  escenario_actual <- reactive({
    escenario_actual <- coeficientes %>% 
      transmute(Municipio = Municipio, monto_original =  part_23,
                monto_per_capita_original = monto_original/Poblacion)
    
    escenario_actual
    
  })
  
  base_sin_garantia <- reactive({
    
    ## Esto cambia la cantidad de dinero que depende de los coeficientes
    
    valor <- if_else(input$transitorio == 0, cant_distribuir() ,nueva_cant_distribuir())
    
    transitorio <- if_else(as.numeric(input$transitorio) == 1, "part_21", "cero")
    
    predial_original <- ponderacion_original[1] * valor
    poblacion_original<- ponderacion_original[2] * valor
    pobreza_original<- ponderacion_original[3] * valor
    
    predial_nuevo <- as.numeric(input$primerc) * valor
    poblacion_nuevo <- as.numeric(input$segundoc) * valor
    pobreza_nuevo <- as.numeric(input$tercerc) * valor
    
    
    base_sin_garantia <- coeficientes %>% 
      left_join(escenario_actual(), by = "Municipio") %>% 
      mutate(cero = 0,
             monto_nuevo = (predial_nuevo*Coef1) + (poblacion_nuevo*Coef2) + (pobreza_nuevo * Coef3) + eval(as.symbol(transitorio)),
             diferencia = monto_nuevo-monto_original,
             monto_per_capita = monto_nuevo/Poblacion)
    
    base_sin_garantia
  })
  
  
  base_con_garantia <- reactive({
    
    ## Al existir garantía, la formula se aplica sobre el valor total del fondo
    valor  <-  if_else(input$transitorio == 0, cant_distribuir() ,nueva_cant_distribuir())
    
    predial_nuevo <- as.numeric(input$primerc) * valor
    poblacion_nuevo <- as.numeric(input$segundoc) * valor
    pobreza_nuevo <- as.numeric(input$tercerc) * valor
    
    inflacion = 0.0782
    
    montos_pregarantia = coeficientes %>% 
      mutate(
        parte_pre_19_nuevo = (predial_nuevo * Coef1) + (poblacion_nuevo * Coef2) + (pobreza_nuevo * Coef3) 
      ) %>% 
      left_join(part_anuales, by = "Municipio") %>% 
      left_join(escenario_actual(), by = "Municipio") %>% 
      mutate(part_m_crecimiento = part_22 * (1 + inflacion), ## Participaciones año anterior + crecimiento fondo
             cuanto_falta_garantia_nuevo = if_else((part_m_crecimiento - parte_pre_19_nuevo) > 0,(part_m_crecimiento - parte_pre_19_nuevo), 0),
             monto_extra_nuevo = if_else((part_m_crecimiento - parte_pre_19_nuevo) > 0,0, abs((part_m_crecimiento - parte_pre_19_nuevo)))## Cuanto extra Art.VI
      ) 
    
    
    ## El monto que representa lo que falta de lo extra
    cuanto_representa_nuevo <- sum(na.omit(montos_pregarantia$cuanto_falta_garantia_nuevo))/sum(na.omit(montos_pregarantia$monto_extra_nuevo))
    
    monto_garantia <- montos_pregarantia %>% 
      filter(!Municipio %in% "Total") %>% 
      transmute(Municipio = Municipio,
                cuanto_falta_garantia_nuevo = cuanto_falta_garantia_nuevo,
                monto_disminucion_pm_nuevo = cuanto_representa_nuevo * monto_extra_nuevo,
                monto_final_nuevo = parte_pre_19_nuevo - monto_disminucion_pm_nuevo + cuanto_falta_garantia_nuevo,
                monto_per_capita = monto_final_nuevo/Poblacion,
                monto_original = monto_original,
                Zona = Zona
      )  
    
    
    base_con_garantia <- monto_garantia %>% 
      transmute(
        monto_nuevo = monto_final_nuevo,
        monto_per_capita = monto_per_capita,
        diferencia = monto_final_nuevo - monto_original,
        Municipio = Municipio
      ) %>% 
      left_join(select(coeficientes,Municipio, Partido, Zona), by = "Municipio")
    
    base_con_garantia
  })
  
  base_por_partido <- reactive({
    base_por_partido <- coeficientes %>% 
      left_join(escenario_actual(), by = "Municipio") %>% 
      mutate(monto_original = monto_original,
             monto_nuevo = (predial_nuevo*Coef1) + (poblacion_nuevo*Coef2) + (pobreza_nuevo*Coef3),
             diferencia = monto_nuevo-monto_original) %>% 
      group_by(Partido) %>% 
      summarise(diferencia = sum(diferencia))
    
    base_por_partido
    
  })
  
  
  
  output$graf_municipios_inicial <- renderPlot({
    
    
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    ggplot(eval(as.symbol(base))()) +
      geom_col(aes(x = reorder(Municipio, -monto_nuevo), y = monto_nuevo, fill = Partido)) +
      scale_fill_manual(values = c(
        "ind" = "gray", 
        "mc" = "orange",
        "morena" = "#6f1d1b",
        "pan" = "blue",
        "pri" = "red",
        "verde" = "green"
      ),
      labels = c(
        "ind" = "Independiente", 
        "mc" = "MC",
        "morena" = "Morena",
        "pan" = "PAN",
        "pri" = "PRI",
        "verde" = "Verde"
      )) +
      scale_y_continuous(labels = comma)+
      theme_classic() + 
      theme(axis.text.x  =  element_text(angle = 90, size = 15),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15)
            ) +
      labs(x = "Municipio", y = "Monto a recibir", title = "Monto total que recibe cada municipio en 2023")
    
  })
  
  
  output$graf_municipios_inicial_poblacion_rural <- renderPlot({
    
    
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    grafico <- eval(as.symbol(base))() %>% 
      filter(Zona %in% "rural")
    
    ggplot(grafico) +
      geom_col(aes(x = reorder(Municipio, -monto_per_capita), y = monto_per_capita, fill = Partido)) +
      scale_fill_manual(values = c(
        "ind" = "gray", 
        "mc" = "orange",
        "morena" = "#6f1d1b",
        "pan" = "blue",
        "pri" = "red",
        "verde" = "green"
      ),
      labels = c(
        "ind" = "Independiente", 
        "mc" = "MC",
        "morena" = "Morena",
        "pan" = "PAN",
        "pri" = "PRI",
        "verde" = "Verde"
      )) +
      scale_y_continuous(labels = comma)+
      theme_classic() + 
      theme(axis.text.x  =  element_text(angle = 90, size = 15),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15)
            ) + 
      labs(x = "Municipio", y = "Monto a recibir", title = "Monto Per Cápita que recibe cada municipio rural en 2023")
    
  })
  
  output$graf_municipios_inicial_poblacion_urbano <- renderPlot({
    
    
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    grafico <- eval(as.symbol(base))() %>% 
      filter(Zona %in% "urbano")
    
    ggplot(grafico) +
      geom_col(aes(x = reorder(Municipio, -monto_per_capita), y = monto_per_capita, fill = Partido)) +
      scale_fill_manual(values = c(
        "ind" = "gray", 
        "mc" = "orange",
        "morena" = "#6f1d1b",
        "pan" = "blue",
        "pri" = "red",
        "verde" = "green"
      ),
      labels = c(
        "ind" = "Independiente", 
        "mc" = "MC",
        "morena" = "Morena",
        "pan" = "PAN",
        "pri" = "PRI",
        "verde" = "Verde"
      )) +
      scale_y_continuous(labels = comma)+
      theme_classic() + 
      theme(axis.text.x  =  element_text(angle = 90, size = 15),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15)) + 
      labs(x = "Municipio", y = "Monto a recibir", title = "Monto Per Cápita que recibe cada municipio urbano en 2023")
    
  })
  
  
  output$graf_municipios_inicial_poblacion_periferico <- renderPlot({
    
    
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    grafico <- eval(as.symbol(base))() %>% 
      filter(Zona %in% "periferico")
    
    ggplot(grafico) +
      geom_col(aes(x = reorder(Municipio, -monto_per_capita), y = monto_per_capita, fill = Partido)) +
      scale_fill_manual(values = c(
        "ind" = "gray", 
        "mc" = "orange",
        "morena" = "#6f1d1b",
        "pan" = "blue",
        "pri" = "red",
        "verde" = "green"
      ),
      labels = c(
        "ind" = "Independiente", 
        "mc" = "MC",
        "morena" = "Morena",
        "pan" = "PAN",
        "pri" = "PRI",
        "verde" = "Verde"
      )) +
      scale_y_continuous(labels = comma)+
      theme_classic() + 
      theme(axis.text.x  =  element_text(angle = 90, size = 15),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15)) + 
      labs(x = "Municipio", y = "Monto a recibir", title = "Monto Per Cápita que recibe cada municipio periferico en 2023")
    
  })
  
  output$graf_municipios <- renderPlot({
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    
    ggplot(eval(as.symbol(base))()) +
      geom_col(aes(x = reorder(Municipio, -diferencia), y = diferencia, fill = Partido)) +
      scale_fill_manual(values = c(
        "ind" = "gray", 
        "mc" = "orange",
        "morena" = "#6f1d1b",
        "pan" = "blue",
        "pri" = "red",
        "verde" = "green"
      ),
      labels = c(
        "ind" = "Independiente", 
        "mc" = "MC",
        "morena" = "Morena",
        "pan" = "PAN",
        "pri" = "PRI",
        "verde" = "Verde"
      )) +
      scale_y_continuous(labels = comma)+
      theme_classic() + 
      theme(axis.text.x  =  element_text(angle = 90, size = 15),
            axis.text.y = element_text(size = 15),
            legend.text = element_text(size = 15)) +
      labs(x = "Municipio", y = "Diferencia", title = "Diferencia contra el escenario real, \n dados los cambios")
    
  })
  
  
  
  tabla_data <- reactive({
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    eval(as.symbol(base))() %>% 
      select(Municipio, diferencia, monto_nuevo, monto_per_capita, Zona, Partido) %>% 
      arrange(desc(diferencia)) %>% 
      adorn_totals() %>% 
      mutate(diferencia = comma(diferencia),
             monto_nuevo = comma(monto_nuevo),
             monto_per_capita = comma(monto_per_capita), 
             Zona, 
             Partido)
  })
  output$tabla <- renderTable({
    tabla_data()
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(tabla_data(), con)
    }
  )
  
  output$tabla_partidos <- renderTable({
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    eval(as.symbol(base))() %>% 
      select(Municipio, Partido, diferencia, monto_nuevo) %>% 
      group_by(Partido) %>% 
      summarise(monto_gana_p = sum(diferencia),
                n = n()) %>% 
      mutate(monto_gana_p = comma(monto_gana_p/n))
  })
  
  output$tabla_zonas <- renderTable({
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    eval(as.symbol(base))() %>% 
      select(Municipio, Zona, diferencia, monto_nuevo, monto_per_capita) %>% 
      group_by(Zona) %>% 
      summarise(monto_gana_p = sum(diferencia),
                promedio_zona = mean(monto_nuevo),
                promedio_z_pc = mean(monto_per_capita),
                n = n()) %>% 
      mutate(monto_gana_p = comma(monto_gana_p/n),
             promedio_zona = dollar(promedio_zona),
             promedio_z_pc = dollar(promedio_z_pc)
             )
  })
  
  
  
  
  ## Aqui trabajo en el mapa
  output$mapa <- renderLeaflet({
    
    base <- if_else(input$compensacion %in% "Sí",
                    "base_con_garantia",
                    "base_sin_garantia"
    )
    
    base_mapa <- eval(as.symbol(base))() %>% 
      left_join(municipio, by = c("Municipio" = "NOMBRE")) %>% 
      st_as_sf()
    
    
    palette <- colorRampPalette(c("red", "blue"))(2)
    breakp <- c(-1000000000000, 0, 100000000000000)
    
    
    ## Estaría bien hacer que la paleta fuera una gradiente
    paleta <- colorBin(palette,domain = base_mapa$diferencia ,bins = breakp)
    
    popup = paste0("Municipio: ",base_mapa$Municipio,"<br/> Diferencia recibida: ", dollar(base_mapa$diferencia ))
    
    
    leaflet(base_mapa) %>% 
      addProviderTiles(provider = providers$OpenStreetMap) %>% 
      addPolygons(fillOpacity = 0.8, 
                  fillColor = ~paleta(diferencia),
                  popup = popup,
                  opacity =1, 
                  weight = 1,
                  color = "black"
                  
      )
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
