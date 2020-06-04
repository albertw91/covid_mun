library(shiny)
library(purrr)
library(dplyr)
library(ggplot2)
library(plotly)

#  \u00E1 \u00E9  \u00ED  \u00F3  \u00FA

# Dashboard: Lineas por municipio. (prod shiny)
#options(encoding = "UTF-8")
ui <- fluidPage(
  
  # Titulo
  titlePanel("COVID19 en M\u00E9xico. Registros reportados por la Sr\u00EDa de Salud al 28 de Mayo"),
  
  #sidebarPanel(
  
  plotlyOutput("Plot1"),
  plotlyOutput("Plot1_1"),
  
  uiOutput("unique_mun"),
  
  plotlyOutput("Plot2"),
 
  h5("* El indicador de vulnerabilidad no considera variables importantes como: Indicador de pobreza multidimencional, nutrici\u00F3n, poblaciones ind\u00EDgenas, acceso a la salud."),
  h5("* Los dados se refieren a la Fecha de Inicio de S\u00EDntomas"), 
  h5("Elaborado por Luis Alberto Flores"), 
  h5("Fuente de los datos:"),
  a("https://www.gob.mx/salud/documentos/datos-abiertos-152127ab", href="https://www.gob.mx/salud/documentos/datos-abiertos-152127ab"),
  h5(" ")
  
)

###############

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  # Carga de csv de enrrequecimiento que contiene: Nombres de Mun, CVE, POB, VA
  html_shp <- readLines('http://www.mediafire.com/file/frxzqg0j9blnq9a/nombres_mun.csv/file')
  rg_shp <- html_shp[grep("Download file", html_shp, perl = F)+1  ]
  rg2_shp <- sub("\\W+href=\\\"", "", rg_shp)
  rg3_shp <- sub("\\\">", "", rg2_shp)
  rm(html_shp)
  nom_mundf <- read.csv(rg3_shp, sep = ",", header = T, stringsAsFactors = F, encoding = "latin1")
  nom_mundf$cve_mun <- as.character(nom_mundf$cve_mun)
  
  # Adecuar claves CVE municipales (concatenar clave de entidad con clave municipal)
  nom_mundf <- nom_mundf %>% 
    mutate(cve_mun = case_when(
      nchar(.$cve_mun) == 4  ~ paste0("0", .$cve_mun),
      nchar(.$cve_mun) == 5  ~  .$cve_mun
      
    )
    )
  
  # Convertir POB a nimerico y obtener per capita
  nom_mundf$Poblacion <- as.numeric(nom_mundf$Poblacion)
  nom_mundf$PerCapita <- nom_mundf$Valor_Agregado / nom_mundf$Poblacion
  
  # Cargar csv de info covid (se actualiza el nombre de archivo. El archivo se aloja día a día en media fire)
  html_shp <- readLines('https://www.mediafire.com/file/4hqawf93yo8uawb/200528COVID19MEXICO.csv/file')
  rg_shp <- html_shp[grep("Download file", html_shp, perl = F)+1  ]
  rg2_shp <- sub("\\W+href=\\\"", "", rg_shp)
  rg3_shp <- sub("\\\">", "", rg2_shp)
  cov_mun <- read.csv(rg3_shp, sep = ",", header = T, stringsAsFactors = F)
  # "200502COVID19MEXICO.csv"
  # crear claves para el csv de covid
  cov_mun <- cov_mun %>% 
    mutate(ent_ = case_when(
      nchar(.$ENTIDAD_RES) == 1  ~ paste0("0",.$ENTIDAD_RES),
      nchar(.$ENTIDAD_RES) == 2  ~ paste0(.$ENTIDAD_RES)
    )
    )
  
  cov_mun <- cov_mun %>% 
    mutate(mun_ = case_when(
      nchar(.$MUNICIPIO_RES) == 1  ~ paste0("00",.$MUNICIPIO_RES),
      nchar(.$MUNICIPIO_RES) == 2  ~ paste0("0",.$MUNICIPIO_RES),
      nchar(.$MUNICIPIO_RES) == 3  ~ paste0(.$MUNICIPIO_RES)
    )
    )
  
  # Concatenar las claves creadas y filtrar casos positivos positivos "RESULTADO == 1"
  cov_mun <- cov_mun %>% 
    mutate(cve_mun = paste0(ent_, mun_))
  cov_mun <-  cov_mun %>% filter(RESULTADO == 1)
  cov_mun$FECHA_SINTOMAS  <- as.Date(as.character(cov_mun$FECHA_SINTOMAS ))
  cov_mun$FECHA_INGRESO <- as.Date(as.character(cov_mun$FECHA_INGRESO ))
  cov_mun <- cov_mun %>% mutate(Conteo = 1)
  cov_mun$SEXO <- as.character(cov_mun$SEXO)
  
  # join tomando la cve entre los datos de covid con datos de enrriquecimiento
  mexico_mun_prueba <- as.data.frame(cov_mun$cve_mun)
  names(mexico_mun_prueba) <- "cve_mun"
  cov_mun <- left_join(x = cov_mun, y = nom_mundf, by= "cve_mun")
  
  
  
  #########################
  # agregaciones
  ########################
  
  # acum nacional y porcentaje (grafico de total nacional)
  casos_by_fecha <- cov_mun %>% group_by(FECHA_SINTOMAS) %>% summarise(count = n()) %>% mutate(Acum = cumsum(count))
  increment <- map_dbl(1:length(casos_by_fecha$count),  ~casos_by_fecha$Acum[(.x+1)] / casos_by_fecha$Acum[.x])
  incremento <- c(0, increment[1:(length(increment)-1)])
  casos_by_fecha$incremento <- incremento
  
  # por mun confirmados y decesos (grafico de barras)
  casos_by_mun <- cov_mun  %>% group_by(nom_mun) %>% summarise(Conteo = n()) %>% arrange(desc(Conteo))
  cov_mun_deceso <- cov_mun %>% filter(FECHA_DEF != "9999-99-99") %>%  group_by(nom_mun) %>% summarise(Conteo = sum(Conteo)) %>% arrange(desc(Conteo))
  casos_by_cve_mun <- cov_mun  %>% group_by(cve_mun, nom_mun) %>% summarise(Conteo = n()) %>% arrange(desc(Conteo))
  # indice
  mun_casos_indice <- right_join(x = casos_by_cve_mun, y = nom_mundf, by= "cve_mun") %>%  mutate(medida=PerCapita * Conteo/100000)
  mun_casos_indice <- mun_casos_indice %>% mutate(indice = 1 - (medida / sum(mun_casos_indice$medida, na.rm = T))) %>% arrange(desc(indice))
  
  # fecha y municipio confirmados (graficos de lineas por municipio)
  cov_fecha_num <- cov_mun %>% group_by(nom_mun, FECHA_SINTOMAS) %>% summarise(Suma_Acum = sum(Conteo)) %>% 
    arrange(FECHA_SINTOMAS) %>% group_by(nom_mun) %>% mutate(Suma_Acum2 = cumsum(Suma_Acum))
  
  
  # mun y sexo (barras final)
  conteo_sexo <- cov_mun %>% select(nom_mun, SEXO) %>% group_by(nom_mun, SEXO) %>% summarise(conteo = n()) 
  conteo_sexo$SEXO <- as.factor(conteo_sexo$SEXO)
  
  # descesos acum por fecha (no se usa)
  cov_fecha_mun_desce <- cov_mun %>% filter(FECHA_DEF != "9999-99-99") %>%  group_by(nom_mun, FECHA_DEF) %>% summarise(Suma_Acum = sum(Conteo)) %>% 
    arrange(FECHA_DEF) %>% group_by(nom_mun) %>% mutate(Suma_Acum2 = cumsum(Suma_Acum))
  
  # tabla municipio, sexo y edad (graficos de distribucion)
  x <- cov_mun %>% select(nom_mun, SEXO, EDAD)
  # media
  mu <- cov_mun %>% select(SEXO, EDAD) %>% group_by(SEXO) %>% summarise(media = mean(EDAD))
  
  
  # nacional lineas acumulado
  output$Plot1 <- renderPlotly({
    p1 <- ggplot(casos_by_fecha, aes(x = FECHA_SINTOMAS, y = cumsum(count)))  + geom_line() +
      geom_point() +
      ggtitle("Suma acumulada de casos positivos a nivel Nacional") +
      xlab("Fecha de Inicio de S\u00EDntomas") + ylab("Suma Acumulada")
    ggplotly(p1)
  })
  
  # nacional lineas registro por dia
  output$Plot1_1 <- renderPlotly({
    p1 <- ggplot(casos_by_fecha, aes(x = FECHA_SINTOMAS, y = (count)))  + geom_line() +
      geom_point() +
      ggtitle("Suma acumulada de casos positivos a nivel Nacional") +
      xlab("Fecha de Inicio de S\u00EDntomas") + ylab("Registros positivos diarios")
    ggplotly(p1)
  })
  
 
  
 ############# reactividad de boton
  
  output$unique_mun <- renderUI({
    df <- cov_mun
    opti <- as.character(sort(unique(df$nom_mun)))
    opti <- c('Nacional', opti)
    #options <- options
    selectInput(
      inputId = "mun_selection",
      label = "Municipio",
      choices = opti,
      selected = c(mun_casos_indice[1,"nom_mun.y"], casos_by_mun[1,"nom_mun"]),
      multiple = TRUE
      #selected = "All"
    )
  })
  
  # grafico de lineas por municipio
  
  output$Plot2 <- renderPlotly({
    if(input$mun_selection == 'Nacional'){
      p2 <- ggplot(casos_by_fecha, aes(x = FECHA_SINTOMAS, y = (cumsum(count)))) + geom_line() +  geom_point() +
        ggtitle("Suma acumulada de casos positivos por Estado") +
        xlab("Fecha de inicio de S\u00EDntomas") + ylab("Suma Acumulada")
      ggplotly(p2)
    }else{
      p2 <- ggplot(cov_fecha_num[cov_fecha_num$nom_mun %in%  c(input$mun_selection),], aes(x = FECHA_SINTOMAS, y = (Suma_Acum2), col = nom_mun)) + geom_line() +
        geom_point() + ggtitle(paste(input$mun_selection,"Suma acumulada de casos positivos")) +
        xlab("Fecha de inicio de S\u00EDntomas") + ylab("Suma Acumulada")
      ggplotly(p2)
    }
  })
  
  
  
  
}

shinyApp(ui, server)
