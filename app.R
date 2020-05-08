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
  titlePanel("COVID19 en M\u00E9xico. Registros reportados por la Sr\u00EDa de Salud al 8 de Mayo"),
  
  #sidebarPanel(
 
  plotlyOutput("Plot1"),
  plotlyOutput("Plot1_1"),
  plotlyOutput("barPlot_mun_casos"),
  plotlyOutput("barPlot_mun_dece"),
  plotlyOutput("barPlot_mun_indice"),
  #plotlyOutput("Plot3"),
  
  uiOutput("unique_mun"),
  
  plotlyOutput("Plot2"),
  plotlyOutput("disPlot1"),
  plotlyOutput("disPlot2"),
  plotlyOutput("barPlot"),
  h5("* El indicador de vulnerabilidad no considera variables importantes como: Indicador de pobreza multidimencional, nutrici\u00F3n, poblaciones ind\u00EDgenas, acceso a la salud."),
  h5("Elaborado por Luis Alberto Flores"), 
  h5("Fuente de los datos:"),
  a("https://www.gob.mx/salud/documentos/datos-abiertos-152127ab", href="https://www.gob.mx/salud/documentos/datos-abiertos-152127ab"),
  h5(" ")
  
)

###############

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  # carga csv de NOmbres de Mun, con cve, pob, Va.
  html_shp <- readLines('http://www.mediafire.com/file/frxzqg0j9blnq9a/nombres_mun.csv/file')
  rg_shp <- html_shp[grep("Download file", html_shp, perl = F)+1  ]
  rg2_shp <- sub("\\W+href=\\\"", "", rg_shp)
  rg3_shp <- sub("\\\">", "", rg2_shp)
  rm(html_shp)
  nom_mundf <- read.csv(rg3_shp, sep = ",", header = T, stringsAsFactors = F, encoding = "latin1")
  nom_mundf$cve_mun <- as.character(nom_mundf$cve_mun)
  # "nombres_mun.csv"
  # arreglar claves.
  nom_mundf <- nom_mundf %>% 
    mutate(cve_mun = case_when(
      nchar(.$cve_mun) == 4  ~ paste0("0", .$cve_mun),
      nchar(.$cve_mun) == 5  ~  .$cve_mun
      
    )
    )
  
  nom_mundf$Poblacion <- as.numeric(nom_mundf$Poblacion)
  nom_mundf$PerCapita <- nom_mundf$Valor_Agregado / nom_mundf$Poblacion
  

  # CArgar info covid (se actualiza el nombre de archivo, previamente se suebe a media fire)
  html_shp <- readLines('https://www.mediafire.com/file/z7rj0fybgsw0i15/200506COVID19MEXICO.csv/file')
  rg_shp <- html_shp[grep("Download file", html_shp, perl = F)+1  ]
  rg2_shp <- sub("\\W+href=\\\"", "", rg_shp)
  rg3_shp <- sub("\\\">", "", rg2_shp)
  cov_mun <- read.csv(rg3_shp, sep = ",", header = T, stringsAsFactors = F)
  # "200502COVID19MEXICO.csv"
  # crear claves
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
  
  # unir claves, filtrar positivos
  cov_mun <- cov_mun %>% 
    mutate(cve_mun = paste0(ent_, mun_))
  cov_mun <-  cov_mun %>% filter(RESULTADO == 1)
  cov_mun$FECHA_SINTOMAS  <- as.Date(as.character(cov_mun$FECHA_SINTOMAS ))
  cov_mun$FECHA_INGRESO <- as.Date(as.character(cov_mun$FECHA_INGRESO ))
  cov_mun <- cov_mun %>% mutate(Conteo = 1)
  cov_mun$SEXO <- as.character(cov_mun$SEXO)
  
  # join por cve de: datos de covid con datos de poblacion
  mexico_mun_prueba <- as.data.frame(cov_mun$cve_mun)
  names(mexico_mun_prueba) <- "cve_mun"
  cov_mun <- left_join(x = cov_mun, y = nom_mundf, by= "cve_mun")
  
  
  
  #########################
  # agregaciones
  
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
  
  ########## Barras por municipos
  
  output$barPlot_mun_casos <- renderPlotly({
    
    #if(input$mun_selection == 'NULL'){
    p7 <-ggplot(data = casos_by_mun[1:12,], aes(x = reorder(nom_mun, -Conteo), y=(Conteo), fill = nom_mun)) +
      geom_bar(stat = "identity", width=0.5) +
      #scale_color_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb"))+
      #scale_fill_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb")) +
      labs(title = "N\u00FAmero de casos por Municipio") +
      theme(axis.text.x=element_text(angle = -25, vjust = 0.5)) +
      xlab("Municipios") + ylab("Casos Positivos")
    ggplotly(p7)
  })
  
  output$barPlot_mun_dece <- renderPlotly({
    
    #if(input$mun_selection == 'NULL'){
    p8 <-ggplot(data = cov_mun_deceso[1:12,], aes(x = reorder(nom_mun, -Conteo), y=(Conteo), fill = nom_mun)) +
      geom_bar(stat = "identity", width=0.5) +
      #scale_color_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb"))+
      #scale_fill_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb")) +
      labs(title = "N\u00FAmero de decesos por Municipio") + 
      theme(axis.text.x=element_text(angle = -25, vjust = 0.5)) +
      xlab("Municipios") + ylab("Decesos")
    ggplotly(p8)
  })
  
  output$barPlot_mun_indice <- renderPlotly({
    
    #if(input$mun_selection == 'NULL'){
    p9 <-ggplot(data = mun_casos_indice[1:12,], aes(x = reorder(nom_mun.y, -indice), y=(indice), fill = nom_mun.y)) +
      geom_bar(stat = "identity", width=0.5) +
      #scale_color_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb"))+
      #scale_fill_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb")) +
      labs(title = "Vulnerabilidad ( 1 - Per c\u00E1pita de Valor Agregado * % de casos cada 100 mil hab)") +
      theme(axis.text.x=element_text(angle = -25, vjust = 0.5)) +
      xlab("Municipios") + ylab("Vulnerabilidad")
    ggplotly(p9)
  })
  
  # Econ\u00F3mica
  
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
  
  
  #p2 <- ggplot(cov_fecha_num[cov_fecha_num$nom_mun %in% c("Acámbaro", "Acambay"),], aes(x = FECHA_SINTOMAS, y = (Suma_Acum2), col = nom_mun)) + geom_line() +
  #  ggtitle(paste(input$mun_selection,"Suma acumulada de casos positivos")) +
  #  xlab("Registro") + ylab("Suma Acumulada")
  #ggplotly(p2)
  
  
  #output$Plot3 <- renderPlotly({
  #  p3<-ggplot(cov_fecha_num, aes(x = FECHA_SINTOMAS, y = (Suma_Acum2), col = Estado)) + geom_line() +
  #    ggtitle("Suma acumulada de casos positivos por Estados") +
  #    xlab("Registro") + ylab("Suma Acumulada")
  #  ggplotly(p3)
  #})
  
  
  fil_conteo_sexo <- eventReactive(c(input$mun_selection), {
    if(input$mun_selection == 'Nacional'){
      conteo_sexo  %>% group_by( SEXO) %>% summarise(conteo =sum(conteo) )
    }else{conteo_sexo %>% filter(nom_mun %in% input$mun_selection) %>% group_by(SEXO) %>% summarise(conteo =sum(conteo) )
    }
  })
  
  output$disPlot1 <- renderPlotly({
    if(input$mun_selection == 'Nacional'){
      p4 <-ggplot(x[x$SEXO == "1",], aes(x = EDAD, color = SEXO, fill = SEXO)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
        geom_density(alpha=0.4)+
        geom_vline(data=mu[mu$SEXO == "1",], aes(xintercept=media),
                   linetype="dashed")+
        scale_color_manual(values=c("#fca6ea", "#fca6ea"))+
        scale_fill_manual(values=c("#fca6ea", "#fca6ea"))+
        labs(title="Distribuci\u00F3n de edad en pob Femenina",x="Edad", y = "Density")
      ggplotly(p4) 
    }else{
      p4 <-ggplot(x[x$SEXO == "1" & x$nom_mun == input$mun_selection,], aes(x = EDAD, color = SEXO, fill = SEXO)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
        geom_density(alpha=0.4)+
        geom_vline(data=mu[mu$SEXO == "1",], aes(xintercept=media),
                   linetype="dashed")+
        scale_color_manual(values=c("#fca6ea", "#fca6ea"))+
        scale_fill_manual(values=c("#fca6ea", "#fca6ea"))+
        labs(title=paste("Distribuci\u00F3n de edad en pob Femenina"),x="Edad", y = "Density")
      ggplotly(p4)
    }
  })
  
  output$disPlot2 <- renderPlotly({
    if(input$mun_selection == 'Nacional'){
      p5 <- ggplot(x[x$SEXO == "2" ,], aes(x=EDAD, color=SEXO, fill=SEXO)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
        geom_density(alpha=0.4)+
        geom_vline(data=mu[mu$SEXO == "2",], aes(xintercept=media),
                   linetype="dashed")+
        scale_color_manual(values=c("#33CFFF","#fca6ea"))+
        scale_fill_manual(values=c("#33CFFF","#fca6ea"))+
        labs(title="Distribuci\u00F3n de edad en pob Masculina",x="Edad", y = "Density")
      ggplotly(p5)
    }else{
      p5 <- ggplot(x[x$SEXO == "2" & x$nom_mun == input$mun_selection,], aes(x=EDAD, color=SEXO, fill=SEXO)) +
        geom_histogram(aes(y=..density..), position="identity", alpha=0.3)+
        geom_density(alpha=0.4)+
        geom_vline(data=mu[mu$SEXO == "2",], aes(xintercept=media),
                   linetype="dashed")+
        scale_color_manual(values=c("#33CFFF","#fca6ea"))+
        scale_fill_manual(values=c("#33CFFF","#fca6ea"))+
        labs(title=paste("Distribuci\u00F3n de edad en pob Masculina"),x="Edad", y = "Density")
      ggplotly(p5)
    }
  })
  
  
  # grafico de barras por sexo. Toma datos reactivos de acuerod al municipio
  output$barPlot <- renderPlotly({
    conteo_sexo_2 <- fil_conteo_sexo()
    
    #if(input$mun_selection == 'NULL'){
    p6 <-ggplot(data = conteo_sexo_2, aes(x=SEXO, y=(conteo), fill = SEXO)) +
      geom_bar(stat = "identity", width=0.5) +
      scale_color_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb"))+
      scale_fill_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb")) +
      labs(title = "N\u00FAmero de casos por sexo") +
      xlab("Sexo") + ylab("Casos Positivos")
    ggplotly(p6)
    #}else{
    #  p6 <-ggplot(data=conteo_sexo[conteo_sexo_2$cve_mun == input$mun_selection,], aes(x=SEXO, y=(conteo), fill = SEXO)) +
    #    geom_bar(stat="identity", width=0.5) +
    #    scale_color_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb"))+
    #    scale_fill_manual(values=c("#fca6ea", "#33CFFF", "#2b97eb")) +
    #    labs(title=paste(input$mun_selection,"Numero de casos por sexo"))
    #  ggplotly(p6)
    #}
  })
}

shinyApp(ui, server)
