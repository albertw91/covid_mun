#library(shiny)
library(purrr)
library(dplyr)
library(leaflet)
library(rgdal)

# mapa GIS municipal (prod shiny)

ui <- fluidPage(
  
  # App title -a---
  titlePanel("COVID19. Positivos  registrados por la Sria de Salud"),
  
  #sidebarPanel(
  fluidPage(
    textOutput("selected_var"),
    
    leafletOutput("map")
    #valueBoxOutput("vbox")
    #fileInput("datafile", "Upload a file")

  )


)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
   #D A T A
  #/srv/connect/apps/prueba_shiny
  
  
  ################ buscando en la pagina de internat ###########
  
  
  html_cov <- readLines('https://anonfile.com/1dtel1s4o0/200419COVID19MEXICO_csv')

  rg <- html_cov[grepl("https.+200419COVID19MEXICO\\.csv", html_cov, perl = F)]
  
  rg2 <- sub(".+href=\\\"", "", rg)
  rg3 <- sub("\\\"><img", "", rg2)
  
  cov_mun <- read.csv(rg3, sep = ",", header = T, stringsAsFactors = F)
  rm(html_cov)
  
  ## tratamiento a cov num
  
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
  cov_mun <- cov_mun %>% 
    mutate(cve_mun = paste0(ent_, mun_))
  
  cov_mun$FECHA_SINTOMAS  <- as.Date(as.character(cov_mun$FECHA_SINTOMAS ))
  
  #cov_mun %>% group_by(FECHA_SINTOMAS) %>% summarise(count = n())
  
  casos_by_mun <- cov_mun %>% filter(RESULTADO == 1) %>% group_by(cve_mun) %>% summarise(count = n())
  
  rm(cov_mun)

  # en zip
  # no zip http://www.mediafire.com/file/9m2k1y5duh5ikzv/Municipios_2013.shp/file
  # zip http://www.mediafire.com/file/i4yc65u3jslj988/702825292829_s.zip/file
                         
  html_shp <- readLines('http://www.mediafire.com/file/9m2k1y5duh5ikzv/Municipios_2013.shp/file')
  
  rg_shp <- html_shp[grep("Download file", html_shp, perl = F)+1  ]
  rg2_shp <- sub("\\W+href=\\\"", "", rg_shp)
  rg3_shp <- sub("\\\">", "", rg2_shp)
  
  download.file(rg3_shp, "Municipios_2013.shp")
  rm(html_shp)
  
  # prj
  
  html_prj <- readLines('http://www.mediafire.com/file/gnmlfs985vmrv52/Municipios_2013.prj/file')
 
  rg_prj <- html_prj[grep("Download file", html_prj, perl = F)+1  ]
  rg2_prj <- sub("\\W+href=\\\"", "", rg_prj)
  rg3_prj <- sub("\\\">", "", rg2_prj)
  
  download.file(rg3_prj, "Municipios_2013.prj")
  rm(html_prj)

  # dbf
  
  html_dbf <- readLines('http://www.mediafire.com/file/fj7uk57ah915p2x/Municipios_2013.dbf/file')
  
  rg_dbf <- html_dbf[grep("Download file", html_dbf, perl = F)+1  ]
  rg2_dbf <- sub("\\W+href=\\\"", "", rg_dbf)
  rg3_dbf <- sub("\\\">", "", rg2_dbf)
  
  download.file(rg3_dbf, "Municipios_2013.dbf")
  rm(html_dbf)
  
  # shx
  
  html_shx <- readLines('http://www.mediafire.com/file/mqkfj6bxzxmomm0/Municipios_2013.shx/file')
  
  rg_shx <- html_shx[grep("Download file", html_shx, perl = F)+1  ]
  rg2_shx <- sub("\\W+href=\\\"", "", rg_shx)
  rg3_shx <- sub("\\\">", "", rg2_shx)
  
  download.file(rg3_shx, "Municipios_2013.shx")
  rm(html_shx)
  
  # xml
  
  html_xml <- readLines('http://www.mediafire.com/file/fyu5wx1ps6uflnk/Municipios_2013.xml/file')
  
  rg_xml <- html_xml[grep("Download file", html_xml, perl = F)+1  ]
  rg2_xml <- sub("\\W+href=\\\"", "", rg_xml)
  rg3_xml <- sub("\\\">", "", rg2_xml)
  
  download.file(rg3_xml, "Municipios_2013.xml")
  rm(html_xml)
  
  # obtener la clave de municipio de covid municipal
  
 
  ##unzip("/srv/connect/apps/covid_mun/702825292829_s/mgm2013v6_0.zip", exdir = "/srv/connect/apps/covid_mun/702825292829_s/mgm2013v6_0")
  #
  mexico_mun <- readOGR("Municipios_2013.shp",  verbose = FALSE)
  
  mexico_mun <- spTransform(mexico_mun, CRS("+init=epsg:4326"))
  
  #mexico_mun <- read_sf(dsn = "Municipios_2013.shp")
  #mexico_mun <- st_transform(mexico_mun, crs = "+init=epsg:4326")
  #Encoding( x = mexico_mun$NOM_MUN ) <- "UTF-16"
  #
  #
  ## tratamiento al shape
  
  mexico_mun$cve_mun <- paste0(mexico_mun$CVE_ENT, mexico_mun$CVE_MUN)
  mexico_mun_prueba <- as.data.frame(mexico_mun$cve_mun)
  names(mexico_mun_prueba) <- "cve_mun"

  ########################## join de covid con shape
  
  join_covid_shape <- left_join(x = mexico_mun_prueba, y = casos_by_mun, by= "cve_mun")
  
  join_covid_shape[is.na(join_covid_shape$count),"count"] <- 0 
  
  mexico_mun$count <- join_covid_shape$count
  
  rm(mexico_mun_prueba, join_covid_shape, casos_by_mun)
  
  mexico_mun$CVE_MUN <- NULL
  mexico_mun$CVE_ENT <- NULL
  mexico_mun$cve_mun <- NULL
  
  
  ##############3 otra data
  
  
  ## O U T P U T S
  
  mun_popup <- paste0("<strong>Municipio: </strong>", 
                      mexico_mun$NOM_MUN, 
                      "<br><strong>Casos Postivos: </strong>", 
                      mexico_mun$count)
  
  pal <- colorNumeric("YlGn", NULL, n = 5)
  
  
  
  #output$vbox <- renderValueBox({
  #  valueBox(
  #    "Title",
  #    conteo_nac,
  #    icon = icon("list"),
  #    color = "purple"
  #  )
  #})
  #
  output$map <- renderLeaflet({
    
    # grafico
    
    a <- leaflet() %>%
      addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
      addPolygons(data = mexico_mun,
                  fillColor = ~pal(mexico_mun$count),
                  color = "#444444",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  popup = mun_popup,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)
      )
  })
  

  
  output$selected_var <- renderText({ 
    list.files(paste0(getwd())) # si accesa
    #list.files(paste0(getwd(), "/Municipios_2013"))
    #list.files(paste0(getwd(), "/702825292829_s"))
    #as.character(paste0(cov_mun[1,]), list.files(getwd()), sep="", collapse="" )
    
  })
  
}

shinyApp(ui, server)
