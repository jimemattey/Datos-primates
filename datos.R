# CARGA DE PAQUETES
library(ggplot2)
library(graphics)
library(plotly)
library(dbplyr)
library(dplyr)
library(DT)
library(leaflet)
library(sf)
library(raster)
library(tidyverse)
library(st_drop_geometry)
library(PASWR2)
library(spData)
library(rgdal)

library(sf)

library(dplyr)
library(spData)

library(leaflet)
library(plotly)
library(DT)
# CARGA ARCHIVO 
inp<- 
  read.csv("primates-cr-registros.csv", sep = "\t")


View(inp)


#TABLA de datos 
inp %>% st_drop_geometry(.) %>%
  select(family,
         species,stateProvince,
         locality,eventDate) %>%
  DT::datatable(
    colnames = c("Familia", "Especie","Provincia","Localidad", "Fecha"),
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE,
      language= list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      pageLength = 15), class= "cell-border stripe")






dataprimate<-inp %>% group_by(inp$species)








# Grafico de pastel
primates <- inp[,c('species', 'eventDate')]

# Promedio y Grafico
primates <- inp[,c('species', 'eventDate')]
data <-primates %>%
  group_by(especies = format(species)) %>%
  summarize(suma_registros = n())


data$suma_registros<- as.numeric(data$suma_registros)


 ""



#GRAFICO

plot_ly(data, labels = ~especies, values = ~suma_registros, 
        type = 'pie') %>% config(locale= "es")



# Mapa





inp %>%
  select(family,
         species,stateProvince,
         locality,eventDate) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'green',
    fillOpacity = 1,
    popup = paste(
     inp$family,
     inp$species,
     inp$stateProvince,
     inp$locality,
     inp$eventDate,
      sep = '<br/>'),
    group = "species") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Stamen Toner Lite", "Imágenes de ESRI"),
    overlayGroups = c("inp (species)")
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE
  )

library(dplyr)
dataprimates <-dplyr::select(inp$family,
                      inp$species,
                      inp$stateProvince,
                      inp$locality,
                      inp$eventDate) %>% group_by(inp$species, format(inp$species, format="species"))

str(inp1)

# MAPA 

inp1 <-
  st_read(
    "primates-cr-registros.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"),
    quiet = TRUE)

st_crs(inp1) = 4326
library(dbplyr)

especies primates <- inp1= group_by(species)

# Mapa de registros de presencia
mapa <- inp1 %>%
  select(family,
         species, stateProvince,
         locality,
         eventDate) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'green',
    fillOpacity = 1,
    popup = paste(
      inp$family,
      inp1$species,
      inp1$stateProvince,
      inp1$locality,
      inp1$eventDate,
      sep = '<br/>'),
    group = "species") %>% addLayersControl(
      baseGroups = c("OpenStreetMap", "Imágenes de ESRI"),
      overlayGroups = c("inp1")) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE)



%>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'blue',
    fillOpacity = 1,
    popup = paste(
      inp$family,
      inp1$species,
      inp1$stateProvince,
      inp1$locality,
      inp1$eventDate,
      sep = '<br/>'
    ),
    group = "species",
    layerId = "species"
  ) %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'blue',
    fillOpacity = 1,
    popup = paste(
      inp$family,
      inp1$species,
      inp1$stateProvince,
      inp1$locality,
      inp1$eventDate,
      sep = '<br/>'
    ),
    group = "species",
    layerId = "species")
  ) %>%
  addCircleMarkers(
    stroke = F,
    radius = 4,
    fillColor = 'blue',
    fillOpacity = 1,
    popup = paste(
      inp$family,
      inp1$species,
      inp1$stateProvince,
      inp1$locality,
      inp1$eventDate,
      sep = '<br/>'),
    group = "species",
    layerId = "species")
  
  
  
  
  
  
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Imágenes de ESRI"),
    overlayGroups = c("inp1")
  ) %>%
  addMiniMap(
    tiles = providers$Stamen.OpenStreetMap.Mapnik,
    position = "bottomleft",
    toggleDisplay = TRUE)



  alt <- getData(
    "worldclim",
    var = "alt",
    res = .5,
    lon = -84,
    lat = 10
  )
  
  altitud <- crop(alt, extent(-86, -82.3, 8, 11.3))
  
  
  class(altitud)
  
  nlayers(altitud)
  altitud <-
    alt %>%
    crop(provincias) %>%
    mask(provincias)
  
  
  
  
  
 ### SUPER IMPORTANTE 
  
mapa %>%
  addRasterImage(
    altitud, 
    colors = pal, 
    opacity = 0.8) 








%>%  addCircleMarkers(
      stroke = F,
      radius = 4,
      fillColor = 'blue',
      fillOpacity = 1,
      popup = paste(
        inp$family,
        inp1$species,
        inp1$stateProvince,
        inp1$locality,
        inp1$eventDate,
        sep = '<br/>'),
      group = "species")
  
  
  
  
  
  
  
  
  
  alt <- getData(
    "worldclim",
    var = "alt",
    res = .5,
    lon = -84,
    lat = 10

    altitud <- crop(alt, extent(-86, -82.3, 8, 11.3))
  
  
  
  
  
  # Mapa de registros de presencia
  inp1 %>%
    select(family,
           species, stateProvince,
           locality,
           eventDate) %>% leaflet(
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes de ESRI") %>%
    addCircleMarkers(
      stroke = F,
      radius = 4,
      fillColor = 'green',
      fillOpacity = 1,
      popup = paste(
        inp$family,
        inp1$species,
        inp1$stateProvince,
        inp1$locality,
        inp1$eventDate,
        sep = '<br/>'
      ),
      group = "species"
    ) %>%
    addCircleMarkers(
      stroke = F,
      radius = 4,
      fillColor = 'blue',
      fillOpacity = 1,
      popup = paste(
        inp$family,
        inp1$species,
        inp1$stateProvince,
        inp1$locality,
        inp1$eventDate,
        sep = '<br/>'),
      group = "species",
      layerId = "species") %>% addLayersControl(
        baseGroups = c("OpenStreetMap", "Imágenes de ESRI"),
        overlayGroups = c("inp1")
      ) %>%
    addMiniMap(
      tiles = providers$Stamen.OpenStreetMap.Mapnik,
      position = "bottomleft",
      toggleDisplay = TRUE) %>% leaflet() %>%
    addTiles() %>%
    addRasterImage(
      altitud, 
      colors = pal, 
      opacity = 0.8)
    
    
    pal <- colorNumeric(
      c("#006400", "#FFFF00", "#0000FF"), 
      values(altitud), 
      na.color = "transparent")
    
    
    leaflet() %>%
    addTiles() %>%
    addRasterImage(
      altitud, 
      colors = pal, 
      opacity = 0.8)