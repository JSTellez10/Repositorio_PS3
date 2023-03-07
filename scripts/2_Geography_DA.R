
  rm(list=ls())
  
  require("pacman")
  p_load(tidyverse, sf, tmaptools, osmdata, leaflet) 

  
#Leemos los datos geográficos

  localidades <-st_read("datos/localidades")
  parques <-st_read("datos/parques")
  museos <-st_read("datos/museos")
  ips <-st_read("datos/ips")
  ese <-st_read("datos/ese")
  colegios <-st_read("datos/colegios")
  cai <-st_read("datos/cai")

  chapinero <- getbb(place_name = "UPZ Chapinero, Bogotá",
                     featuretype = "boundary:administrative",
                     format_out = "sf_polygon") %>% .$multipolygon
  
  bogota <- getbb(place_name = "Bogotá", 
                  featuretype = "boundary:administrative", 
                  format_out = "sf_polygon") %>% .$multipolygon
  
  bogota <- st_transform(bogota, 4686)

#Transformamos los archivos de cada variable
  
  st_crs(parques)
  head(parques)
  parques <- st_transform(parques,4686)
  sf_parques = parques %>% st_sf %>% st_cast 
  head(sf_parques)


  sf_parques<-st_as_sf(parques,coords=c('longp','latp'),crs=4326)
  
#Aseguramos que todo se encuentre en los límites de Bogotá
  
  sf_parques2 <- gIntersects(sf_localidades$geometry , sf_parques$geometry)
  sf_parques2 <- st_overlaps(sf_parques,bogota)
  
  
  limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
  
  filtro1 <- between(sf_parques$long, limites[1, "min"], limites[1, "max"])
  filtro2 <- between(sf_parques$lat, limites[2, "min"], limites[2, "max"])
  filtro <- filtro1 & filtro2
  
  sf_parques <- sf_parques[filtro,]
  sf_museos <- sf_museos[filtro,]
  sf_ips <- sf_ips[filtro,]
  sf_ese <- sf_ese[filtro,]
  sf_colegios <- sf_colegios[filtro,]
  sf_cai <- sf_cai[filtro,]
  sf_localidades <- sf_localidades[filtro,]
  
#Generamos los centroides de cada variable
  
  centroides_parques  <- gCentroid(as(sf_parques$geometry, "Spatial"), byid = T)
  centroides_museos   <- gCentroid(as(sf_museos$geometry, "Spatial"), byid = T)
  centroides_ips      <- gCentroid(as(sf_ips$geometry, "Spatial"), byid = T)
  centroides_ese      <- gCentroid(as(sf_ese$geometry, "Spatial"), byid = T)
  centroides_colegios <- gCentroid(as(sf_colegios$geometry, "Spatial"), byid = T)
  centroides_cai      <- gCentroid(as(sf_cai$geometry, "Spatial"), byid = T)
  
  #localidades2 <-st_as_sf(localidades,coords=c('longp','latp'),crs=4326)
  #parques2     <-st_as_sf(parques,coords=c('longp','latp'),crs=4326)
  #museos2      <-st_as_sf(museos,coords=c('longp','latp'),crs=4326)
  #ips2         <-st_as_sf(ips,coords=c('longp','latp'),crs=4326)
  #ese2         <-st_as_sf(ese,coords=c('longp','latp'),crs=4326)
  #colegios2    <-st_as_sf(colegios,coords=c('longp','latp'),crs=4326)

  
#Mapa Completo

  leaflet() %>%
    addTiles() %>%
    addPolygons(data=bogota, color = "#5CACEE", opacity = 0.4) %>%
    addCircles(data=train, radius = 0.1, color = "#DAA520", opacity = 2) %>%
    addPolygons(data=chapinero, color = "#008B45", opacity = 0.4)%>%
    #addPolygons(data=sf_localidades, color = "#5CACEE", opacity = 1)%>%
    addCircles(lng = centroides_colegios$x, lat = centroides_colegios$y, col = "#00CDCD", opacity = 0.8, radius = 0.5, popup = sf_colegios$NOMBRE_EST)%>%
    addCircles(lng = centroides_parques$x, lat = centroides_parques$y, col = "#00EE00", opacity = 0.8, radius = 0.5, popup = sf_parques$NOMBRE)%>%
    addCircles(lng = centroides_ips$x, lat = centroides_ips$y, col = "#FF3030", opacity = 0.8, radius = 0.5, popup = sf_ips$nombre_pre)%>%
    addCircles(lng = centroides_ese$x, lat = centroides_ese$y, col = "#FF3030", opacity = 0.8, radius = 0.5, popup = sf_ese$RSOENTADSC)%>%
    addCircles(lng = centroides_museos$x, lat = centroides_museos$y, col = "#0000FF", opacity = 0.8, radius = 0.5, popup = sf_museos$LecNombre)%>%
    addCircles(lng = centroides_cai$x, lat = centroides_cai$y, col = "#008B00", opacity = 0.8, radius = 0.5, popup = sf_cai$CAIDESCRIP)

  