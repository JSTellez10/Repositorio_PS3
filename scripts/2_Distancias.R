
#Variables del entorno geográfico----
 
 available_features()
 available_tags("boundary")
 
 bogota <- opq(bbox = getbb("Bogotá Colombia"))
 
 ##Bogotá----
 bogota_polig <- getbb(place_name = "Bogotá", 
                    featuretype = "boundary:administrative", 
                    format_out = "sf_polygon") %>% .$multipolygon
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=bogota_polig, color = "#3A5FCD", opacity = 0.5)
 
 
 ##Parques----
 parques <- bogota %>% add_osm_feature(key="leisure",value="park") %>% osmdata_sf()
 parques_geom <- parques$osm_polygons %>% select(osm_id, name)
 centroides_parques <- gCentroid(as(parques_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=parques_geom, color = "#2E8B57", opacity = 0.8, popup = parques_geom$name) %>%
   addCircles(lng = centroides_parques$x, lat = centroides_parques$y, col = "#FF7F00", opacity = 0.8, radius = 0.5)
 
 
 ##Paradas de buses----
 buses <- bogota %>% add_osm_feature(key="highway",value="bus_stop") %>% osmdata_sf()
 buses_puntos <- buses$osm_point
 
 leaflet() %>%
   addTiles() %>%
   addCircles(data=buses_puntos, color = "#27408B", opacity = 0.7)
 
 
 ##Centros Comerciales----
 mall <- bogota %>% add_osm_feature(key="shop",value="mall") %>% osmdata_sf()
 mall_geom <- mall$osm_polygons %>% select(osm_id, name)
 centroides_mall <- gCentroid(as(mall_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=mall_geom, color = "#8B4789", opacity = 1, popup = mall_geom$name)  %>%
   addCircles(lng = centroides_mall$x, lat = centroides_mall$y, col = "#FF7F00", opacity = 1, radius = 0.5)
 
 
 ##Estaciones de Transmilenio----
 estaciones_tm <- bogota %>% add_osm_feature(key="amenity", value = "bus_station") %>% osmdata_sf()
 estaciones_tm_puntos <- estaciones_tm$osm_points
 
 leaflet() %>%
   addTiles() %>%
   addCircles(data=estaciones_tm_puntos, color = "#00868B", opacity = 1)
 
 
 ##Vías de transmilenio----
 vias_tm <- bogota %>% add_osm_feature(key="highway", value = "service") %>% osmdata_sf()
 vias_tm_lineas <- vias_tm$osm_lines %>% filter(grepl("TransMilenio" ,name))

 leaflet() %>%
   addTiles() %>%
   addPolylines(data = vias_tm_lineas, color = "#CD3700")
 
 
 ##Centros Médicos----
 salud <- bogota %>% add_osm_feature(key="amenity", value = c("clinic","hospital")) %>% osmdata_sf()
 salud_geom <- salud$osm_polygons %>% select(osm_id, name)
 centroides_salud <- gCentroid(as(salud_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=salud_geom, color = "#D8152F", opacity = 1, popup = salud_geom$name)  %>%
   addCircles(lng = centroides_salud$x, lat = centroides_salud$y, col = "#0000FF", opacity = 0.8, radius = 0.5)

 
 ##Seguridad----
 seguridad <- bogota %>% add_osm_feature(key="amenity", value = "police") %>% osmdata_sf()
 seguridad_geom <- seguridad$osm_polygons %>% select(osm_id, name)
 centroides_seguridad <- gCentroid(as(seguridad_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=seguridad_geom, color = "#D8152F", opacity = 1, popup = seguridad_geom$name)  %>%
   addCircles(lng = centroides_seguridad$x, lat = centroides_seguridad$y, col = "#0000FF", opacity = 0.8, radius = 0.5)
 
  ##Bibliotecas----
 bibliotecas <- bogota %>% add_osm_feature(key="amenity", value = "library") %>% osmdata_sf()
 bibliotecas_geom <- bibliotecas$osm_polygons %>% select(osm_id, name)
 centroides_biblio <- gCentroid(as(bibliotecas_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=bibliotecas_geom, color = "#D8152F", opacity = 1, popup = bibliotecas_geom$name)  %>%
   addCircles(lng = centroides_biblio$x, lat = centroides_biblio$y, col = "#0000FF", opacity = 0.8, radius = 0.5)
 
 
 ##Museos----
 museos <- bogota %>% add_osm_feature(key="tourism", value = "museum") %>% osmdata_sf()
 museos_geom <- museos$osm_polygons %>% select(osm_id, name)
 centroides_museos <- gCentroid(as(museos_geom$geometry, "Spatial"), byid = T)
 
 leaflet() %>%
   addTiles() %>%
   addPolygons(data=museos_geom, color = "#002AFF", opacity = 1, popup = museos_geom$name)  %>%
   addCircles(lng = centroides_museos$x, lat = centroides_museos$y, col = "#FF7F00", opacity = 0.8, radius = 0.5)
 
#Distancias----

 train_sf <- st_as_sf(train, coords = c("lon", "lat"))
 st_crs(train_sf) <- 4326
 centroides_salud_d <- st_as_sf(centroides_salud, coords = c("x", "y"))
 
 dist_matrix <- st_distance(x = train_sf, y = centroides_salud_d)
 
 dist_min <- apply(dist_matrix, 1, min)
 train$distancia_hospital <- dist_min
 train_sf$distancia_hospital <- dist_min
 
 p <- ggplot(train, aes(x = distancia_hospital)) +
      geom_histogram(bins = 50, fill = "darkblue") +
      labs(x = "Distancia mínima a un hospital metros", y = "Cantidad",
      title = "Distribución de la distancia a los hospitales") +
      theme_bw()
 
 
 p <- ggplot(train, aes(x = distancia_hospital, y = price)) +
      geom_point(col = "darkblue") +
      labs(x = "Distancia mínima a un hospital (log-scale)", 
      y = "Precio Inmueble (log-scale)",
      title = "Relación entre la proximidad a un hospital y el precio del inmueble") +
      scale_x_log10() +
      scale_y_log10(labels = scales::dollar) +
      theme_bw()
 
 posicion <- apply(dist_matrix, 1, function(x) which(min(x) == x))
 area_hospital <- st_area(salud_geom)
 train$area_hospital <- area_hospital[posicion]
 train$area_hospital <- as.numeric(train$area_hospital)
 train_sf$area_hospital <- area_hospital[posicion]
 train_sf$area_hospital <- as.numeric(train_sf$area_hospital)
 
 p <- ggplot(train, aes(x = area_hospital, y = price)) +
      geom_point(col = "darkblue", alpha = 0.4) +
      labs(x = "Área del hospital más cercano (log-scale)", 
      y = "Precio del inmueble (log-scale)",
      title = "Relación entre área de un hospital y el precio del inmueble") +
      scale_x_log10() +
      scale_y_log10(labels = scales::dollar) +
      theme_bw()
 