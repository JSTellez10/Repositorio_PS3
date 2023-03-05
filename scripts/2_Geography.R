
#------------------------------------------------------------------------------#
#
#                        2 - GEOGRAPHICAL ENVIRONMENT
#
#------------------------------------------------------------------------------#

#Variables del entorno geográfico----
 
 #available_features()       #Lo usamos para ver los tipos de objetos en el mapa
 #available_tags("boundary")
 
 bogota <- opq(bbox = getbb("Bogotá Colombia"))
 
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
 
 