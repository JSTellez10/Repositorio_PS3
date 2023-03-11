
#------------------------------------------------------------------------------#
#
#                             5 - PREDICTION
#
#------------------------------------------------------------------------------#

#Limpieza y alistamiento de la BD Test----

test <- test %>% mutate(latp=lat,longp=lon, ln_price = log(price))
test <- st_as_sf(test,coords=c('longp','latp'),crs = 4686)

#Limpieza de la BD ----

sapply(test, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables

filtro <- is.na(test$surface_total) #Transformamos los NA a ceros
test$surface_total[filtro] <- 0

filtro <- is.na(test$surface_covered) #Trasnsformamos los NA a ceros
test$surface_covered[filtro] <- 0

filtro <- is.na(test$bedrooms) #Trasnsformamos los NA a ceros #REVISAR
test$bedrooms[filtro] <- 0

filtro <- is.na(test$bathrooms) #Trasnsformamos los NA a ceros #REVISAR
test$bathrooms[filtro] <- 0

filtro <- is.na(test$rooms) #Trasnsformamos los NA a ceros #REVISAR
test$rooms[filtro] <- 0

filtro <- is.na(test$lat) | is.na(test$lon) #| is.na(test$rooms) | is.na(test$bathrooms) #| is.na(test$surface_total) | is.na(test$surface_covered) 
test <- test[!filtro, ] #Filtramos los registros sin lat y lon

limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
filtro1 <- between(test$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(test$lat, limites[2, "min"], limites[2, "max"])
filtro <- filtro1 & filtro2
test <- test[filtro,] 

test <- test %>% mutate(area_maxima = ifelse(surface_total>surface_covered, surface_total, surface_covered)) #Seleccionamos el área máxima

sapply(test, function(x) sum(is.na(x))) %>% as.data.frame() #Revisamos los NA de las variables

prop.table(table(test$area_maxima == 0)) %>% as.data.frame() #Existen muchos lugares sin área (TRUE)
table(test$area_maxima == 0) %>% as.data.frame()

table(test$property_type) %>% as.data.frame() #Distribución entre Casas y Apartamentos
test$property_type <- as.factor(test$property_type) #Transformamos la variable de propiedad como factor

table(test$operation_type) %>% as.data.frame() #Validamos que todo corresponde a Venta

glimpse(test)

test <- st_join(test, delitos)

#Distancias----

test_sf <- st_as_sf(test, coords = c("lon", "lat"))
test_sf <- st_transform(test_sf,4686)

##Parques----

dist_matrix <- st_distance(x = test_sf, y = centroides_parques_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_parque <- dist_min

##Museos----

dist_matrix <- st_distance(x = test_sf, y = centroides_museos_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_museo <- dist_min

##IPS----

dist_matrix <- st_distance(x = test_sf, y = centroides_ips_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_ips <- dist_min

##ESE----

dist_matrix <- st_distance(x = test_sf, y = centroides_ese_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_ese <- dist_min

##Colegios----

dist_matrix <- st_distance(x = test_sf, y = centroides_colegios_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_colegios <- dist_min

##CAI----

dist_matrix <- st_distance(x = test_sf, y = centroides_cai_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_cai <- dist_min

##Biblo Estaciones----

dist_matrix <- st_distance(x = test_sf, y = centroides_best_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_best <- dist_min

##Centros Financieros----

dist_matrix <- st_distance(x = test_sf, y = centroides_centrof_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_centrof <- dist_min

##Cuadrante Policía----

dist_matrix <- st_distance(x = test_sf, y = centroides_cuadrantes_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_cuadrantes <- dist_min

##Estaciones de buses----

dist_matrix <- st_distance(x = test_sf, y = buses_puntos_d)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_buses <- dist_min

##Estaciones de Transmilenio----

dist_matrix <- st_distance(x = test_sf, y = centroides_tm)
dist_min <- apply(dist_matrix, 1, min)

test$distancia_tm <- dist_min


#Predicciones para Kaggle----

test$price <- predict(EN5, newdata = test)
exportar <- test %>% select(property_id, price, -geometry) %>% as.data.frame()
exportar <- exportar %>% select(property_id, price) %>% as.data.frame()
write.csv(exportar, "submission.csv", row.names = FALSE)

