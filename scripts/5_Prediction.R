
#------------------------------------------------------------------------------#
#
#                             5 - PREDICTION
#
#------------------------------------------------------------------------------#

#Limpieza y alistamiento de la BD Test----

  test <- test %>% mutate(latp=lat,longp=lon)
  test <- st_as_sf(test,coords=c('longp','latp'),crs=4326)

  sapply(test, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
  
  filtro <- is.na(test$surface_total) #Trasnsformamos los NA a ceros
  test$surface_total[filtro] <- 0
  
  filtro <- is.na(test$surface_covered) #Trasnsformamos los NA a ceros
  test$surface_covered[filtro] <- 0
  
  filtro <- is.na(test$lat) | is.na(test$lon) #| is.na(test$rooms) | is.na(test$bathrooms) #| is.na(test$surface_total) | is.na(test$surface_covered) 
  test <- test[!filtro, ] #Filtramos los registros sin lat y lon
  
  limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
  filtro1 <- between(test$lon, limites[1, "min"], limites[1, "max"])
  filtro2 <- between(test$lat, limites[2, "min"], limites[2, "max"])
  filtro <- filtro1 & filtro2
  test <- test[filtro,] 
  
  test <- test %>% mutate(area_maxima = ifelse(surface_total>surface_covered, surface_total, surface_covered)) #Seleccionamos el área máxima
  
  sapply(test, function(x) sum(is.na(x))) %>% as.data.frame() #Revisamos los NA de las variables
  
  prop.table(table(test$area_maxima == 0)) %>% as.data.frame() #Existen muchos lugares sin área
  table(test$area_maxima == 0) %>% as.data.frame()
  
  table(test$property_type) %>% as.data.frame() #Distribución entre Casas y Apartamentos
  table(test$operation_type) %>% as.data.frame() #Validamos la operación de venta
  
  test$property_type <- as.factor(test$property_type) #Transformamos la variable de propiedad como factor
  
  glimpse(test)


#Predicciones para Kaggle----




