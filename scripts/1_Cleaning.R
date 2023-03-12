
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 3
#
#             ¿Ganar dinero con ML? "Se trata de la ubicación…!"
#
#       Grupo 5:  Isabella Mendez Pedraza.
#                 Manuela Ojeda Ojeda.
#                 Juan Sebastian Tellez Melo.
#                 Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

<<<<<<< HEAD
#Importante para ejecutar el código----

  #Para ejecutar el código es necesario descargar todas las carpetas que se encuentran en el siguiente repositorio:
  #https://github.com/AndresMPL/Repositorio_PS3/tree/main/datasets/localidades
  #Guardar estas carpetas en una carpeta llamada "datos" y fijar el directorio de trabajo en esta última carpeta contenedora
=======
  rm(list=ls())
>>>>>>> 20cfe1f6c072d32ac881a337b72122ba4e5fbb37

  library(pacman) 
  
  setwd("C:/Users/User/Documents/Big_Data/BD_Taller 3") 
  #Para iniciar se debe decargar todas las carpetas del siguiente sitio:
  #https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/
  #Guardarlas en una carpeta "datos" y fijar el directorio donde se encuentre esta carpeta
  
  #Cargamos las siguientes librerías verificando que no existen alertas por funciones duplicadas entre ellas
  
  p_load(tidyverse, rio, sf, dplyr, osmdata, leaflet, scales, stargazer,rstudioapi, 
         rio, plotly, modeldata, rgeos, tmaptools, IRdisplay, spatialsample, geojsonio)
  
  

 
#Lectura de los datos de Train ----

 test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/test.csv")
 train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/train.csv")
 
 glimpse(test) 
 glimpse(train) #Las dos BD de Train y Test tienen las mismas variables

#Primer análisis de la BD----
 
 est_prev <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
 stargazer(round(est_prev), digits = 4, title="Tabla de Estadísticas descriptivas", type='text')
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
 
 #Vamos a sacar los metros cuadrados a partir de la descripción de las propiedades
 p_load(tm, tidytext) 
 
 id<-train$property_id
 descripcion <- train$description
 titulo <- train$title
 
 df<-data.frame(id,descripcion,titulo)
 
 #Ponemos todo en minúscula, quitamos espacios en blanco sobrante y signos de puntuación
 
 train$descripcion <- removePunctuation(train$description)
 train$descripcion <- tolower(train$description)
 train$descripcion <- stripWhitespace(train$description)
 
 #Generamos bigramas
 
 bigrams <- train %>% ungroup() %>%
            unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)
 
 head(bigrams)
 
 #Eliminamos los bigramas que no contengan información de metros cuadrados
 
 descripcion_keep <- c("mts2", "m", "mts", "metros", "m2", "mt2")
 bigrams_keep <- data.frame(word2 = descripcion_keep)
 
 bigrams2 <- bigrams %>% ungroup() %>%
             separate(bigram, c("word1", "word2"), sep = " ") %>%
             semi_join(bigrams_keep, by = "word2") %>%
             unite(bigram, word1, sep = " ")
 
 head(bigrams2)
 
 #Se borra la plabra relacionada a metros cuadrados
 
 train2 <- bigrams2 %>% select(-c("word2"))
 colnames(train2)
 
 train2 <- transform(train2,bigram = as.numeric(bigram))
 
 sapply(train2, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
 
 filtro <- is.na(train2$bigram) #Transformamos los NA a ceros
 sum(filtro)
 train2$bigram[filtro] <- 0
 
 train2 <- train2 %>% rename(mts2=bigram)
 
 #Buscamos informacion sobre si las propiedades tienen parqueaderos / garajes o no a partir de la descripción
 
 library(tokenizers)
 train2$parqueadero <- tokenize_words(train2$description)
 descripcion_keep2 <- c("parqueadero?", "garaje?")
 train2$parqueadero <- as.logical(grepl(paste(descripcion_keep2, collapse = "|"), train2$parqueadero))
 train2$parqueadero <- as.integer(as.logical(train2$parqueadero))
 
 train_area <- train2 %>% select(property_id, mts2, parqueadero)
 
 train_area2 <- train_area %>% group_by(property_id) %>% slice(1)
 
 train <- left_join(train, train_area2)
 
 #Verificamos que los datos no hayan cambiado
     est_prev <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
     stargazer(round(est_prev), digits = 4, title="Tabla de Estadísticas descriptivas", type='text')
     sapply(train, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables


#Limpiamos la BD----
 
 train <- train %>% mutate(latp=lat,longp=lon, ln_price = log(price))
 train <- sf::st_as_sf(train,coords=c('longp','latp'),crs = 4686)
 class(train)
 head(train)
 
 
#Mapa Train----
#Mapa de distribución de los datos de entrenamiento
 
 bogota_polig <- getbb(place_name = "Bogotá", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") %>% .$multipolygon
 
 centroide_bta <- gCentroid(as(bogota_polig$geometry, "Spatial"), byid = T)
 
 localidades <- st_read("datos/localidades")
 localidades <- localidades %>% select(LocNombre, geometry)
 train <- st_join(train, localidades)
 train_est <- train %>% select(LocNombre) %>% as.data.frame()
 train_est <- train_est %>% select(-geometry) %>% as.data.frame()
 train_est <- table(train_est) %>% as.data.frame()
 localidades <- inner_join(localidades, train_est, by = "LocNombre")
 localidades <- st_transform(localidades,4326)

 pal <- colorNumeric(palette = c("#FF6600", "#0033FF"), domain = c(train$price), reverse = TRUE)
 
 map1 <- leaflet() %>%
          addProviderTiles(providers$Stamen.Toner) %>%
          addPolygons(data=localidades, opacity = 0.6, fill = FALSE, color ="#D08151", weight = 3) %>%
          addCircleMarkers(radius = 1, lng = train$lon, lat = train$lat, weight = 3, color = pal(train$price), fill = pal(train$price)) %>%
          setView(lng = centroide_bta$x, lat = centroide_bta$y, zoom = 11) %>% 
          addLegend("bottomright", pal = pal, values = train$price,
                   title = "Precios",
                   labFormat = labelFormat(prefix = "$"),
                   opacity = 1)
   
 map1
 

#Visualizar cuales de estos inmuebles son casas y cuales apartamentos
 
 color <- rep(NA,nrow(train))
 color[train$property_type == "Casa"] <- "#FFB900"
 color[train$property_type == "Apartamento"] <- "#19AF00"
   
leaflet() %>%
          addProviderTiles(providers$Stamen.Toner) %>%
          addPolygons(data=localidades, opacity = 0.5, fill = FALSE, color ="#003CFF", weight = 2.5) %>% 
          addCircles(lng = train$lon, lat = train$lat, col = color, weight = 2) %>% 
          setView(lng = centroide_bta$x, lat = centroide_bta$y, zoom = 11) %>% 
          addLegend("bottomright", labels = c("Apartamentos","Casas"), colors = c("#19AF00","#FFB900"),
          title = "Tipos de inmuebles",
          opacity = 1) 

#Limpieza de la BD ----
 
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
   
  #Imputamos las variables surface_total, surfaced_covered, bedrooms, bathrooms, rooms y guardamos las originales por separado
 
  train$surface_total_imp <- train$surface_total
  train$surface_covered_imp <- train$surface_covered
  train$bedrooms_imp <- train$bedrooms
  train$bathrooms_imp <- train$bathrooms
  train$rooms_imp <- train$rooms

 filtro <- is.na(train$surface_total) 
 sum(filtro)
 train$surface_total_imp[filtro] <- mean(train$surface_total, na.rm = T)
 
 filtro <- is.na(train$surface_covered) 
 sum(filtro)
 train$surface_covered_imp[filtro] <- mean(train$surface_covered, na.rm = T)
 
 filtro <- is.na(train$bedrooms) 
 sum(filtro)
 train$bedrooms_imp[filtro] <- mean(train$bedrooms, na.rm = T)
 
 filtro <- is.na(train$bathrooms) 
 sum(filtro)
 train$bathrooms_imp[filtro] <- mean(train$bathrooms, na.rm = T)
 
 filtro <- is.na(train$rooms)
 sum(filtro)
 train$rooms_imp[filtro] <- mean(train$rooms, na.rm = T)

##############################################################################

 
 filtro <- is.na(train$surface_total) 
 train$surface_total[filtro] <- mean(train$surface_total, na.rm = T)
 
 filtro <- is.na(train$surface_covered) 
 train$surface_covered[filtro] <- mean(train$surface_covered, na.rm = T)
 
 filtro <- is.na(train$bedrooms) 
 train$bedrooms[filtro] <- mean(train$bedrooms, na.rm = T)
 
 filtro <- is.na(train$bathrooms) 
 train$bathrooms[filtro] <- mean(train$bathrooms, na.rm = T)
 
 filtro <- is.na(train$rooms)
 train$rooms[filtro] <- mean(train$rooms, na.rm = T)
 #-----------------------------------------------------------------------------
 
 filtro <- is.na(train$surface_total_imp) 
 train$surface_total_imp[filtro] <- mean(train$surface_total_imp, na.rm = T)
 
 filtro <- is.na(train$surface_covered_imp) 
 train$surface_covered_imp[filtro] <- mean(train$surface_covered_imp, na.rm = T)
 
 filtro <- is.na(train$bedrooms_imp) 
 train$bedrooms_imp[filtro] <- mean(train$bedrooms_imp, na.rm = T)
 
 filtro <- is.na(train$bathrooms_imp) 
 train$bathrooms_imp[filtro] <- mean(train$bathrooms_imp, na.rm = T)
 
 filtro <- is.na(train$rooms_imp)
 train$rooms_imp[filtro] <- mean(train$rooms_imp, na.rm = T)
 
 filtro <- is.na(train$mts2) 
 train$mts2[filtro] <- mean(train$mts2, na.rm = T)
 
 filtro <- is.na(train$parqueadero)
 train$parqueadero[filtro] <- mean(train$parqueadero, na.rm = T)
 
 
############################################################################## 
 
 filtro <- is.na(train$lat) | is.na(train$lon) #| is.na(train$rooms) | is.na(train$bathrooms) #| is.na(train$surface_total) | is.na(train$surface_covered) 
 train <- train[!filtro, ] #Filtramos los registros sin lat y lon
 
 limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
 filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
 filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])
 filtro <- filtro1 & filtro2
 train <- train[filtro,] 
 
 train <- train %>% mutate(area_maxima = ifelse(surface_total>surface_covered, surface_total, surface_covered)) #Seleccionamos el área máxima
 
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame() #Revisamos los NA de las variables
 
 prop.table(table(train$area_maxima == 0)) %>% as.data.frame() #Existen muchos lugares sin área (TRUE)
 table(train$area_maxima == 0) %>% as.data.frame()
 
 table(train$property_type) %>% as.data.frame() #Distribución entre Casas y Apartamentos
 train$property_type <- as.factor(train$property_type) #Transformamos la variable de propiedad como factor
 
 table(train$operation_type) %>% as.data.frame() #Validamos que todo corresponde a Venta

 glimpse(train)
 
 
#Tabla de Estadísticas Descriptivas - Después de limpieza
 
 summary(train$price) %>%
   as.matrix() %>%
   as.data.frame() %>%
   mutate(V1 = scales::dollar(V1))
 
 estadisticas <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
 estadisticas <- estadisticas %>% select(-geometry) %>% as.data.frame()
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='text')
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='latex')
 
#Matriz de Correlaciones
 
 stargazer(cor(round(estadisticas, 4)), title="Tabla de Correlaciones", type='text')
 stargazer(cor(round(estadisticas, 4)), title="Tabla de Correlaciones", type='latex')

#Distribuciones de los precios
 
 price_boxplot <- ggplot() +
                  geom_boxplot(aes(y = train$price), fill = "#3FA0FF", alpha=0.5) +
                  labs(y = "Precio de venta", title = "Distribución de Precio") +
                  scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                  theme_bw() +
                  theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot + coord_flip()

#Log Precio
 
 price_boxplot_ln <-  ggplot() +
                   geom_boxplot(aes(y = train$ln_price), fill = "#3FA0FF", alpha=0.5) +
                   labs(y = "Precio de venta (log)", title = "Distribución de Precio (Log)") +
                   scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                   theme_bw() +
                   theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot_ln + coord_flip()
 
#Precio
 
 price_histogram <-   ggplot(data = train, mapping = aes(x = price))  + 
                      geom_histogram(bins = 15, position = 'identity', color="#424242", fill="#BFBFBF") +
                      labs(title = 'Distribución de los precios de venta',
                      x = 'Precio de Venta',
                      y = 'Frecuencia') + 
                      scale_x_continuous(labels = label_number()) +
                      theme_bw()
 
 price_histogram
 
   
