
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

#Importante para ejecutar el código----

  #Para ejecutar el código es necesario descargar todas las carpetas que se encuentran en el siguiente repositorio:
  #https://github.com/AndresMPL/Repositorio_PS3/tree/main/datasets/localidades
  #Guardar estas carpetas en una carpeta llamada "datos" y fijar el directorio de trabajo en esta última carpeta contenedora

  library(pacman) 
  
  setwd("C:/Users/User/Documents/Big_Data/BD_Taller 3") 
  #Para iniciar se debe decargar todas las carpetas del siguiente sitio:
  #https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/
  #Guardarlas en una carpeta "datos" y fijar el directorio donde se encuentre esta carpeta
  
  #Cargamos las siguientes librerías verificando que no existen alertas por funciones duplicadas entre ellas
  
  p_load(tidyverse, rio, sf, dplyr, osmdata, leaflet, scales, stargazer,rstudioapi, 
         rio, plotly, modeldata, rgeos, tmaptools, IRdisplay, spatialsample, geojsonio)
  
   rm(list=ls())

 
#Lectura de los datos de Train ----

 test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/test.csv")
 train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/train.csv")
 
 glimpse(test) 
 glimpse(train) #Las dos BD de Train y Test tienen las mismas variables
 
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
 
#Tabla de Estadísticas Descriptivas - Antes de limpieza
 
 summary(train$price) %>%
         as.matrix() %>%
         as.data.frame() %>%
         mutate(V1 = scales::dollar(V1))
 
 estadisticas <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
 estadisticas <- estadisticas %>% select(-geometry) %>% as.data.frame()
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='text')
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='latex')
 
 
#Distribuciones de los precios
 
 price_boxplot <- ggplot() +
                  geom_boxplot(aes(y = train$price), fill = "#BFBFBF", alpha=0.5) +
                  labs(y = "Precio de venta", title = "Distribución de Precio") +
                  scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                  theme_bw() +
                  theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot + coord_flip()

#Log Precio
 
 price_boxplot_ln <-  ggplot() +
                     geom_boxplot(aes(y = train$ln_price), fill = "#BFBFBF", alpha=0.5) +
                     labs(y = "Precio de venta (log)", title = "Distribución de Precio (Log)") +
                     scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                     theme_bw() +
                     theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot_ln  + coord_flip()
 
#Precio
 
 price_histogram <-   ggplot(data = train, mapping = aes(x = price))  + 
                      geom_histogram(bins = 15, position = 'identity', color="#424242", fill="#BFBFBF") +
                      labs(title = 'Distribución de los precios de venta',
                      x = 'Precio de Venta',
                      y = 'Frecuencia') + 
                      scale_x_continuous(labels = label_number()) +
                      theme_bw()
 
 price_histogram
 
#Log Precio
 
 price_histogram_ln <-  ggplot(data = train, mapping = aes(x = ln_price))  + 
                       geom_histogram(bins = 15, position = 'identity', color="#424242", fill="#BFBFBF") +
                       labs(title = 'Distribución de los precios de venta (Log)',
                       x = 'Precio de Venta (log)',
                       y = 'Frecuencia') + 
                       scale_x_continuous(labels = label_number()) +
                       theme_bw()
 
 price_histogram_ln

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
 
 filtro <- is.na(train$surface_total) #Transformamos los NA a ceros
 sum(filtro)
 train$surface_total[filtro] <- 0
 
 filtro <- is.na(train$surface_covered) #Trasnsformamos los NA a ceros
 sum(filtro)
 train$surface_covered[filtro] <- 0
 
 filtro <- is.na(train$bedrooms) #Trasnsformamos los NA a ceros #REVISAR
 sum(filtro)
 train$bedrooms[filtro] <- 0
 
 filtro <- is.na(train$bathrooms) #Trasnsformamos los NA a ceros #REVISAR
 sum(filtro)
 train$bathrooms[filtro] <- 0
 
 filtro <- is.na(train$rooms) #Trasnsformamos los NA a ceros #REVISAR
 sum(filtro)
 train$rooms[filtro] <- 0

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
 
 estadisticas <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
 estadisticas <- estadisticas %>% select(-geometry) %>% as.data.frame()
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='text')
 stargazer(round(estadisticas), digits = 2, title="Tabla de Estadísticas descriptivas", type='latex')
 
#Matriz de Correlaciones
 
 stargazer(cor(round(estadisticas, 4)), title="Tabla de Correlaciones", type='text')
<<<<<<< HEAD
 stargazer(cor(round(estadisticas, 4)), title="Tabla de Correlaciones", type='latex')
=======

#Distribuciones de los precios
 
 price_boxplot <- ggplot() +
                  geom_boxplot(aes(y = train$price), fill = "#3FA0FF", alpha=0.5) +
                  labs(y = "Precio de venta", title = "Distribución de Precio") +
                  scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                  theme_bw() +
                  theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot

#Log Precio
 
 price_boxplot_ln <-  ggplot() +
                   geom_boxplot(aes(y = train$ln_price), fill = "#3FA0FF", alpha=0.5) +
                   labs(y = "Precio de venta (log)", title = "Distribución de Precio (Log)") +
                   scale_x_discrete() + scale_y_continuous(labels = label_dollar(prefix = "$")) + 
                   theme_bw() +
                   theme(axis.title = element_text(size = 10, color = "black", face = "bold"))
 
 price_boxplot_ln
 
#Precio
 
 price_histogram <-   ggplot(data = train, mapping = aes(x = price))  + 
                      geom_histogram(bins = 15, position = 'identity', color="#424242", fill="#BFBFBF") +
                      labs(title = 'Distribución de los precios de venta',
                      x = 'Precio de Venta',
                      y = 'Frecuencia') + 
                      scale_x_continuous(labels = label_number()) +
                      theme_bw()
 
 price_histogram
 

#Visualizar cuales de estos inmuebles son casas y cuales apartamentos
 
 color <- rep(NA,nrow(train))
 color[train$property_type == "Casa"] <- "#ffff3f"
   color[train$property_type == "Apartamento"] <- "#007f5f"
     
   
   leaflet() %>%
     addTiles() %>%
     addCircles(lng = train$lon,
                lat = train$lat,
                col = color)
   
   
   ###############################################################################
  
   #Vamos a sacar los predictores a partir de la descripción de las propiedades
   p_load(tm, tidytext) 
   
   descripcion <- train$description
   titulo <- train$title

   #Ponemos todo en minúscula, quitamos espacios en blanco sobrante y signos de puntuación
   
   descripcion <- removePunctuation(descripcion)
   descripcion <- tolower(descripcion)
   descripcion <- stripWhitespace(descripcion)
   
   descripcion2 <- tibble(n_descripcion = 1:38644, text_descripcion = descripcion)
   descripcion2
   
   # Generar bigramas
   
   bigrams <- as.data.frame(descripcion) %>%
     unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)
   
   
   # Eliminar los bigramas que no contengan información de mts
   
   descripcion_keep <- c("mts2", "m", "mts", "metros", "m2", "mt2")
   bigrams_keep <- data.frame(word2 = descripcion_keep)
   
   bigrams2 <- bigrams %>%
     separate(bigram, c("word1", "word2"), sep = " ") %>%
     semi_join(bigrams_keep, by = "word2") %>%
     unite(bigram, word1, word2, sep = " ")
   
   dim(bigrams)
   dim(bigrams2)
   
   
   
<<<<<<< HEAD
   
=======
>>>>>>> 5daeb334b6479439e8dd1f579b86e77aab9cd6b4
>>>>>>> e732ed90e5584e4d7ed7d0324c2d132e117ca270
