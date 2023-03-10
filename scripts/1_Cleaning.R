
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
 train <- st_as_sf(train,coords=c('longp','latp'),crs = 4686)

#Limpieza de la BD ----
 
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
 
 filtro <- is.na(train$surface_total) #Transformamos los NA a ceros
 train$surface_total[filtro] <- 0
 
 filtro <- is.na(train$surface_covered) #Trasnsformamos los NA a ceros
 train$surface_covered[filtro] <- 0
 
 filtro <- is.na(train$bedrooms) #Trasnsformamos los NA a ceros #REVISAR
 train$bedrooms[filtro] <- 0
 
 filtro <- is.na(train$bathrooms) #Trasnsformamos los NA a ceros #REVISAR
 train$bathrooms[filtro] <- 0
 
 filtro <- is.na(train$rooms) #Trasnsformamos los NA a ceros #REVISAR
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
 
 
#Mapa Train ----

 chapinero <- getbb(place_name = "UPZ Chapinero, Bogotá",
                    featuretype = "boundary:administrative",
                    format_out = "sf_polygon") %>% .$multipolygon
 
 bogota_polig <- getbb(place_name = "Bogotá", 
                       featuretype = "boundary:administrative", 
                       format_out = "sf_polygon") %>% .$multipolygon
 
 #Aquí vemos los puntos sobre los cuales queremos predecir su precio de venta
 
 leaflet() %>%
           addTiles() %>%
           addPolygons(data=bogota_polig, color = "#5CACEE", opacity = 0.8, weight = 0.8) %>%
           addCircles(data=train, radius = 0.1, color = "#00008B", opacity = 0.5) %>%
           addPolygons(data=chapinero, color = "#EE3B3B", opacity = 0.5)

 
#Descripción y Estadísticas----

 summary(train$price) %>%
                     as.matrix() %>%
                     as.data.frame() %>%
                     mutate(V1 = scales::dollar(V1))
 
#Tabla de Estadísticas Descriptivas
 
 estadisticas <- train %>% select(price, surface_total, surface_covered, rooms, bedrooms, bathrooms) %>% as.data.frame()
 estadisticas <- estadisticas %>% select(-geometry) %>% as.data.frame()
 stargazer(round(estadisticas, 3), title="Tabla de Estadísticas descriptivas", type='text')
 stargazer(round(estadisticas, 3), title="Tabla de Estadísticas descriptivas", type='latex')
 
#Matriz de Correlaciones
 
 stargazer(cor(round(estadisticas, 4)), title="Tabla de Correlaciones", type='text')

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
 
#Log Precio
 
 price_histogram_ln <-  ggplot(data = train, mapping = aes(x = ln_price))  + 
                       geom_histogram(bins = 15, position = 'identity', color="#424242", fill="#BFBFBF") +
                       labs(title = 'Distribución de los precios de venta (Log)',
                       x = 'Precio de Venta (log)',
                       y = 'Frecuencia') + 
                       scale_x_continuous(labels = label_number()) +
                       theme_bw()
 
 price_histogram_ln

       