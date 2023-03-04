
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 3
#
#   Grupo 5:  Isabella Mendez Pedraza.
#             Manuela Ojeda Ojeda.
#             Juan Sebastian Tellez Melo.
#             Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

library(pacman) 

p_load(tidyverse, rstudioapi, rio, plotly, 
       leaflet, rgeos, tmaptools, sf, stargazer, osmdata) 

 rm(list=ls())

#Lectura de los datos

 test   <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/test.csv")
 train  <- import("https://raw.githubusercontent.com/AndresMPL/Repositorio_PS3/main/datasets/train.csv")
 
 glimpse(test) 
 glimpse(train)
 
 train <- train %>% mutate(latp=lat,longp=lon)
 train <- st_as_sf(train,coords=c('longp','latp'),crs=4326)

#Limpieza----
 
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame()  #Revisamos los NA de las variables
 
 filtro <- is.na(train$surface_total) #Trasnsformamos los NA a ceros
 train$surface_total[filtro] <- 0
 
 filtro <- is.na(train$surface_covered) #Trasnsformamos los NA a ceros
 train$surface_covered[filtro] <- 0
 
 filtro <- is.na(train$lat) | is.na(train$lon) #| is.na(train$rooms) | is.na(train$bathrooms) #| is.na(train$surface_total) | is.na(train$surface_covered) 
 train <- train[!filtro, ] #Filtramos los registros sin lat y lon
 
 limites <- getbb("Bogota Colombia") #Aseguramos que sean inmuebles en Bogotá
 filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
 filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])
 filtro <- filtro1 & filtro2
 train <- train[filtro,] 
 
 train <- train %>% mutate(area_maxima = ifelse(surface_total>surface_covered, surface_total, surface_covered)) #Seleccionamos el área máxima
 
 sapply(train, function(x) sum(is.na(x))) %>% as.data.frame() #Revisamos los NA de las variables
 
 prop.table(table(train$area_maxima == 0)) %>% as.data.frame() #Existen muchos lugares sin área
 table(train$area_maxima == 0) %>% as.data.frame()
 
 table(train$property_type) %>% as.data.frame() #Distribución entre Casas y Apartamentos
 table(train$operation_type) %>% as.data.frame() #Validamos la operación de venta

 train$property_type <- as.factor(train$property_type) #Transformamos la variable de propiedad como factor
 glimpse(train)
 
#Mapa Train----

 chapinero <- getbb(place_name = "UPZ Chapinero, Bogotá",
                    featuretype = "boundary:administrative",
                    format_out = "sf_polygon") %>% .$multipolygon
 
 leaflet() %>%
   addTiles() %>%
   addCircles(data=train, radius = 0.5, color = "#2400D9") %>%
   addPolygons(data=bogota_polig, color = "#9999FF", opacity = 0.5) %>%
   addPolygons(data=chapinero, color = "#FF5500", opacity = 0.5)

 
#Descripción y Estadísticas----

 summary(train$price) %>%
                     as.matrix() %>%
                     as.data.frame() %>%
                     mutate(V1 = scales::dollar(V1))
 
 price_boxplot <- ggplot() +
                  geom_boxplot(aes(y = train$price), fill = "#3FA0FF", alpha=0.5) +
                  labs(y = "Precio de venta") +
                  scale_x_discrete() + 
                  theme_bw() 
 

 price_histogram <-   ggplot(data = train, mapping = aes(x = price))  + 
                      geom_histogram(aes(y =after_stat(density)), bins = 9,
                      position = 'identity', color="#424242", fill="#E3E3E3") +
                      stat_function(fun = dnorm, xlim = c(min(train$price), max(train$price)), colour="#1C86EE", linewidth=1,
                      args = list(mean = mean(train$price), 
                      sd = sd(train$price))) + 
                      labs(title = 'Distribución de los precios de venta',
                      x = 'Precio de Venta',
                      y = 'Frecuencia') + 
                      theme_bw()
