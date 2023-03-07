
#------------------------------------------------------------------------------#
#
#                            3 - DISTANCES
#
#------------------------------------------------------------------------------#

#Distancias----


train_sf <- st_as_sf(train, coords = c("lon", "lat"))
train_sf <- st_transform(train_sf,4686)
class(train)
class(train_sf)

##Parques----
  
  centroides_parques_d <- st_as_sf(centroides_parques, coords = c("x", "y"))
  #st_crs(centroides_parques_d) <- 4686
  dist_matrix <- st_distance(x = train_sf, y = centroides_parques_d)
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_parque <- dist_min
  train_sf$distancia_parque <- dist_min
  
  imagen_1 <- ggplot(train, aes(x = distancia_parque)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un parque metros", y = "Cantidad",
              title = "Distribución de la distancia a los parquees") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_parque, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un parque", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un parque y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
  
  
##Museos----
  
  centroides_museos_d <- st_as_sf(centroides_museos, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_museos_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_museo <- dist_min
  train_sf$distancia_museo <- dist_min

  imagen_1 <- ggplot(train, aes(x = distancia_museo)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un museo metros", y = "Cantidad",
              title = "Distribución de la distancia a los museoes") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_museo, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un museo", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un museo y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
  
  
##IPS----

  centroides_ips_d <- st_as_sf(centroides_ips, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_ips_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_ips <- dist_min
  train_sf$distancia_ips <- dist_min
  
  imagen_1 <- ggplot(train, aes(x = distancia_ips)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud metros", y = "Cantidad",
              title = "Distribución de la distancia a los saludes") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_ips, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un salud y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
 
  
##ESE----
  
  centroides_ese_d <- st_as_sf(centroides_ese, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_ese_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_ese <- dist_min
  train_sf$distancia_ese <- dist_min
  
  imagen_1 <- ggplot(train, aes(x = distancia_ese)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud metros", y = "Cantidad",
              title = "Distribución de la distancia a los saludes") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_ese, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un salud y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
 
  
##Colegios----
  
  centroides_colegios_d <- st_as_sf(centroides_colegios, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_colegios_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_colegios <- dist_min
  train_sf$distancia_colegios <- dist_min
  
  
  imagen_1 <- ggplot(train, aes(x = distancia_colegios)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud metros", y = "Cantidad",
              title = "Distribución de la distancia a los saludes") +
              theme_bw()
            
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_colegios, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un salud", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un salud y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2

  
##CAI----

  centroides_cai_d <- st_as_sf(centroides_cai, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_cai_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_cai <- dist_min
  train_sf$distancia_cai <- dist_min
  
  imagen_1 <- ggplot(train, aes(x = distancia_cai)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un seguridad metros", y = "Cantidad",
              title = "Distribución de la distancia a los seguridades") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_cai, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un seguridad", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un seguridad y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2

  
##Biblo Estaciones----
  
  centroides_best_d <- st_as_sf(centroides_best, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_best_d)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_best <- dist_min
  train_sf$distancia_best <- dist_min
  
  
  imagen_1 <- ggplot(train, aes(x = distancia_best)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un biblio metros", y = "Cantidad",
              title = "Distribución de la distancia a los biblioes") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_best, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un biblio", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un biblio y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
  
  
##Centros Financieros----
  
  centroides_centrof_f <- st_as_sf(centroides_centrof, coords = c("x", "y"))
  
  dist_matrix <- st_distance(x = train_sf, y = centroides_centrof_f)
  
  dist_min <- apply(dist_matrix, 1, min)
  
  train$distancia_centrof <- dist_min
  train_sf$distancia_centrof <- dist_min
  
  
  imagen_1 <- ggplot(train, aes(x = distancia_centrof)) +
              geom_histogram(bins = 50, fill = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un biblio metros", y = "Cantidad",
              title = "Distribución de la distancia a los biblioes") +
              theme_bw()
  
  imagen_1
  
  imagen_2 <- ggplot(train, aes(x = distancia_centrof, y = price)) +
              geom_point(col = "#6C7B8B", alpha = 0.4) +
              labs(x = "Distancia mínima a un biblio", 
              y = "Precio Inmueble",
              title = "Relación entre la proximidad a un biblio y el precio del inmueble") +
              scale_x_continuous() +
              scale_y_continuous(labels = scales::dollar) +
              theme_bw()
  
  imagen_2
  